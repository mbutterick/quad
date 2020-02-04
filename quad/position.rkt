#lang debug br
(require "quad.rkt" "param.rkt" fontland)
(provide (all-defined-out))

(define pt-x first)
(define pt-y second)
(define (pt x y) (list x y))
(define (pt+ . pts) (apply map + pts))
(define (pt- . pts) (apply map - pts))

(define (sum-base qs which)
  (for/sum ([q (in-list qs)])
           (which (size q))))
(define (sum-y qs) (sum-base qs pt-y))
(define (sum-x qs) (sum-base qs pt-x))

(define valid-anchors '(nw n ne w c e sw s se bi bo))

(define (coerce-int x) (if (integer? x) (inexact->exact x) x))

(define font-cache (make-hash))
(define (get-font font-name)
  (hash-ref! font-cache font-name (λ () (open-font font-name))))

(define font-path-key 'font-path)

(define ascender-cache (make-hash))
(define (ascender q)
  (define font-key-val (quad-ref q font-path-key))
  (unless font-key-val
    (error 'ascender-no-font-key))
  (hash-ref! ascender-cache font-key-val (λ () (font-ascent (get-font font-key-val)))))

(define units-cache (make-hash))
(define (units-per-em q)
  (define font-key-val (quad-ref q font-path-key))
  (unless font-key-val
    (error 'units-per-em-no-font-key))
  (hash-ref! units-cache font-key-val (λ () (font-units-per-em (get-font font-key-val)))))

(define (fontsize q)
  (define val (quad-ref q 'font-size current-default-font-size))
  ((if (number? val) values string->number) val))

(define (vertical-baseline-offset q [fallback-val 0])
  (cond
    [(quad-ref q font-path-key)
     (* (/ (ascender q) (units-per-em q) 1.0) (fontsize q))]
    [else fallback-val]))

(define (anchor->local-point q anchor)
  ;; calculate the location of the anchor on the bounding box relative to '(0 0) (aka "locally")
  (match-define (list x-fac y-fac)
    (case anchor
      [(nw) '(0 0  )] [(n) '(0.5 0  )] [(ne) '(1 0  )]
      [( w) '(0 0.5)] [(c) '(0.5 0.5)] [( e) '(1 0.5)]
      [(sw) '(0 1  )] [(s) '(0.5 1  )] [(se) '(1 1  )]
      [(baseline-in bi) '(0 0  )]                  [(baseline-out bo) '(1 0  )]
      [else (raise-argument-error 'anchor->local-point (format "anchor value in ~v" valid-anchors) anchor)]))
  (match-define (list x y) (size q))
  (pt (coerce-int (* x x-fac))
      (coerce-int (+ (* y y-fac) (match anchor
                                   [(or 'bi 'bo 'baseline-in 'baseline-out)
                                    ;; if no font available, match baseline to south edge by passing y as fallback value
                                    (vertical-baseline-offset q y)]
                                   [_ 0])))))

(define (to-point q)
  ;; calculate absolute location
  ;; based on current origin and point type.
  ;; don't include offset, so location is on bounding box
  (anchor->global-point q (quad-to q)))

(define (from-point q)
  ;; calculate absolute location
  ;; based on current origin and point type.
  ;; don't include offset, so location is on bounding box
  (anchor->global-point q (quad-from q)))

(define (anchor->global-point q anchor)
  ;; don't include shift here: it should be baked into origin calculation
  (pt+ (anchor->local-point q anchor) (quad-origin q)))

(define (position q-or-qs [ref-src #f])
  (define-values (arg post-proc)
    (match q-or-qs
      [(? quad? q) (values q values)]
      [(list (? quad? qs) ...) (values (make-quad #:elems qs) quad-elems)]))
  (post-proc (position-one arg ref-src)))

(define (position-one q ref-src)
  ;; recursively calculates coordinates for quad & subquads
  ;; need to position before recurring, so subquads have accurate reference point
  (define positioned-q
    (quad-copy q
               [origin (let* ([ref-pt (cond
                                        [(quad? ref-src)
                                         (anchor->global-point ref-src (or (quad-from-parent q) (quad-from q)))]
                                        [ref-src] ; for passing explicit points in testing
                                        [else (pt 0 0)])]
                              [this-origin (pt- ref-pt (to-point q))]
                              [shifted-origin (pt+ this-origin (quad-shift q))])
                         shifted-origin)]
               ;; set shift to zero because it's baked into new origin value
               [shift (pt 0 0)]))
  (define positioned-elems
    ;; for purposes of positioning the elements, we want to also bake in the `shift-elements` value
    ;; but we don't want this origin to be permanent on the parent.
    ;; akin to `push` a graphics state and then `pop` afterwards.
    (let ([parent-q (quad-copy positioned-q
                               [origin (pt+ (quad-origin positioned-q) (quad-shift-elems positioned-q))]
                               [shift-elems (pt 0 0)])])
      ;; can't use for/list here because previous quads provide context for later ones
      (let loop ([prev-elems null] [elems (quad-elems parent-q)])
        (match elems
          [(? null?) (reverse prev-elems)]
          [(cons (? quad? this-q) rest)
           (define ref-q (if (or (quad-from-parent this-q) (null? prev-elems))
                             parent-q
                             (car prev-elems)))
           (loop (cons (position-one this-q ref-q) prev-elems) rest)]
          [(cons x rest) (loop (cons x prev-elems) rest)]))))
  (quad-update! positioned-q [elems positioned-elems]))

(define (distance q)
  (match (pt- (from-point q) (to-point q))
    [(list-no-order 0 val) val]
    [(list ∆x ∆y) (sqrt (+ (expt ∆x 2) (expt ∆y 2)))]))

(define (flatten-quad q)
  (cons (quad-copy q [elems null])
        (apply append (map flatten-quad (quad-elems q)))))

(define (bounding-box . qs-in)
  ;; size of box that holds q and all subqs, based on reported origin and size
  ;; does not know anything about drawing (which may go outside the box)
  (define qs (flatten-quad (make-quad #:elems qs-in)))
  (define (outer-pt q) (pt+ (quad-origin q) (quad-size q)))
  (define max-outer-pt (apply map max (cons '(0 0) (map outer-pt qs))))
  (define min-origin (apply map min (cons '(0 0) (map quad-origin qs))))
  (append min-origin max-outer-pt))

(define (attach-to from-q from-pt to-q to-pt)
  (struct-copy quad from-q
               [elems (cons (struct-copy quad to-q
                                         [from-parent from-pt]
                                         [to to-pt])
                            (quad-elems from-q))]))

(module+ test
  (require rackunit)

  (test-case
   "bounding boxes"
   (define q10 (make-quad #:size '(10 10)))
   (define q20 (make-quad #:size '(20 20)))
   (check-equal? (bounding-box q10) '(0 0 10 10))
   (check-equal? (bounding-box (position (make-quad #:elems (list q10 q10)))) '(0 0 20 10))
   (check-equal? (bounding-box q20) '(0 0 20 20))
   (check-equal? (bounding-box (position (make-quad #:elems (list q10 q20)))) '(0 0 30 20))
   (check-equal? (bounding-box (position (make-quad #:elems (list q10 q20 q20)))) '(0 0 50 20)))

  (test-case
   "origins"
   (define size (pt 10 10))
   (define orig (pt 5 5))
   (check-equal? (quad-origin (position (q #:to 'nw #:size size) orig)) (pt 5 5))
   (check-equal? (quad-origin (position (q #:to 'n #:size size) orig)) (pt 0 5))
   (check-equal? (quad-origin (position (q #:to 'ne #:size size) orig)) (pt -5 5))
   (check-equal? (quad-origin (position (q #:to 'e #:size size) orig)) (pt -5 0))
   (check-equal? (quad-origin (position (q #:to 'se #:size size) orig)) (pt -5 -5))
   (check-equal? (quad-origin (position (q #:to 's #:size size) orig)) (pt 0 -5))
   (check-equal? (quad-origin (position (q #:to 'sw #:size size) orig)) (pt 5 -5))
   (check-equal? (quad-origin (position (q #:to 'w #:size size) orig)) (pt 5 0)))

  
  (test-case
   "origins with shifts"
   (define size (pt 10 10))
   (define orig (pt 5 5))
   (define shift (pt 3 3))
   (check-equal? (quad-origin (position (q #:to 'nw #:size size #:shift shift) orig)) (pt+ (pt 5 5) shift))
   (check-equal? (quad-origin (position (q #:to 'n #:size size #:shift shift) orig)) (pt+ (pt 0 5) shift))
   (check-equal? (quad-origin (position (q #:to 'ne #:size size #:shift shift) orig)) (pt+ (pt -5 5) shift))
   (check-equal? (quad-origin (position (q #:to 'e #:size size #:shift shift) orig)) (pt+ (pt -5 0) shift))
   (check-equal? (quad-origin (position (q #:to 'se #:size size #:shift shift) orig)) (pt+ (pt -5 -5) shift))
   (check-equal? (quad-origin (position (q #:to 's #:size size #:shift shift) orig)) (pt+ (pt 0 -5) shift))
   (check-equal? (quad-origin (position (q #:to 'sw #:size size #:shift shift) orig)) (pt+ (pt 5 -5) shift))
   (check-equal? (quad-origin (position (q #:to 'w #:size size #:shift shift) orig)) (pt+ (pt 5 0) shift)))

  (test-case
   "in points"
   (define size '(10 10))
   (define pos '(5 5))
   (check-equal? (to-point (q #:to 'nw #:size size #:origin pos)) (pt 5 5))
   (check-equal? (to-point (q #:to 'n #:size size #:origin pos)) (pt 10 5))
   (check-equal? (to-point (q #:to 'ne #:size size #:origin pos)) (pt 15 5))
   (check-equal? (to-point (q #:to 'w #:size size #:origin pos)) (pt 5 10))
   (check-equal? (to-point (q #:to 'c #:size size #:origin pos)) (pt 10 10))
   (check-equal? (to-point (q #:to 'e #:size size #:origin pos)) (pt 15 10))
   (check-equal? (to-point (q #:to 'sw #:size size #:origin pos)) (pt 5 15))
   (check-equal? (to-point (q #:to 's #:size size #:origin pos)) (pt 10 15))
   (check-equal? (to-point (q #:to 'se #:size size #:origin pos)) (pt 15 15)))


  (test-case
   "out points"
   (define size (pt 10 10))
   (define pos (pt 5 5))
   (check-equal? (from-point (q #:from 'nw #:size size #:origin pos)) (pt 5 5))
   (check-equal? (from-point (q #:from 'n #:size size #:origin pos)) (pt 10 5))
   (check-equal? (from-point (q #:from 'ne #:size size #:origin pos)) (pt 15 5))
   (check-equal? (from-point (q #:from 'w #:size size #:origin pos)) (pt 5 10))
   (check-equal? (from-point (q #:from 'c #:size size #:origin pos)) (pt 10 10))
   (check-equal? (from-point (q #:from 'e #:size size #:origin pos)) (pt 15 10))
   (check-equal? (from-point (q #:from 'sw #:size size #:origin pos)) (pt 5 15))
   (check-equal? (from-point (q #:from 's #:size size #:origin pos)) (pt 10 15))
   (check-equal? (from-point (q #:from 'se #:size size #:origin pos)) (pt 15 15)))

  )

#;(module+ test
    (require racket/runtime-path fontland/font)
    (define-runtime-path fira "fira.ttf")

    (define q1 (q (list 'in 'bi 'out 'bo 'size '(10 10) font-path-key fira 'fontsize 12)))
    (define q2 (q (list 'in 'bi 'out 'bo 'size '(10 10) font-path-key fira 'fontsize 24)))
    (define q3 (q (list 'in 'bi 'out 'bo 'size '(10 10) font-path-key fira 'fontsize 6)))
    #;(position (q #f q1 q2 q3)))


#;(module+ test
    (require rackunit)
    (define q (q (list 'in 'bi 'out 'bo 'size '(10 10) font-path-key fira 'fontsize 12)))
    (check-equal? (ascender q) 935)
    (check-equal? (units-per-em q) 1000)
    (define ascender-scaled (* (/ (ascender q) (units-per-em q)) (quad-ref q 'fontsize) 1.0))
    (check-equal? (in-point q) (list 0 ascender-scaled))
    (check-equal? (out-point q) (list 10 ascender-scaled)))