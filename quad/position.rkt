#lang debug br
(require "quad.rkt" "param.rkt" fontland)
(provide (all-defined-out))

(define pt-x first)
(define pt-y second)
(define (pt x y) (list x y))
(define (pt+ . pts) (apply map + pts))
(define (pt- . pts) (apply map - pts))

(define valid-anchors '(nw n ne w c e sw s se bi bo))

(define (coerce-int x) (if (integer? x) (inexact->exact x) x))

(define font-cache (make-hash))
(define (get-font font-name)
  (hash-ref! font-cache font-name (λ () (open-font font-name))))

(define font-path-key 'font-path)

(define ascender-cache (make-hash))
(define (ascender q)
  (define font-key-val (quad-ref q font-path-key #false))
  (unless font-key-val
    (error 'ascender-no-font-key))
  (hash-ref! ascender-cache font-key-val (λ () (font-ascent (get-font font-key-val)))))

(define units-cache (make-hash))
(define (units-per-em q)
  (define font-key-val (quad-ref q font-path-key #false))
  (unless font-key-val
    (error 'units-per-em-no-font-key))
  (hash-ref! units-cache font-key-val (λ () (font-units-per-em (get-font font-key-val)))))

(define (fontsize q)
  (define val (quad-ref q 'font-size current-default-font-size))
  ((if (number? val) values string->number) val))

(define (vertical-baseline-offset q)
  (cond
    [(quad-ref q font-path-key #false)
     (* (/ (ascender q) (units-per-em q) 1.0) (fontsize q))]
    [else 0]))

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
                                   [(or 'bi 'bo 'baseline-in 'baseline-out) (vertical-baseline-offset q)]
                                   [_ 0])))))

(define (in-point q)
  ;; calculate absolute location of in-point
  ;; based on current origin and point type.
  ;; don't include offset, so location is on bounding box
  (anchor->global-point q (quad-in q)))

(define (out-point q)
  ;; calculate absolute location of out-point
  ;; based on current origin and point type.
  ;; don't include offset, so location is on bounding box
  (anchor->global-point q (quad-out q)))

(define (anchor->global-point q anchor)
  ;; don't include shift here: it should be baked into origin calculation
  (pt+ (anchor->local-point q anchor) (quad-origin q)))

(define (position q [ref-src #f])
  ;; recursively calculates coordinates for quad & subquads
  (define ref-pt (cond
                   [(quad? ref-src) (anchor->global-point ref-src (quad-out q))]
                   [ref-src] ; for passing explicit points in testing
                   [else (pt 0 0)]))
  (define this-origin (pt- ref-pt (in-point q)))
  (define shifted-origin (pt+ this-origin (quad-shift q)))
  ;; need to position before recurring, so subquads have accurate reference point
  (define positioned-q (struct-copy quad q
                                    [origin shifted-origin]
                                    ;; set shift to zero because it's baked into new origin value
                                    [shift (pt 0 0)]))
  (let ([parent-q positioned-q])
    (struct-copy quad parent-q
                 [elems
                  ;; can't use for/list here because previous quads provide context for later ones
                  (let loop ([prev-elems null] [elems (quad-elems parent-q)])
                          (match elems
                            [(? null?) (reverse prev-elems)]
                            [(cons (? quad? this-q) rest)
                             (define ref-q (if (or (quad-on-parent this-q) (null? prev-elems))
                                               parent-q
                                               (car prev-elems)))
                             (loop (cons (position this-q ref-q) prev-elems) rest)]
                            [(cons x rest) (loop (cons x prev-elems) rest)]))])))

(define (distance q)
  (match (pt- (out-point q) (in-point q))
    [(list-no-order 0 val) val]
    [(list ∆x ∆y) (sqrt (+ (expt ∆x 2) (expt ∆y 2)))]))

(module+ test
  (require rackunit)
  (test-case
   "origins"
   (define size (pt 10 10))
   (define orig (pt 5 5))
   (check-equal? (quad-origin (position (q #:in 'nw #:size size) orig)) (pt 5 5))
   (check-equal? (quad-origin (position (q #:in 'n #:size size) orig)) (pt 0 5))
   (check-equal? (quad-origin (position (q #:in 'ne #:size size) orig)) (pt -5 5))
   (check-equal? (quad-origin (position (q #:in 'e #:size size) orig)) (pt -5 0))
   (check-equal? (quad-origin (position (q #:in 'se #:size size) orig)) (pt -5 -5))
   (check-equal? (quad-origin (position (q #:in 's #:size size) orig)) (pt 0 -5))
   (check-equal? (quad-origin (position (q #:in 'sw #:size size) orig)) (pt 5 -5))
   (check-equal? (quad-origin (position (q #:in 'w #:size size) orig)) (pt 5 0)))

  
  (test-case
   "origins with shifts"
   (define size (pt 10 10))
   (define orig (pt 5 5))
   (define shift (pt 3 3))
   (check-equal? (quad-origin (position (q #:in 'nw #:size size #:shift shift) orig)) (pt+ (pt 5 5) shift))
   (check-equal? (quad-origin (position (q #:in 'n #:size size #:shift shift) orig)) (pt+ (pt 0 5) shift))
   (check-equal? (quad-origin (position (q #:in 'ne #:size size #:shift shift) orig)) (pt+ (pt -5 5) shift))
   (check-equal? (quad-origin (position (q #:in 'e #:size size #:shift shift) orig)) (pt+ (pt -5 0) shift))
   (check-equal? (quad-origin (position (q #:in 'se #:size size #:shift shift) orig)) (pt+ (pt -5 -5) shift))
   (check-equal? (quad-origin (position (q #:in 's #:size size #:shift shift) orig)) (pt+ (pt 0 -5) shift))
   (check-equal? (quad-origin (position (q #:in 'sw #:size size #:shift shift) orig)) (pt+ (pt 5 -5) shift))
   (check-equal? (quad-origin (position (q #:in 'w #:size size #:shift shift) orig)) (pt+ (pt 5 0) shift)))

  (test-case
   "in points"
   (define size '(10 10))
   (define pos '(5 5))
   (check-equal? (in-point (q #:in 'nw #:size size #:origin pos)) (pt 5 5))
   (check-equal? (in-point (q #:in 'n #:size size #:origin pos)) (pt 10 5))
   (check-equal? (in-point (q #:in 'ne #:size size #:origin pos)) (pt 15 5))
   (check-equal? (in-point (q #:in 'w #:size size #:origin pos)) (pt 5 10))
   (check-equal? (in-point (q #:in 'c #:size size #:origin pos)) (pt 10 10))
   (check-equal? (in-point (q #:in 'e #:size size #:origin pos)) (pt 15 10))
   (check-equal? (in-point (q #:in 'sw #:size size #:origin pos)) (pt 5 15))
   (check-equal? (in-point (q #:in 's #:size size #:origin pos)) (pt 10 15))
   (check-equal? (in-point (q #:in 'se #:size size #:origin pos)) (pt 15 15)))


  (test-case
   "out points"
   (define size (pt 10 10))
   (define pos (pt 5 5))
   (check-equal? (out-point (q #:out 'nw #:size size #:origin pos)) (pt 5 5))
   (check-equal? (out-point (q #:out 'n #:size size #:origin pos)) (pt 10 5))
   (check-equal? (out-point (q #:out 'ne #:size size #:origin pos)) (pt 15 5))
   (check-equal? (out-point (q #:out 'w #:size size #:origin pos)) (pt 5 10))
   (check-equal? (out-point (q #:out 'c #:size size #:origin pos)) (pt 10 10))
   (check-equal? (out-point (q #:out 'e #:size size #:origin pos)) (pt 15 10))
   (check-equal? (out-point (q #:out 'sw #:size size #:origin pos)) (pt 5 15))
   (check-equal? (out-point (q #:out 's #:size size #:origin pos)) (pt 10 15))
   (check-equal? (out-point (q #:out 'se #:size size #:origin pos)) (pt 15 15)))

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