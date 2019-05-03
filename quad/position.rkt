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

(define (inner-point q)
  ;; calculate absolute location of inner-point
  ;; based on current origin and point type.
  ;; include offset, because it's intended to adjust inner 
  (pt+ (quad-origin q) (anchor->local-point q (or (quad-inner q) (quad-in q))) (quad-offset q)))

(define (in-point q)
  ;; calculate absolute location of in-point
  ;; based on current origin and point type.
  ;; don't include offset, so location is on bounding box
  (pt+ (quad-origin q) (anchor->local-point q (quad-in q))))

(define (out-point q)
  ;; calculate absolute location of out-point
  ;; based on current origin and point type.
  ;; don't include offset, so location is on bounding box
  (pt+ (quad-origin q) (anchor->local-point q (quad-out q))))

(define (position q [previous-end-pt (pt 0 0)])
  ;; recursively calculates coordinates for quad & subquads
  ;; based on starting origin point
  (define new-origin (pt- previous-end-pt (in-point q)))
  (let ([q (struct-copy quad q [origin new-origin])])
    (let loop ([pt (inner-point q)] [acc null] [elems (quad-elems q)])
      (match elems
        [(== empty) (struct-copy quad q [elems (reverse acc)])]
        [(cons (? quad? q) rest)
         (define new-q (position q pt))
         (loop (out-point new-q) (cons new-q acc) rest)]
        [(cons x rest) (loop pt (cons x acc) rest)]))))

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
   "in points"
   (define size '(10 10))
   (define origin '(5 5))
   (check-equal? (in-point (q #:in 'nw #:size size #:origin origin)) (pt 5 5))
   (check-equal? (in-point (q #:in 'n #:size size #:origin origin)) (pt 10 5))
   (check-equal? (in-point (q #:in 'ne #:size size #:origin origin)) (pt 15 5))
   (check-equal? (in-point (q #:in 'w #:size size #:origin origin)) (pt 5 10))
   (check-equal? (in-point (q #:in 'c #:size size #:origin origin)) (pt 10 10))
   (check-equal? (in-point (q #:in 'e #:size size #:origin origin)) (pt 15 10))
   (check-equal? (in-point (q #:in 'sw #:size size #:origin origin)) (pt 5 15))
   (check-equal? (in-point (q #:in 's #:size size #:origin origin)) (pt 10 15))
   (check-equal? (in-point (q #:in 'se #:size size #:origin origin)) (pt 15 15)))

  (test-case
   "out points"
   (define size (pt 10 10))
   (define origin (pt 5 5))
   (check-equal? (out-point (q #:out 'nw #:size size #:origin origin)) (pt 5 5))
   (check-equal? (out-point (q #:out 'n #:size size #:origin origin)) (pt 10 5))
   (check-equal? (out-point (q #:out 'ne #:size size #:origin origin)) (pt 15 5))
   (check-equal? (out-point (q #:out 'w #:size size #:origin origin)) (pt 5 10))
   (check-equal? (out-point (q #:out 'c #:size size #:origin origin)) (pt 10 10))
   (check-equal? (out-point (q #:out 'e #:size size #:origin origin)) (pt 15 10))
   (check-equal? (out-point (q #:out 'sw #:size size #:origin origin)) (pt 5 15))
   (check-equal? (out-point (q #:out 's #:size size #:origin origin)) (pt 10 15))
   (check-equal? (out-point (q #:out 'se #:size size #:origin origin)) (pt 15 15)))

  (test-case
   "inner points"
   (define size '(20 20))
   (define orig '(10 10))
   (check-equal? (inner-point (position (q #:size size #:inner 'nw) orig)) (pt 10 10))
   (check-equal? (inner-point (position (q #:size size #:inner 'n) orig)) (pt 20 10))
   (check-equal? (inner-point (position (q #:size size #:inner 'ne) orig)) (pt 30 10))
   (check-equal? (inner-point (position (q #:size size #:inner 'e) orig)) (pt 30 20))
   (check-equal? (inner-point (position (q #:size size #:inner 'se) orig)) (pt 30 30))
   (check-equal? (inner-point (position (q #:size size #:inner 's) orig)) (pt 20 30))
   (check-equal? (inner-point (position (q #:size size #:inner 'sw) orig)) (pt 10 30))
   (check-equal? (inner-point (position (q #:size size #:inner 'w) orig)) (pt 10 20)))

  (test-case
   "inner points with offsets"
   (define size (pt 10 10))
   (define orig (pt 0 0))
   (define off (pt (random 100) (random 100)))
   (check-equal? (inner-point (position (q #:size size #:inner 'nw #:offset off) orig)) (pt+ '(0 0) off))
   (check-equal? (inner-point (position (q #:size size #:inner 'n #:offset off) orig)) (pt+ '(5 0) off))
   (check-equal? (inner-point (position (q #:size size #:inner 'ne #:offset off) orig)) (pt+ '(10 0) off))
   (check-equal? (inner-point (position (q #:size size #:inner 'e #:offset off) orig)) (pt+ '(10 5) off))
   (check-equal? (inner-point (position (q #:size size #:inner 'se #:offset off) orig)) (pt+ '(10 10) off))
   (check-equal? (inner-point (position (q #:size size #:inner 's #:offset off) orig)) (pt+ '(5 10) off))
   (check-equal? (inner-point (position (q #:size size #:inner 'sw #:offset off) orig)) (pt+ '(0 10) off))
   (check-equal? (inner-point (position (q #:size size #:inner 'w #:offset off) orig)) (pt+ '(0 5) off))))

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