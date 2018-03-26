#lang debug br
(require racket/contract "quad.rkt" "generic.rkt")
(provide (all-defined-out))

(define pt-x first)
(define pt-y second)
(define (pt x y) (list x y))
(define (pt+ . pts) (apply map + pts))
(define (pt- . pts) (apply map - pts))
(define point? (list/c number? number?))


(define valid-anchors '(nw n ne w c e sw s se bi bo))

(define (valid-anchor? anchor)
  (and (memq anchor valid-anchors) #t))

(define (random-anchor)
  (list-ref valid-anchors (random (length valid-anchors))))

(define (coerce-int x) (if (integer? x) (inexact->exact x) x))


(define (get-font p)
  (define fonts (make-hash))
  (hash-ref! fonts p (λ () (openSync p))))

(define ascender-cache (make-hash))
(define/contract (ascender q)
  (quad? . -> . real?)
  (define p (hash-ref (attrs q) 'font "Courier"))
  (unless p
    (error 'ascender-no-font-key))
  (hash-ref! ascender-cache p (λ () (ascent (get-font p)))))

(define units-cache (make-hash))
(define/contract (units-per-em q)
  (quad? . -> . real?)
  (define p (hash-ref (attrs q) 'font "Courier"))
  (unless p
    (error 'units-per-em-no-font-key))
  (hash-ref! units-cache p (λ () (unitsPerEm (get-font p)))))

(define (fontsize q)
  ;; this needs to not default to 0
  ;; needs parameter with default font size
  (define val (hash-ref (attrs q) 'fontsize (λ () (error 'no-font-size))))
  ((if (number? val) values string->number) val))

(define (vertical-baseline-offset q)
  (* (/ (ascender q) (units-per-em q) 1.0) (fontsize q)))

(define/contract (anchor->local-point q anchor)
  (quad? symbol? . -> . point?)
  ;; calculate the location of the anchor on the bounding box relative to '(0 0) (aka "locally")
  (unless (valid-anchor? anchor)
    (raise-argument-error 'relative-anchor-pt "valid anchor" anchor))
  (match-define (list x-fac y-fac)
    (case anchor
      [(nw) '(0 0  )] [(n) '(0.5 0  )] [(ne) '(1 0  )]
      [( w) '(0 0.5)] [(c) '(0.5 0.5)] [( e) '(1 0.5)]
      [(sw) '(0 1  )] [(s) '(0.5 1  )] [(se) '(1 1  )]
      [(bi) '(0 0  )]                  [(bo) '(1 0  )]))
  (match-define (list x y) (size q))
  (pt (coerce-int (* x x-fac))
      (coerce-int (+ (* y y-fac) (if (memq anchor '(bi bo)) (vertical-baseline-offset q) 0)))))

(define point/c (quad? . -> . point?))

(define/contract (inner-point q)
  point/c
  ;; calculate absolute location of inner-point
  ;; based on current origin and point type.
  ;; include offset, because it's intended to adjust inner 
  (pt+ (origin q) (anchor->local-point q (inner q)) (offset q)))

(define/contract (in-point q)
  point/c
  ;; calculate absolute location of in-point
  ;; based on current origin and point type.
  ;; don't include offset, so location is on bounding box
  (pt+ (origin q) (anchor->local-point q (in q))))

(define/contract (out-point q)
  point/c
  ;; calculate absolute location of out-point
  ;; based on current origin and point type.
  ;; don't include offset, so location is on bounding box
  (pt+ (origin q) (anchor->local-point q (out q))))


(define/contract (position q [previous-end-pt #f])
  ((quad?) (point?) . ->* . quad?)
  ;; recursively calculates coordinates for quad & subquads
  ;; based on starting origin point
  (set-origin! q (if previous-end-pt
                     (pt- previous-end-pt (in-point q))
                     (in-point q)))
  (for/fold ([pt (inner-point q)])
            ([q (in-list (elems q))]
             #:when (quad? q))
    (out-point (position q pt)))
  q)


(module+ test
  (require rackunit)
  (test-case
   "origins"
   (define size (pt 10 10))
   (define orig (pt 5 5))
   (check-equal? (origin (position (quad (hasheq 'in 'nw 'size size)) orig)) (pt 5 5))
   (check-equal? (origin (position (quad (hasheq 'in 'n 'size size)) orig)) (pt 0 5))
   (check-equal? (origin (position (quad (hasheq 'in 'ne 'size size)) orig)) (pt -5 5))
   (check-equal? (origin (position (quad (hasheq 'in 'e 'size size)) orig)) (pt -5 0))
   (check-equal? (origin (position (quad (hasheq 'in 'se 'size size)) orig)) (pt -5 -5))
   (check-equal? (origin (position (quad (hasheq 'in 's 'size size)) orig)) (pt 0 -5))
   (check-equal? (origin (position (quad (hasheq 'in 'sw 'size size)) orig)) (pt 5 -5))
   (check-equal? (origin (position (quad (hasheq 'in 'w 'size size)) orig)) (pt 5 0)))

  (test-case
   "in points"
   (check-equal? (in-point (quad (hasheq 'in 'nw 'size '(10 10) 'origin '(5 5)))) (pt 5 5))
   (check-equal? (in-point (quad (hasheq 'in 'n 'size '(10 10) 'origin '(5 5)))) (pt 10 5))
   (check-equal? (in-point (quad (hasheq 'in 'ne 'size '(10 10) 'origin '(5 5)))) (pt 15 5))
   (check-equal? (in-point (quad (hasheq 'in 'w 'size '(10 10) 'origin '(5 5)))) (pt 5 10))
   (check-equal? (in-point (quad (hasheq 'in 'c 'size '(10 10) 'origin '(5 5)))) (pt 10 10))
   (check-equal? (in-point (quad (hasheq 'in 'e 'size '(10 10) 'origin '(5 5)))) (pt 15 10))
   (check-equal? (in-point (quad (hasheq 'in 'sw 'size '(10 10) 'origin '(5 5)))) (pt 5 15))
   (check-equal? (in-point (quad (hasheq 'in 's 'size '(10 10) 'origin '(5 5)))) (pt 10 15))
   (check-equal? (in-point (quad (hasheq 'in 'se 'size '(10 10) 'origin '(5 5)))) (pt 15 15)))

  (test-case
   "out points"
   (check-equal? (out-point (quad (hasheq 'out 'nw 'size '(10 10) 'origin '(5 5)))) (pt 5 5))
   (check-equal? (out-point (quad (hasheq 'out 'n 'size '(10 10) 'origin '(5 5)))) (pt 10 5))
   (check-equal? (out-point (quad (hasheq 'out 'ne 'size '(10 10) 'origin '(5 5)))) (pt 15 5))
   (check-equal? (out-point (quad (hasheq 'out 'w 'size '(10 10) 'origin '(5 5)))) (pt 5 10))
   (check-equal? (out-point (quad (hasheq 'out 'c 'size '(10 10) 'origin '(5 5)))) (pt 10 10))
   (check-equal? (out-point (quad (hasheq 'out 'e 'size '(10 10) 'origin '(5 5)))) (pt 15 10))
   (check-equal? (out-point (quad (hasheq 'out 'sw 'size '(10 10) 'origin '(5 5)))) (pt 5 15))
   (check-equal? (out-point (quad (hasheq 'out 's 'size '(10 10) 'origin '(5 5)))) (pt 10 15))
   (check-equal? (out-point (quad (hasheq 'out 'se 'size '(10 10) 'origin '(5 5)))) (pt 15 15)))

  (test-case
   "inner points"
   (define size '(20 20))
   (define orig '(10 10))
   (check-equal? (inner-point (position (quad (hasheq 'size size 'inner 'nw)) orig)) (pt 10 10))
   (check-equal? (inner-point (position (quad (hasheq 'size size 'inner 'n)) orig)) (pt 20 10))
   (check-equal? (inner-point (position (quad (hasheq 'size size 'inner 'ne)) orig)) (pt 30 10))
   (check-equal? (inner-point (position (quad (hasheq 'size size 'inner 'e)) orig)) (pt 30 20))
   (check-equal? (inner-point (position (quad (hasheq 'size size 'inner 'se)) orig)) (pt 30 30))
   (check-equal? (inner-point (position (quad (hasheq 'size size 'inner 's)) orig)) (pt 20 30))
   (check-equal? (inner-point (position (quad (hasheq 'size size 'inner 'sw)) orig)) (pt 10 30))
   (check-equal? (inner-point (position (quad (hasheq 'size size 'inner 'w)) orig)) (pt 10 20)))

  (test-case
   "inner points with offsets"
   (define size (pt 10 10))
   (define orig (pt 0 0))
   (define off (pt (random 100) (random 100)))
   (check-equal? (inner-point (position (quad (hasheq 'size size 'inner 'nw 'offset off)) orig)) (pt+ '(0 0) off))
   (check-equal? (inner-point (position (quad (hasheq 'size size 'inner 'n 'offset off)) orig)) (pt+ '(5 0) off))
   (check-equal? (inner-point (position (quad (hasheq 'size size 'inner 'ne 'offset off)) orig)) (pt+ '(10 0) off))
   (check-equal? (inner-point (position (quad (hasheq 'size size 'inner 'e 'offset off)) orig)) (pt+ '(10 5) off))
   (check-equal? (inner-point (position (quad (hasheq 'size size 'inner 'se 'offset off)) orig)) (pt+ '(10 10) off))
   (check-equal? (inner-point (position (quad (hasheq 'size size 'inner 's 'offset off)) orig)) (pt+ '(5 10) off))
   (check-equal? (inner-point (position (quad (hasheq 'size size 'inner 'sw 'offset off)) orig)) (pt+ '(0 10) off))
   (check-equal? (inner-point (position (quad (hasheq 'size size 'inner 'w 'offset off)) orig)) (pt+ '(0 5) off)))

  (test-case
   "folding positions"
   (define (unit [attrs null] . elems) (apply quad (append attrs '(size (1 1))) elems))
   (check-equal? (position (unit null (unit '(out se) (unit) (unit) (unit))
                                 (unit '(out se) (unit) (unit) (unit))
                                 (unit '(out se) (unit) (unit) (unit))))
                 (unit '(origin (0 0))
                       (unit '(origin (0 0) out se)
                             (unit '(origin (0 0))) (unit '(origin (1 0))) (unit '(origin (2 0))))
                       (unit '(origin (1 1) out se)
                             (unit '(origin (1 1))) (unit '(origin (2 1))) (unit '(origin (3 1))))
                       (unit '(origin (2 2) out se)
                             (unit '(origin (2 2))) (unit '(origin (3 2))) (unit '(origin (4 2))))))))


(require racket/runtime-path fontkit/font)
(define-runtime-path fira "fira.ttf")

(define q1 (quad (list 'in 'bi 'out 'bo 'size '(10 10) 'font fira 'fontsize 12)))
(define q2 (quad (list 'in 'bi 'out 'bo 'size '(10 10) 'font fira 'fontsize 24)))
(define q3 (quad (list 'in 'bi 'out 'bo 'size '(10 10) 'font fira 'fontsize 6)))
#;(position (quad #f q1 q2 q3))


(module+ test
  (require rackunit)
  (define q (quad (list 'in 'bi 'out 'bo 'size '(10 10) 'font fira 'fontsize 12)))
  (check-equal? (ascender q) 935)
  (check-equal? (units-per-em q) 1000)
  (define ascender-scaled (* (/ (ascender q) (units-per-em q)) (hash-ref (attrs q) 'fontsize) 1.0))
  (check-equal? (in-point q) (list 0 ascender-scaled))
  (check-equal? (out-point q) (list 10 ascender-scaled)))