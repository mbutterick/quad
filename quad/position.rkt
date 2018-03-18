#lang debug br
(require racket/contract "quad.rkt" "generic.rkt")
(provide (all-defined-out))

(define pt-x first)
(define pt-y second)
(define (pt x y) (list x y))
(define (pt+ . pts) (apply map + pts))
(define (pt- . pts) (apply map - pts))
(define point? (list/c number? number?))


(define (valid-anchor? anchor)
  (define valid-anchors '(nw n ne w c e sw s se bi bo))
  (and (memq anchor valid-anchors) #t))


(define (coerce-int x) (if (integer? x) (inexact->exact x) x))

(define/contract (ascender q)
  (quad? . -> . real?)
  (define p (hash-ref (attrs q) 'font "Courier"))
  (unless p
    (error 'ascender-no-font-key))
  (ascent (openSync p)))

(define/contract (units-per-em q)
  (quad? . -> . real?)
  (define p (hash-ref (attrs q) 'font "Courier"))
  (unless p
    (error 'units-per-em-no-font-key))
  (unitsPerEm (openSync p)))

(define (fontsize q)
  (define val (hash-ref (attrs q) 'fontsize 0))
  ((if (number? val) values string->number) val))


(define/contract (anchor->point q anchor)
  (quad? symbol? . -> . point?)
  (unless (valid-anchor? anchor)
    (raise-argument-error 'relative-anchor-pt "valid anchor" anchor))
  (match-define (list x-fac y-fac)
    (case anchor
      [(nw) '(0 0  )] [(n) '(0.5 0  )] [(ne) '(1 0  )]
      [( w) '(0 0.5)] [(c) '(0.5 0.5)] [( e) '(1 0.5)]
      [(sw) '(0 1  )] [(s) '(0.5 1  )] [(se) '(1 1  )]
      [(bi) (list 0 (* (/ (ascender q) (units-per-em q) 1.0) (fontsize q)))]
      [(bo) (list 1 (* (/ (ascender q) (units-per-em q) 1.0) (fontsize q)))]))
  (match-define (list x y) (size q))
  (pt (coerce-int (* x x-fac)) (coerce-int (* y y-fac))))

(define point/c (quad? . -> . point?))

(define/contract (inner-point q)
  point/c
  (pt+ (origin q) (anchor->point q (inner q)) (offset q)))

(define/contract (in-point q)
  point/c
  (anchor->point q (in q)))

(define/contract (out-point q)
  point/c
  (pt+ (origin q) (anchor->point q (out q)))) ; no offset because end-point is calculated without padding


(define/contract (position q [previous-end-pt (origin q)])
  ((quad?) (point?) . ->* . quad?)
  (set-origin! q (pt- previous-end-pt (in-point q)))
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
   "inner points"
   (define size '(10 10))
   (define orig '(0 0))
   (check-equal? (inner-point (position (quad (hasheq 'size size 'inner 'nw)) orig)) (pt 0 0))
   (check-equal? (inner-point (position (quad (hasheq 'size size 'inner 'n)) orig)) (pt 5 0))
   (check-equal? (inner-point (position (quad (hasheq 'size size 'inner 'ne)) orig)) (pt 10 0))
   (check-equal? (inner-point (position (quad (hasheq 'size size 'inner 'e)) orig)) (pt 10 5))
   (check-equal? (inner-point (position (quad (hasheq 'size size 'inner 'se)) orig)) (pt 10 10))
   (check-equal? (inner-point (position (quad (hasheq 'size size 'inner 's)) orig)) (pt 5 10))
   (check-equal? (inner-point (position (quad (hasheq 'size size 'inner 'sw)) orig)) (pt 0 10))
   (check-equal? (inner-point (position (quad (hasheq 'size size 'inner 'w)) orig)) (pt 0 5)))

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

(module+ test
  (define q (quad (list 'in 'bi 'out 'bo 'size '(1 1) 'font fira 'fontsize 72)))
  (in-point q)
  (out-point q)
  (ascender q)
  (units-per-em q))