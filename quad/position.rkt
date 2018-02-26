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
  (define valid-anchors '(nw n ne w c e sw s se))
  (and (memq anchor valid-anchors) #t))


(define (coerce-int x) (if (integer? x) (inexact->exact x) x))


(define/contract (anchor->point q anchor [signal #f])
  ((quad? symbol?) (any/c) . ->* . point?)
  (unless (valid-anchor? anchor)
    (raise-argument-error 'relative-anchor-pt "valid anchor" anchor))
  (match-define (list x-fac y-fac)
    (case anchor
      [(nw) '(0 0  )] [(n) '(0.5 0  )] [(ne) '(1 0  )]
      [( w) '(0 0.5)] [(c) '(0.5 0.5)] [( e) '(1 0.5)]
      [(sw) '(0 1  )] [(s) '(0.5 1  )] [(se) '(1 1  )]))
  (match-define (list x y) (size q signal))
  (pt (coerce-int (* x x-fac)) (coerce-int (* y y-fac))))

(define point/c ((quad?) (any/c) . ->* . point?))

(define/contract (inner-point q [signal #f])
  point/c
  (pt+ (origin q) (anchor->point q (inner q) signal) (offset q)))

(define/contract (in-point q [signal #f])
  point/c
  (anchor->point q (in q) signal))

(define/contract (out-point q [signal #f])
  point/c
  (pt+ (origin q) (anchor->point q (out q) signal))) ; no offset because end-point is calculated without padding


(define/contract (position q [previous-end-pt (pt 0 0)])
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
   (check-equal? (position (quad (quad '(out se) (quad) (quad) (quad))
                                 (quad '(out se) (quad) (quad) (quad))
                                 (quad '(out se) (quad) (quad) (quad))))
                 (quad '(origin (0 0))
                       (quad '(origin (0 0) out se) (quad '(origin (0 0))) (quad '(origin (1 0))) (quad '(origin (2 0))))
                       (quad '(origin (1 1) out se) (quad '(origin (1 1))) (quad '(origin (2 1))) (quad '(origin (3 1))))
                       (quad '(origin (2 2) out se) (quad '(origin (2 2))) (quad '(origin (3 2))) (quad '(origin (4 2))))))))