#lang debug br
(require racket/contract "quad.rkt" "generic.rkt")
(provide (all-defined-out))

(define pt-x real-part)
(define pt-y imag-part)
(define (pt x y) (+ x (* y +i)))
(define point? number?)

(define (valid-anchor? anchor)
  (define valid-anchors '(nw n ne e se s sw w))
  (and (memq anchor valid-anchors) #t))

(define (coerce-int x) (if (integer? x) (inexact->exact x) x))

(define/contract (relative-anchor-pt q anchor)
  (quad? symbol? . -> . point?)
  (unless (valid-anchor? anchor)
    (raise-argument-error 'anchor-adjustment "valid anchor" anchor))
  (define-values (xfac yfac)
    (case anchor
      [(nw) (values 0 0)]
      [(n) (values 0.5 0)]
      [(ne) (values 1 0)]
      [(e) (values 1 0.5)]
      [(se) (values 1 1)]
      [(s) (values 0.5 1)]
      [(sw) (values 0 1)]
      [(w) (values 0 0.5)]))
  (pt (coerce-int (* (pt-x (size q)) xfac))
      (coerce-int (* (pt-y (size q)) yfac))))


(define/contract (inner-point q)
  (quad? . -> . point?)
  (+ (origin q) (relative-anchor-pt q (inner q)) (offset q)))


(define/contract (end-point q)
  (quad? . -> . point?)
  ;; no offset because end-point is "pre-padding"
  (+ (origin q) (relative-anchor-pt q (end q))))
    

(define/contract (align! q where)
  (quad? point? . -> . quad?)
  (set-origin! q (- where (relative-anchor-pt q (start q))))
  q)

(define/contract (position q [where 0])
  ((quad?) (point?) . ->* . quad?)
  (align! q where)
  (fold-positions (elems q) (inner-point q))
  q)

(define/contract (fold-positions qs [start-pt 0])
  (((listof quad?)) (point?) . ->* . point?)
  (foldl (λ (q pt) (end-point (position q pt))) start-pt qs))



(module+ test
  (require rackunit)
  (test-case
   "origins"
   (define size 10+10i)
   (define orig 5+5i)
   (check-equal? (origin (position (quad (hasheq 'start 'nw 'size size)) orig)) 5+5i)
   (check-equal? (origin (position (quad (hasheq 'start 'n 'size size)) orig)) +5i)
   (check-equal? (origin (position (quad (hasheq 'start 'ne 'size size)) orig)) -5+5i)
   (check-equal? (origin (position (quad (hasheq 'start 'e 'size size)) orig)) -5)
   (check-equal? (origin (position (quad (hasheq 'start 'se 'size size)) orig)) -5-5i)
   (check-equal? (origin (position (quad (hasheq 'start 's 'size size)) orig)) -5i)
   (check-equal? (origin (position (quad (hasheq 'start 'sw 'size size)) orig)) 5-5i)
   (check-equal? (origin (position (quad (hasheq 'start 'w 'size size)) orig)) 5))

  (test-case
   "inner points"
   (define size 10+10i)
   (define orig 0)
   (check-equal? (inner-point (position (quad (hasheq 'size size 'inner 'nw)) orig)) 0)
   (check-equal? (inner-point (position (quad (hasheq 'size size 'inner 'n)) orig)) 5)
   (check-equal? (inner-point (position (quad (hasheq 'size size 'inner 'ne)) orig)) 10)
   (check-equal? (inner-point (position (quad (hasheq 'size size 'inner 'e)) orig)) 10+5i)
   (check-equal? (inner-point (position (quad (hasheq 'size size 'inner 'se)) orig)) 10+10i)
   (check-equal? (inner-point (position (quad (hasheq 'size size 'inner 's)) orig)) 5+10i)
   (check-equal? (inner-point (position (quad (hasheq 'size size 'inner 'sw)) orig)) +10i)
   (check-equal? (inner-point (position (quad (hasheq 'size size 'inner 'w)) orig)) +5i))

  (test-case
   "inner points with offsets"
   (define size 10+10i)
   (define orig 0)
   (define off (pt (random 100) (random 100)))
   (check-equal? (inner-point (position (quad (hasheq 'size size 'inner 'nw 'offset off)) orig)) (+ 0 off))
   (check-equal? (inner-point (position (quad (hasheq 'size size 'inner 'n 'offset off)) orig)) (+ 5 off))
   (check-equal? (inner-point (position (quad (hasheq 'size size 'inner 'ne 'offset off)) orig)) (+ 10 off))
   (check-equal? (inner-point (position (quad (hasheq 'size size 'inner 'e 'offset off)) orig)) (+ 10+5i off))
   (check-equal? (inner-point (position (quad (hasheq 'size size 'inner 'se 'offset off)) orig)) (+ 10+10i off))
   (check-equal? (inner-point (position (quad (hasheq 'size size 'inner 's 'offset off)) orig)) (+ 5+10i off))
   (check-equal? (inner-point (position (quad (hasheq 'size size 'inner 'sw 'offset off)) orig)) (+ +10i off))
   (check-equal? (inner-point (position (quad (hasheq 'size size 'inner 'w 'offset off)) orig)) (+ +5i off)))

  (test-case
   "folding positions"
   (check-equal? (position (quad (quad (hasheq 'size +i 'end 'se) (quad) (quad) (quad))
                                 (quad (hasheq 'size +i 'end 'se) (quad) (quad) (quad))
                                 (quad (hasheq 'size +i 'end 'se) (quad) (quad) (quad))))

                 (position (quad (quad (hasheq 'size +i 'end 'se 'origin 0) (quad (hasheq 'origin 0))
                                       (quad (hasheq 'origin 1)) (quad (hasheq 'origin 2)))
                                 (quad (hasheq 'size +i 'end 'se 'origin +i) (quad (hasheq 'origin +i))
                                       (quad (hasheq 'origin 1+i)) (quad (hasheq 'origin 2+i)))
                                 (quad (hasheq 'size +i 'end 'se 'origin +2i) (quad (hasheq 'origin +2i))
                                       (quad (hasheq 'origin 1+2i)) (quad (hasheq 'origin 2+2i))))))))