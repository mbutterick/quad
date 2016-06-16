#lang racket/base
(provide (all-defined-out))
(require (for-syntax racket/string))

(struct $quad (attrs list) #:transparent)

(define quad? $quad?)

(define quad-attrs $quad-attrs)
(define quad-list $quad-list)

(define (quad-attrs? x) (list? x))


#|
Attrs needed to specify rendered appearance:
(font) family
(font) style 
(font) size
color
background
position

|#
(define default-attrs (vector 12 "Courier" 0))
(define (quad attr . xs)
  ($quad (or attr (attrs)) xs))

(define (attrs #:size [size #f]
               #:font [font #f]
               #:posn [posn #f])
  (vector size font posn))


(define (attr-size a) (vector-ref a 0))
(define (attr-font a) (vector-ref a 1))
(define (attr-x a) (vector-ref a 2))
(define (attr-y a) (vector-ref a 3))

(define (override-with dest source)
  ;; replace missing values in dest with values from source
  (for ([i (in-range (vector-length source))])
            (unless (vector-ref dest i)
              (vector-set! dest i (vector-ref source i))))
  dest)

(module+ test
  (require rackunit)
  (define q (quad #f "bar"))
  (check-true (quad? q))
  (check-false (quad? 42))
  (check-equal? (quad-attrs q) (attrs))
  (check-equal? (quad-list q) '("bar")))