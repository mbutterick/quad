#lang quad/dev
(provide (all-defined-out))
(require (for-syntax racket/string))


(define (quad-attrs q) (vector-ref q 1))
(define (quad-list q) (vector-ref q 0))


(define (quad? x)
  (and (vector? x)
       (vector? (quad-attrs x))
       (list? (quad-list x))))

(define (quad-attrs? x) (list? x))


(define default-attrs (vector 12 "Courier" 0 0))
(define (quad attrs . xs)
  (vector-immutable xs attrs))

(define (make-quad-attrs #:size [size #f]
              #:font [font #f]
              #:x [x #f]
              #:y [y #f])
  (vector size font x y))

(define attrs '(size font x y))

(define (attr-size a) (vector-ref a 0))
(define (attr-font a) (vector-ref a 1))
(define (attr-x a) (vector-ref a 2))
(define (attr-y a) (vector-ref a 3))

(module+ test
  (require rackunit)
  (define q (quad "bar"))
  (check-true (quad? q))
  (check-false (quad? 42))
  (check-equal? (quad-attrs q) default-attrs)
  (check-equal? (quad-list q) '("bar")))