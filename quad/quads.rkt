#lang quad/dev
(provide (all-defined-out))
(require (for-syntax racket/string))

(define (quad-name q) (vector-ref q 0))
(define (quad-attrs q) (vector-ref q 1))
(define (quad-list q) (vector-ref q 2))


(define (quad? x)
  (and (vector? x)
       (symbol? (quad-name x))
       (or (not (quad-attrs x)) (hash? (quad-attrs x)))
       (list? (quad-list x))))

(define (quad-attrs? x) (list? x))

(define (quad name attrs xs)
  (vector name attrs xs))


(module+ test
  (require rackunit)
  (define q (quad 'foo #f '("bar")))
  (check-true (quad? q))
  (check-false (quad? 42))
  (check-equal? (quad-name q) 'foo)
  (check-equal? (quad-attrs q) #f)
  (check-equal? (quad-list q) '("bar")))