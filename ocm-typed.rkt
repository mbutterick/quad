#lang typed/racket/base
(require (for-syntax racket/base racket/syntax))
(require racket/list sugar/debug rackunit racket/function racket/vector sugar/cache "logger-typed.rkt")
(define-logger ocm)

#|
Totally monotone matrix searching algorithms.

The offline algorithm in ConcaveMinima is from Agarwal, Klawe, Moran,
Shor, and Wilbur, Geometric applications of a matrix searching algorithm,
Algorithmica 2, pp. 195-208 (1987).

The online algorithm in OnlineConcaveMinima is from Galil and Park,
A linear time algorithm for concave one-dimensional dynamic programming,
manuscript, 1989, which simplifies earlier work on the same problem
by Wilbur (J. Algorithms 1988) and Eppstein (J. Algorithms 1990).

D. Eppstein, March 2002, significantly revised August 2005

|#

;(provide smawky? make-ocm reduce reduce2 (prefix-out ocm- (combine-out min-value min-index)))

(define (select-elements xs is)
  (map (curry list-ref xs) is))

(define (odd-elements xs)
  (select-elements xs (range 1 (length xs) 2)))

(define (vector-odd-elements xs)
  (for/vector ([i (in-range (vector-length xs))] #:when (odd? i))
    (vector-ref xs i)))

(define (even-elements xs)
  (select-elements xs (range 0 (length xs) 2)))
