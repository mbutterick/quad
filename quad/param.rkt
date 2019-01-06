#lang racket/base
(provide (all-defined-out))

(define current-default-attrs (make-parameter (make-hasheq)))
(define current-wrap-distance (make-parameter 1))
(define current-default-font-size (make-parameter 12))