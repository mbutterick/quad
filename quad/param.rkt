#lang racket/base
(provide (all-defined-out))

(define current-default-attrs (make-parameter (make-hasheq)))
(define current-wrap-distance (make-parameter 1))