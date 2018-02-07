#lang racket/base
(provide (all-defined-out))

(define current-default-attrs (make-parameter (make-hasheq)))
(define current-line-width (make-parameter 1))