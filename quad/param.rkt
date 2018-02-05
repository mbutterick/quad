#lang racket/base
(provide (all-defined-out))

(define current-default-attrs (make-parameter null))
(define current-line-width (make-parameter 1))