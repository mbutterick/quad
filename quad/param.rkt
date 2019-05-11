#lang racket/base
(provide (all-defined-out))

(define current-default-attrs (make-parameter (make-hasheq)))
(define current-wrap-distance (make-parameter 1))
(define current-default-font-size (make-parameter 12))
(define current-missing-glyph-action (make-parameter #f)) ; #f or 'error or 'warning or 'fallback or 'omit