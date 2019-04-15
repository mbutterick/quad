#lang debug racket/base
(provide (all-defined-out))
(define current-doc (make-parameter #f))
(define current-pdf (make-parameter #f))

(define draw-debug? (make-parameter #f))
(define draw-debug-line? (make-parameter #t))
(define draw-debug-block? (make-parameter #t))
(define draw-debug-string? (make-parameter #t))