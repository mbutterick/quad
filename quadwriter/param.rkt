#lang debug racket/base
(provide (all-defined-out))
(define current-doc (make-parameter #f))
(define current-pdf (make-parameter #f))
(define current-locale (make-parameter 'us))

(define draw-debug? (make-parameter #t))
(define draw-debug-line? (make-parameter #t))
(define draw-debug-block? (make-parameter #t))
(define draw-debug-string? (make-parameter #t))