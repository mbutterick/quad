#lang debug racket/base
(provide (all-defined-out))
(define current-doc (make-parameter #false))
(define current-pdf (make-parameter #false))
(define current-locale (make-parameter 'us))

(define draw-debug? (make-parameter #false))
(define draw-debug-line? (make-parameter #true))
(define draw-debug-block? (make-parameter #true))
(define draw-debug-string? (make-parameter #true))

(define debug-page-width (make-parameter #f))
(define debug-page-height (make-parameter #f))
(define zoom-factor (make-parameter 1))