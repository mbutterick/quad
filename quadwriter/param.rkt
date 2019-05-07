#lang debug racket
(define-for-syntax debug-mode #true)

(define-syntax (go stx)
  (syntax-case stx ()
    [_
     debug-mode
     #'(begin
         (provide (all-defined-out))
         (define current-doc (make-parameter #false))
         (define current-pdf (make-parameter #false))
         (define current-locale (make-parameter 'us))

         (define draw-debug? (make-parameter #t))
         (define draw-debug-line? (make-parameter #true))
         (define draw-debug-block? (make-parameter #true))
         (define draw-debug-string? (make-parameter #true))

         (define debug-page-width (make-parameter 400))
         (define debug-page-height (make-parameter 400))
         (define debug-x-margin (make-parameter 40))
         (define debug-y-margin (make-parameter 40))
         (define zoom-factor (make-parameter 3)))]
    [_
     #'(begin
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
         (define debug-x-margin (make-parameter #f))
         (define debug-y-margin (make-parameter 40))
         (define zoom-factor (make-parameter 1)))]))

(go)