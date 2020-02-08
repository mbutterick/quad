#lang debug racket
(define-for-syntax debug-mode #false)

(define-syntax (go stx)
  (syntax-case stx ()
    [_
     debug-mode
     #'(begin
         (provide (all-defined-out))
         (define current-doc (make-parameter #false))
         (define current-pdf (make-parameter #false))
         (define current-line-wrap (make-parameter #f)) ; because kp is slow and maybe we want to disable for "draft" mode
         (define section-pages-used (make-parameter 0))
         (define current-named-quads (make-parameter #false))
         
         (define quadwriter-test-mode (make-parameter #f)) ; used during rackunit to suppress nondeterministic elements, like timestamp in header

         (define draw-debug? (make-parameter #true))
         (define draw-debug-line? (make-parameter #true))
         (define draw-debug-block? (make-parameter #false))
         (define draw-debug-string? (make-parameter #true))
         (define draw-debug-image? (make-parameter #false))

         (define debug-page-width (make-parameter 400))
         (define debug-page-height (make-parameter 400))
         (define debug-x-margin (make-parameter 50))
         (define debug-y-margin (make-parameter 50))
         (define debug-column-count (make-parameter 1))
         (define debug-column-gap (make-parameter 36))
         (define zoom-factor (make-parameter 1))
         (define log-clipping? (make-parameter 'warn)))]
    [_
     #'(begin
         (provide (all-defined-out))
         (define current-doc (make-parameter #false))
         (define current-pdf (make-parameter #false))
         (define current-line-wrap (make-parameter #f))
         (define section-pages-used (make-parameter 0))
         (define current-named-quads (make-parameter #false))
         
         (define quadwriter-test-mode (make-parameter #f))

         (define draw-debug? (make-parameter #false))
         (define draw-debug-line? (make-parameter #true))
         (define draw-debug-block? (make-parameter #true))
         (define draw-debug-string? (make-parameter #true))
         (define draw-debug-image? (make-parameter #true))

         (define debug-page-width (make-parameter #f))
         (define debug-page-height (make-parameter #f))
         (define debug-x-margin (make-parameter #f))
         (define debug-y-margin (make-parameter #f))
         (define debug-column-count (make-parameter #f))
         (define debug-column-gap (make-parameter #f))
         (define zoom-factor (make-parameter 1))
         (define log-clipping? (make-parameter 'warn)))]))

(go)