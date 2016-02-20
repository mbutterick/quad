#lang racket/base
(provide (except-out (all-from-out racket/base) #%module-begin)
         (rename-out [quad-module-begin #%module-begin]))
(require (for-syntax racket/base syntax/strip-context))
(require quad/quads quad/main quad/world quad/render racket/class)

(define-syntax (quad-module-begin stx)
  (syntax-case stx ()
    [(_ expr ...)
     (replace-context #'(expr ...)
                      #'(#%module-begin
                         
                         (define out (block '(measure 200.0 font "Times New Roman" leading 16.0 vmeasure 300.0 size 13.5) expr ...))
                           (provide out)))]))

