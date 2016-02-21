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
                         (module outy racket/base
                           (require quad/quads)
                           (define out (block '(font "Times New Roman" measure 360.0 leading 14.0 column-count 1 column-gutter 10.0 size 11.5 x-align justify x-align-last-line left) expr ...))
                           (provide out))
                         (require 'outy)
                         (provide (all-from-out 'outy))
                         (displayln out)))]))
