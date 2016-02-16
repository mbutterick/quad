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
                         (define src (box null (list expr ...)))
                         (parameterize ([world:quality-default world:draft-quality])
                           (displayln "Typesetting:")
                           (displayln src)
                           (define to (time (typeset src)))
                           (displayln "PDF rendering:"))))]))