#lang racket/base
(require (for-syntax racket/base))
(provide (rename-out [quad-mb #%module-begin])
         #%top-interaction)

(define-syntax (quad-mb stx)
  (syntax-case stx ()
    [(_ . ARGS) #'(#%module-begin . ARGS)]))