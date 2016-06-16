#lang racket/base
(require (for-syntax racket/base) racket/list sugar/debug "quads.rkt")
(provide (except-out (all-from-out racket/base) #%module-begin)
         (all-from-out racket/list sugar/debug "quads.rkt")
         (rename-out [~module-begin #%module-begin])
         (for-syntax (all-from-out racket/base)))

(define-syntax-rule (~module-begin . args)
  (#%module-begin
     . args))

(module reader syntax/module-reader
  #:language 'quad/dev)