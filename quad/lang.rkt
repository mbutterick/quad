#lang racket/base
(require (for-syntax racket/base)
         quad
         (prefix-in doclang: "doclang-raw.rkt"))
(provide (rename-out [quad-mb #%module-begin])
         #%top-interaction
         #%top
         #%app
         #%datum
         (except-out (all-from-out racket/base) #%module-begin))

(define-for-syntax export-name 'quad)
(define-syntax (quad-mb stx)
  (syntax-case stx ()
    [(_ . ARGS)
     (with-syntax ([EXPORT (datum->syntax stx export-name)])
       #'(doclang:#%module-begin
          EXPORT ; positional arg for doclang-raw: name of export
          (λ (xs)
            (define x (q #:elems xs))
            (println x)
            x) ;  positional arg for doclang-raw: post-processor
          (provide EXPORT)
          (begin . ARGS)))]))