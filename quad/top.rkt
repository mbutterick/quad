#lang quad/dev
(provide (all-defined-out))
(require "quads.rkt" (for-syntax racket/string racket/syntax))

(define-syntax (~top stx)
  (syntax-case stx ()
    [(_ . id)
     (let ([id-str (format "~a" (syntax->datum #'id))])
       (if (id-str . string-prefix? . "q:")
           (with-syntax ([new-id (format-id #'id "~a" (string-trim id-str "q:" #:right? #f))])
             #'(λ args (apply quad 'new-id args)))
           #'(#%top . id)))]))