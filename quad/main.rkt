#lang racket/base
(require "quads.rkt" "doc.rkt" "parse.rkt" "tokenize.rkt" racket/list racket/string)
(provide (except-out (all-from-out racket/base "quads.rkt") #%module-begin)
         (rename-out [~module-begin #%module-begin]))

(define-namespace-anchor ns)

(define-syntax-rule (~module-begin lang-line-config-arg . args)
  (#%module-begin
   (define main-quad (quad #f . args))
   ;; branch on config-arg to allow debug / inspection options on #lang line
   (case (string-trim lang-line-config-arg)
     [("#:atoms") (tokenize main-quad)]
     [else (eval (parse (tokenize main-quad)) (namespace-anchor->namespace ns))])))

(module reader syntax/module-reader
  "main.rkt")