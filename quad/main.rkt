#lang racket/base
(require "quads.rkt" "typeset.rkt" "atomize.rkt" racket/list racket/string)
(provide (except-out (all-from-out racket/base "quads.rkt") #%module-begin)
         (rename-out [~module-begin #%module-begin]))

(define-syntax-rule (~module-begin lang-line-config-arg . args)
  (#%module-begin
   (define main-quad (quad #f . args))
   ;; branch on config-arg to allow debug / inspection options on #lang line
   (case (string-trim lang-line-config-arg)
     [("#:atoms") (atomize main-quad)]
     [else (typeset (atomize main-quad))])))

(module reader syntax/module-reader
  "main.rkt")