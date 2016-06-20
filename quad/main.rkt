#lang racket/base
(require "quads.rkt" "typeset.rkt" "atomize.rkt" "render.rkt" racket/list racket/string)
(provide (except-out (all-from-out racket/base "quads.rkt") #%module-begin)
         (rename-out [~module-begin #%module-begin]))

(define-syntax-rule (~module-begin lang-line-config-arg . args)
  (#%module-begin
   (define main-quad (apply quad #f (add-between (list . args) "\n"))) ; at-reader splits lines, but we want one contiguous run
   ;; branch on config-arg to allow debug / inspection options on #lang line
   (case (string-trim lang-line-config-arg)
     [("#:atoms") (atomize main-quad)]
     [("#:text") (time (debug-render (typeset-fit (atomize main-quad))))]
     [else (typeset-fit (atomize main-quad))])))

(module reader syntax/module-reader
  "main.rkt")