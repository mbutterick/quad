#lang quad/dev
(require "quads.rkt" "typeset.rkt" "atomize.rkt" "render.rkt" racket/list racket/string)
(provide (except-out (all-from-out quad/dev "quads.rkt") #%module-begin)
         (rename-out [~module-begin #%module-begin]))

(define-syntax-rule (~module-begin lang-line-config-arg . args)
  (#%module-begin
   (define main-quad (apply quad #f (add-between (list . args) "\n"))) ; at-reader splits lines, but we want one contiguous run
   ;; branch on config-arg to allow debug / inspection options on #lang line
   (define config-pieces (string-split (string-trim lang-line-config-arg)))
   (case (car config-pieces)
     [("#:atoms") (atomize main-quad)]
     [("#:fit") (time (debug-render (apply typeset-fit (atomize main-quad) (map string->number (cdr config-pieces)))))]
     [else (typeset-fit (atomize main-quad))])))

(module reader syntax/module-reader
  quad/main)