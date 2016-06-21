#lang quad/dev
(require "quads.rkt" "typeset.rkt" "atomize.rkt" "render.rkt" "render-pdf.rkt" racket/list racket/string)
(provide (except-out (all-from-out quad/dev "quads.rkt") #%module-begin)
         (rename-out [~module-begin #%module-begin]))

(define-syntax-rule (~module-begin lang-line-config-arg . args)
  (#%module-begin
   (define main-quad (apply quad #f (list . args))) ; at-reader splits lines, but we want one contiguous run
   ;; branch on config-arg to allow debug / inspection options on #lang line
   (define config-pieces (string-split (string-trim lang-line-config-arg)))
   (and (pair? config-pieces)
        (let ([config-args (map string->number (cdr config-pieces))])
          (case (car config-pieces)
            [("in") (atomize main-quad)]
            [("out") (time (apply fit (atomize main-quad) config-args))]
            [("test") (time (debug-render (apply fit (atomize main-quad) config-args)))]
            [("pdf") (time (render-pdf (apply fit (atomize main-quad) config-args)))]
            [else (fit (atomize main-quad))])))))

(module reader syntax/module-reader
  quad/main)