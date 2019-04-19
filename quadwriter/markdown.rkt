#lang debug racket/base
(require (for-syntax)
         racket/list
         racket/match
         quadwriter/core
         "tags.rkt"
         "lang-helper.rkt")
(provide (all-defined-out)
         #%app #%datum #%top-interaction
         (all-from-out "tags.rkt"))

(define rsquo "’")
(define rdquo "”")
(define lsquo "‘")
(define ldquo "“")
(define hellip "…")
(define ndash "–")
(define mdash "—")

(define (doc-proc exprs)
  (define strs (match exprs
                 [(? null?) '(" ")]
                 [strs strs]))
  (root null (add-between strs (list pbr)
                          #:before-first (list pbr)
                          #:after-last (list pbr)
                          #:splice? #true)))

(make-mb doc-proc)

(module reader racket/base
  (require syntax/strip-context
           racket/port
           (only-in markdown parse-markdown)
           "lang-helper.rkt")
  (provide (rename-out [rs read-syntax]))
 
  (define (rs path-string p)
    (define pt (xexpr->parse-tree (parse-markdown (port->string p))))
    (strip-context
     (with-syntax ([PATH-STRING path-string]
                   [PT pt])
       #'(module _ quadwriter/markdown
           PATH-STRING
           . PT)))))