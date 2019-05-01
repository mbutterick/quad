#lang debug racket/base
(require racket/list
         racket/match
         quadwriter/core
         "tags.rkt"
         "lang-helper.rkt")
(provide (all-defined-out)
         #%app #%top #%datum #%top-interaction require
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
                 [(? null?) '(" ")] ; single nonbreaking space, so something prints
                 [strs strs]))
  ;; markdown parser returns list of paragraphs
  (root null (match strs
               [(list str) strs]
               [_ (add-between strs (list qexpr-para-break)
                               #:before-first (list qexpr-para-break)
                               #:after-last (list qexpr-para-break)
                               #:splice? #true)])))
(make-module-begin doc-proc)

(module reader racket/base
  (require racket/port markdown "lang-helper.rkt")
  (provide read-syntax get-info)
  (define read-syntax (make-read-syntax 'quadwriter/markdown
                                        (λ (path-string p) (xexpr->parse-tree (parse-markdown (port->string p)))))))