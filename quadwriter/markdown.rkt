#lang debug racket/base
(require (for-syntax racket/base)
         racket/list
         racket/match
         quadwriter/core
         "tags.rkt"
         "font.rkt"
         "reader-helper.rkt"
         "param.rkt")
(provide (except-out (all-defined-out) mb)
         (rename-out [mb #%module-begin])
         #%app #%datum #%top-interaction
         (all-from-out "tags.rkt"))

(define rsquo "’")
(define rdquo "”")
(define lsquo "‘")
(define ldquo "“")
(define hellip "…")
(define ndash "–")
(define mdash "—")

(define-syntax (mb stx)
  (syntax-case stx ()
    [(_ PATH-STRING . STRS)
     (with-syntax ([DOC (datum->syntax #'PATH-STRING 'doc)])
       #'(#%module-begin
          ;; stick an nbsp in the strings so we have one printing char
          (define strs (match (list . STRS)
                         [(? null?) '(" ")]
                         [strs strs]))
          (define DOC (root null (add-between strs (list pbr)
                                              #:before-first (list pbr)
                                              #:after-last (list pbr)
                                              #:splice? #true)))
          (provide DOC)
          (module+ main
            (render-pdf DOC (path-string->pdf-path 'PATH-STRING)))))]))

(module reader racket/base
  (require syntax/strip-context
           (only-in markdown parse-markdown)
           "reader-helper.rkt")
  (provide (rename-out [rs read-syntax]))
 
  (define (rs path-string p)
    (define stxs (quad-at-reader path-string p))
    (define parsed-stxs
      (datum->syntax stxs
                     (xexpr->parse-tree
                      (parse-markdown (apply string-append (syntax->datum stxs))))))
    (strip-context
     (with-syntax ([PATH-STRING path-string]
                   [PARSED-STXS parsed-stxs])
       #'(module _ quadwriter/markdown
           PATH-STRING
           . PARSED-STXS)))))