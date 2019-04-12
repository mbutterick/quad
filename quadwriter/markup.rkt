#lang debug racket/base
(require (for-syntax racket/base)
         racket/list
         racket/match
         quadwriter/core
         "tags.rkt")
(provide (except-out (all-defined-out) mb)
         (rename-out [mb #%module-begin])
         #%app #%datum #%top-interaction)
(provide (all-from-out "tags.rkt")
         rsquo rsquo lsquo ldquo hellip ndash mdash)

(define rsquo "’")
(define rdquo "”")
(define lsquo "‘")
(define ldquo "“")
(define hellip "…")
(define ndash "–")
(define mdash "—")

(define-syntax-rule (mb PDF-PATH . STRS)
  (#%module-begin
   ;; stick an nbsp in the strings so we have one printing char
   (define strs (match (list . STRS)
                  [(? null?) '(" ")]
                  [strs strs]))
   (define qx (root null (add-between strs (list pbr)
                                      #:before-first (list pbr)
                                      #:after-last (list pbr)
                                      #:splice? #true)))
   (run qx PDF-PATH)))

(module reader racket/base
  (require pollen/decode syntax/strip-context "reader-helper.rkt")
  (provide (rename-out [rs read-syntax]))
 
  (define (rs path-string p)
    (define stx (quad-at-reader path-string p))
    (define parsed-stxs
      (datum->syntax stx
                     (xexpr->parse-tree
                      (decode-paragraphs (syntax->datum stx)))))
    (strip-context
     (with-syntax ([STXS parsed-stxs]
                   [PDF-PATH (path-string->pdf-path path-string)])
       #'(module _ quadwriter/markdown
           PDF-PATH
           . STXS)))))