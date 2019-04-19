#lang debug racket/base
(require (for-syntax racket/base)
         quadwriter/core
         pollen/tag
         "lang-helper.rkt"
         quad)
(provide (except-out (all-from-out racket/base) #%module-begin)
         q)

(define q (default-tag-function 'q))

(define (doc-proc strs) (cons 'q strs))
(make-mb doc-proc)

(module+ reader
  (require scribble/reader syntax/strip-context "lang-helper.rkt")
  (provide (rename-out [quadwriter-rs read-syntax]))
  
  (define (quadwriter-rs path-string p)
    (define stxs (quad-at-reader path-string p))
    (strip-context
     (with-syntax ([STXS stxs]
                   [PDF-PATH (path-replace-extension path-string #".pdf")])
       #'(module _ quadwriter/main
           PDF-PATH
           . STXS)))))