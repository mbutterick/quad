#lang debug racket/base
(require (for-syntax racket/base)
         quadwriter/core
         pollen/tag
         quad)
(provide (except-out (all-from-out racket/base) #%module-begin)
         (rename-out [mb #%module-begin])
         q)

(define q (default-tag-function 'q))

(define-syntax-rule (mb PDF-PATH . EXPRS)
  (#%module-begin
     (run (cons 'q (list . EXPRS)) PDF-PATH)))

(module+ reader
  (require scribble/reader syntax/strip-context)
  (provide (rename-out [quadwriter-rs read-syntax]))
  
  (define (quadwriter-rs path-string p)
    (define quad-at-reader (make-at-reader
                            #:syntax? #t 
                            #:inside? #t
                            #:command-char #\◊))
    (define stxs (quad-at-reader path-string p))
    (strip-context
     (with-syntax ([STXS stxs]
                   [PDF-PATH (path-replace-extension path-string #".pdf")])
       #'(module _ quadwriter/main
           PDF-PATH
           . STXS)))))