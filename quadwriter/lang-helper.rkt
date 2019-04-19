#lang debug racket/base
(require (for-syntax racket/base)
         racket/match
         scribble/reader
         quadwriter/core
         txexpr)
(provide (all-defined-out))

(define-syntax-rule (make-mb DOC-PROC)
  (begin
    (provide (rename-out [mb #%module-begin]))
    (define-syntax (mb stx)
      (syntax-case stx ()
        [(_ PATH-STRING . EXPRS)
         (with-syntax ([DOC (datum->syntax #'PATH-STRING 'doc)])
           #'(#%module-begin
              ;; stick an nbsp in the strings so we have one printing char
              (define DOC (DOC-PROC (list . EXPRS)))
              (provide DOC)
              (module+ main
                (render-pdf DOC (path-string->pdf-path 'PATH-STRING)))))]))))

(define (path-string->pdf-path path-string)
  (match (format "~a" path-string)
    ;; weird test but sometimes DrRacket calls the unsaved file
    ;; 'unsaved-editor and sometimes "unsaved editor"
    [(regexp #rx"unsaved.editor")
     (build-path (find-system-path 'desk-dir) "untitled.pdf")]
    [_ (path-replace-extension path-string #".pdf")]))

(define quad-at-reader (make-at-reader
                        #:syntax? #t 
                        #:inside? #t
                        #:command-char #\◊))

(define (xexpr->parse-tree x)
  ;; an ordinary txexpr can't serve as a parse tree because of the attrs list fails when passed to #%app.
  ;; so stick an `attr-list` identifier on it which can hook into the expander.
  ;; sort of SXML-ish.
  (let loop ([x x])
    (match x
      [(txexpr tag attrs elems) (list* tag (cons 'attr-list attrs) (map loop elems))]
      [(? list? xs) (map loop xs)]
      [_ x])))