#lang debug racket/base
(require racket/match
         scribble/reader
         txexpr)
(provide (all-defined-out))

(define (path-string->pdf-path path-string)
  (match (format "~a" path-string)
    ["unsaved-editor" (build-path (find-system-path 'desk-dir) "untitled.pdf")]
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