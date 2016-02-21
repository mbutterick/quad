#lang s-exp syntax/module-reader
quad/lang/quad
#:read quad-read
#:read-syntax quad-read-syntax
#:whole-body-readers? #t ;; need this to make at-reader work
#:info custom-get-info
(require scribble/reader)

(define (quad-read p)
  (syntax->datum (quad-read-syntax (object-name p) p)))

(define quad-command-char #\@)

(define (quad-read-syntax path-string p)
  (define quad-at-reader (make-at-reader
                          #:command-char quad-command-char
                          #:syntax? #t 
                          #:inside? #t))
  (define source-stx (quad-at-reader path-string p))
  source-stx)

(define (custom-get-info key default [proc (λ _ #f)])
  (displayln 'yay)
  (case key
    [(color-lexer)
     (define my-make-scribble-inside-lexer
       (dynamic-require 'syntax-color/scribble-lexer 'make-scribble-inside-lexer (λ () #f)))
     (cond [my-make-scribble-inside-lexer
            (my-make-scribble-inside-lexer #:command-char quad-command-char)]
           [else default])]
    [(drracket:toolbar-buttons)
     (define my-make-drracket-buttons (dynamic-require 'quad/lang/buttons 'make-drracket-buttons))
     (my-make-drracket-buttons)]
    [else default]))