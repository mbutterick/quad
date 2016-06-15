#lang racket/base
(provide (except-out (all-from-out racket/base) #%module-begin)
         (rename-out [quad-module-begin #%module-begin]))
(require (for-syntax racket/base syntax/strip-context))
(require quad/quads quad/typeset quad/world quad/render racket/class)

(define-syntax (quad-module-begin stx)
  (syntax-case stx ()
    [(_ expr ...)
     (replace-context #'(expr ...)
                      #'(#%module-begin
                         (module outy racket/base
                           (require quad/quads)
                           (define out (block '(font "Times New Roman" measure 360.0 leading 14.0 column-count 1 column-gutter 10.0 size 11.5 x-align justify x-align-last-line left) expr ...))
                           (provide out))
                         (require 'outy)
                         (provide (all-from-out 'outy))
                         (displayln out)))]))

(module reader syntax/module-reader
  quad/main
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
    (case key
      [(color-lexer)
       (define my-make-scribble-inside-lexer
         (dynamic-require 'syntax-color/scribble-lexer 'make-scribble-inside-lexer (λ () #f)))
       (cond [my-make-scribble-inside-lexer
              (my-make-scribble-inside-lexer #:command-char quad-command-char)]
             [else default])]
      [(drracket:toolbar-buttons)
       (define my-make-drracket-buttons (dynamic-require 'quad/buttons 'make-drracket-buttons))
       (my-make-drracket-buttons)]
      [else default])))