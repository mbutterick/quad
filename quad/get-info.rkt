#lang racket
(require racket/class)
(provide get-info)

(define (custom-indent-rules x)
  (match x
    ;; tbd: custom indent rules, examples below
    #;[("with-pattern"
        "with-shared-id"
        "pattern-case"
        "pattern-case-filter") 'lambda]
    #;[("define-macro"
        "define-macro-cases"
        "define-cases"
        "while"
        "until") 'define]
    [_ #false]))

(define (indenter t pos)
  (send t compute-racket-amount-to-indent pos custom-indent-rules))

(define (get-info key default-value proc)
  (define (fallback) (if proc (proc key default-value) default-value))
  (define (try-dynamic-require lib export)
    (with-handlers ([exn:missing-module?
                     (λ (x) (case key
                              [(drracket:indentation) indenter]
                              [else (fallback)]))])
      (dynamic-require lib export)))
  (case key
    [(color-lexer)
     (try-dynamic-require 'syntax-color/scribble-lexer 'scribble-lexer)]
    [(drracket:indentation)
     (try-dynamic-require 'scribble/private/indentation 'determine-spaces)]
    [(drracket:keystrokes)
     (try-dynamic-require 'scribble/private/indentation 'keystrokes)]
    [else (fallback)]))