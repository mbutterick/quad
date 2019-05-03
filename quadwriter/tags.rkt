#lang racket/base
(require quad/qexpr
         pollen/tag
         quadwriter/core
         racket/string
         racket/list
         racket/dict
         racket/match
         txexpr/base
         "font.rkt")
(provide (all-defined-out))

(define (root attrs exprs)
  (qexpr (append `(#;(first-line-indent "12")
                   #;(line-align "center")
                   #;(line-wrap "kp")
                   (page-margin-left "120")
                   (page-margin-top "80")
                   (page-margin-bottom "120")
                   (line-height "17")
                   #;(line-align-last "center")) attrs) exprs))

(define-tag-function (p attrs exprs)
  ;; no font-family so that it adopts whatever the surrounding family is
  (qexpr (append `((keep-first "2")(keep-last "3") (line-align "left") (font-size-adjust "100%") (character-tracking "0") (hyphenate "true") (display ,(symbol->string (gensym)))) attrs) exprs))

(define-tag-function (hr attrs exprs)
  hrbr)

(define-tag-function (blockquote attrs exprs)
  (qexpr (append '((display "block") 
                   (first-line-indent "0") 
                   (background-color "#eee") 
                   (font-family "fira-sans") (font-size "10") (line-height "14")
                   (border-width-top "0.5") (border-color-top "gray") (border-inset-top "8")
                   (border-width-left "3") (border-color-left "gray") (border-inset-left "20")
                   (border-width-bottom "0.5") (border-color-bottom "gray") (border-inset-bottom "-2")
                   (border-width-right "0.5") (border-color-right "gray") (border-inset-right "20")
                   (inset-top "10") (inset-bottom "8") (inset-left "30") (inset-right "30")
                   (keep-lines "yes"))
                 attrs) exprs))

(define id (default-tag-function 'id))
(define class (default-tag-function 'class))

(define-tag-function (strong attrs exprs)
  (qexpr (list* '(font-bold "true") '(font-size-adjust "100%") attrs) exprs))
(define b strong)

(define-tag-function (a attrs exprs)
  (qexpr `((link ,(cadr (assoc 'href attrs)))(color "MediumVioletRed")) exprs))

(define-tag-function (em attrs exprs)
  (qexpr (list* '(font-italic "true") '(font-size-adjust "100%") attrs) exprs))
(define i em)

(define-syntax-rule (attr-list . attrs) 'attrs)

(define (heading-base font-size attrs exprs)
  (qexpr (append `((font-family "fira-sans-light") (first-line-indent "0") (display "block") (font-size ,(number->string font-size))(line-height ,(number->string (* 1.2 font-size))) (border-width-top "0.5")(border-inset-top "9") (inset-bottom "-3") (inset-top "6") (keep-with-next "true")) attrs) exprs))

(define-tag-function (h1 attrs exprs)
  (heading-base 20 (append '() attrs) exprs))

(define-tag-function (h2 attrs exprs) (heading-base 16 attrs exprs))
(define-tag-function (h3 attrs exprs) (heading-base 14 attrs exprs))

(define h4 h3)
(define h5 h3)
(define h6 h3)

(define-tag-function (code attrs exprs)
  (qexpr (append '((font-family "fira-mono")#;(line-align "right")(font-size "10")(bg "aliceblue")) attrs) exprs))

(define-tag-function (pre attrs exprs)
  ;; pre needs to convert white space to equivalent layout elements
  (define new-exprs (add-between
                     (for*/list ([expr (in-list exprs)]
                                 [str (in-list (string-split (string-join (get-elements expr) "") "\n"))])
                       `(,(get-tag expr) ,(get-attrs expr) ,(string-replace str " " " ")))
                     lbr))
  (qexpr (list* '(display "block") '(background-color "aliceblue")
                '(first-line-indent "0")
                '(font-family "fira-mono") '(font-size "11") '(line-height "14")
                '(border-inset-top "10")
                '(border-width-left "2") '(border-color-left "#669") '(border-inset-left "0")
                '(border-inset-bottom "-4")
                '(inset-left "12")  '(inset-top "12") '(inset-bottom "8")
                attrs) new-exprs))

(define (list-base attrs exprs [bullet-val #f])
  (define bullet-space-factor 2.5)
  (define em (dict-ref attrs 'font-size default-font-size))
  (define bullet-indent (* bullet-space-factor em))
  (qexpr (list* `(inset-left ,(number->string bullet-indent)) attrs)
         (add-between
          (for/list ([(expr idx) (in-indexed exprs)]
                     #:when (txexpr? expr))
            (list* (get-tag expr) (cons (list 'list-index (or bullet-val (format "~a" (add1 idx)))) (get-attrs expr)) (get-elements expr)))
          qexpr-para-break)))

(define-tag-function (ol attrs exprs) (list-base attrs exprs))
(define-tag-function (ul attrs exprs) (list-base attrs exprs "•"))
(define-tag-function (li attrs exprs) (qexpr attrs exprs))