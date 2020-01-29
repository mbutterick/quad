#lang debug racket/base
(require quad/qexpr
         pollen/tag
         quadwriter/core
         racket/string
         racket/list
         racket/dict
         racket/match
         txexpr/base
         "font.rkt"
         "attrs.rkt")
(provide (all-defined-out))

(define (root attrs exprs)
  ;; don't put any attributes in root, so they can be overriden by top-level attributes in source
  (qexpr attrs exprs))

(define-tag-function (p attrs exprs)
  (match exprs
    [(cons (? txexpr? tx) _)
     #:when (equal? (attr-ref tx :font-family #false) "code")
     ;; wrap code blocks in `pre` to distinguish them from inline
     (apply pre attrs exprs)]
    ;; no font-family so that it adopts whatever the surrounding family is
    [_ (qexpr (append (list->attrs
                       :keep-first-lines "2"
                       :keep-last-lines "3"
                       :font-size "100%"
                       :hyphenate "true"
                       :display (symbol->string (gensym)))
                      attrs) exprs)]))

(define-tag-function (img attrs exprs)
  (qexpr (list->attrs
          :line-height "false"
          :image-height "150"
          :line-align-last "center" ; need `last` because img is a block-level element.
          :image-file (second (assq 'src attrs))
          :image-alt (second (assq 'alt attrs))
          :display "block") exprs))

(define-tag-function (br attrs exprs) line-break)

(define-tag-function (hr attrs exprs) hr-break)

(define-tag-function (blockquote attrs exprs)
  (qexpr (append (list->attrs
                  :display "block"
                  :first-line-indent "0" 
                  :background-color "#eee"
                  :block-clip "true"
                  :font-family "blockquote"
                  :font-size "10"
                  :line-height "14"
                  :border-width-top "0.5"
                  :border-color-top "gray"
                  :border-inset-top "8"
                  :border-width-left "3"
                  :border-color-left "gray"
                  :border-inset-left "20"
                  :border-width-bottom "0.5"
                  :border-color-bottom "gray"
                  :border-inset-bottom "-2"
                  :border-width-right "0.5"
                  :border-color-right "gray"
                  :border-inset-right "20"
                  :inset-top "10"
                  :inset-bottom "8"
                  :inset-left "30"
                  :inset-right "30"
                  :keep-all-lines "yes")
                 attrs) exprs))

(define id (default-tag-function 'id))
(define class (default-tag-function 'class))

(define-tag-function (strong attrs exprs)
  (qexpr (append (list->attrs
                  :font-bold "true"
                  :font-size "100%")
                 attrs) exprs))

(define b strong)

(define-tag-function (a attrs exprs)
  (qexpr (list->attrs
          :link (second (assoc :href attrs))
          :font-color "MediumVioletRed") exprs))

(define-tag-function (em attrs exprs)
  (qexpr (append
          (list->attrs
           :font-italic "true"
           :font-size "100%") attrs) exprs))

(define i em)

(define-syntax-rule (attr-list . attrs) 'attrs)

(define (heading-base font-size inset-top attrs exprs)
  (qexpr (append (list->attrs
                  :font-family "heading"
                  :first-line-indent "0"
                  :display "block"
                  :font-size (number->string font-size)
                  :line-height (number->string (* 1.2 font-size))
                  :border-width-top "0.5"
                  :border-inset-top "9"
                  :inset-bottom "-3"
                  :inset-top (number->string inset-top)
                  :keep-with-next "true") attrs) exprs))

(define-tag-function (h1 attrs exprs)
  (qexpr null (list page-break (heading-base 20 6 attrs exprs))))

(define-tag-function (h2 attrs exprs) (heading-base 16 7 attrs exprs))
(define-tag-function (h3 attrs exprs) (heading-base 14 8 attrs exprs))

(define h4 h3)
(define h5 h3)
(define h6 h3)

(define-tag-function (code attrs exprs)
  (qexpr (append (list->attrs
                  :font-family "code"
                  :font-size "10"
                  :bg "aliceblue") attrs) exprs))

(define-tag-function (pre attrs exprs)
  ;; pre needs to convert white space to equivalent layout elements
  (define new-exprs (add-between
                     (for*/list ([expr (in-list exprs)]
                                 [str (in-list (string-split (string-join (get-elements expr) "") "\n"))])
                                (list (get-tag expr) (get-attrs expr) (string-replace str " " " ")))
                     line-break))
  (qexpr (append (list->attrs
                  :display "block"
                  :background-color "aliceblue"
                  :first-line-indent "0"
                  :block-clip "true"
                  :font-family "code"
                  :font-size "11"
                  :line-height "14"
                  :border-inset-top "10"
                  :border-width-left "2"
                  :border-color-left "#669"
                  :border-inset-left "0"
                  :border-inset-bottom "-4"
                  :inset-left "12"
                  :inset-top "12"
                  :inset-bottom "8")
                 attrs) new-exprs))

(define (list-base attrs exprs [bullet-val #f])
  (define bullet-space-factor 2.5)
  (define em (dict-ref attrs :font-size default-font-size))
  (define bullet-indent (* bullet-space-factor em))
  (qexpr (cons (list :inset-left (number->string bullet-indent)) attrs)
         (add-between
          (for/list ([(expr idx) (in-indexed exprs)]
                     #:when (txexpr? expr))
                    (list* (get-tag expr) (cons (list :list-index (or bullet-val (format "~a" (add1 idx)))) (get-attrs expr)) (get-elements expr)))
          para-break)))

(define bullet-quad '(q ((special "bullet"))))

(define-tag-function (ol attrs exprs) (list-base attrs exprs))
(define-tag-function (ul attrs exprs) (list-base attrs exprs "•"))
(define-tag-function (li attrs exprs)
  (define new-bullet-quad (match exprs
                            [(cons (txexpr _ attrs _) _)
                             (match bullet-quad
                               [(txexpr tag battrs elems)
                                (define new-attrs
                                  (hash->attrs
                                   (copy-block-attrs (attrs->hash attrs) (attrs->hash battrs))))
                                (txexpr tag new-attrs elems)])]
                            [_ bullet-quad]))
  (qexpr attrs (cons new-bullet-quad exprs)))