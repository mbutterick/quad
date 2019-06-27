#lang debug racket/base
(require (for-syntax racket/base racket/syntax syntax/strip-context)
         racket/match
         racket/sequence
         racket/string)
(provide (all-defined-out))

(define (list->attrs . kvs)
  (for/list ([kv (in-slice 2 kvs)])
            kv))

(define (cm->in x) (/ x 2.54))
(define (in->pts x) (* 72 x))
(define (mm->cm x) (/ x 10))

(define (parse-dimension x)
  (match x
    [#false #false]
    [(? number?) x]
    [(? string? x)
     (match (cdr (regexp-match #rx"^(-?[0-9\\.]+)([a-z]+)$"  (string-downcase x)))
       [(list num-string unit)
        ((match unit
           [(regexp #rx"in(ch(es)?)?$") in->pts]
           [(regexp #rx"cm$") (compose1 in->pts cm->in)]
           [(regexp #rx"mm$") (compose1 in->pts cm->in mm->cm)]
           [_ (raise-argument-error 'parse-dimension "dimension string" x)]) (string->number num-string))])]))

(define (copy-block-attrs source-hash dest-hash)
  (define new-hash (make-hasheq))
  (for ([(k v) (in-hash dest-hash)])
       (hash-set! new-hash k v))
  (for* ([k (in-list block-attrs)]
         [v (in-value (hash-ref source-hash k #f))]
         #:when v)
        (hash-set! new-hash k v))
  new-hash)

(define-syntax (define-attrs stx)
  (syntax-case stx ()
    [(_ (ATTR-NAME ...))
     (with-syntax ([(ATTR-ID ...) (for/list ([attr-id (in-list (syntax->list #'(ATTR-NAME ...)))])
                                            (format-id stx ":~a" (syntax-e attr-id)))])
       #'(begin
           (define ATTR-ID 'ATTR-NAME) ...))]
    [(_ ID (ATTR-NAME ...))
     (replace-context stx
                      #'(begin
                          (define ID (list 'ATTR-NAME ...))
                          (define-attrs (ATTR-NAME ...))))]))


#|
Naming guidelines
+ shorter is better
+ general to specific: border-color-left, not border-left-color or left-border-color
+ don't refer to specific output format, e.g. PDF or HTML
+ consistency with CSS style property names is OK if the concept is mostly the same, but usually it's not
+ default value for any missing attr is #false
+ measurement units are points by default

|#


(define-attrs (font-family
               font-path
               font-size
               font-size-adjust
               font-color
               font-features
               font-features-adjust
               font-italic
               font-bold
               font-tracking
               font-baseline-shift
               bg
               link
               href
               line-height
               hyphenate
               list-index
               no-colbr
               no-pbr
               page-number
               doc-title))
  

(define-attrs block-attrs (display
                           ;; inset values increase the layout size of the quad.
                           ;; they are relative to the natural layout box.
                           inset-top
                           inset-bottom
                           inset-left
                           inset-right
                           ;; border-inset values do not increase the layout size of the quad.
                           ;; they are relative to the layout size of the quad, with inset values included.
                           ;; this is different from CSS, where margin + padding increase the size of the layout.
                           ;; one has to be dependent on the other, so a choice must be made.
                           ;; I find this approach more sensible because
                           ;; borders are a styling element, not a layout element.
                           ;; this means that changing the inset values will change the position of the border.
                           ;; but this is preferable to the CSS model, where moving the border changes the layout.
                           ;; principle: minimize the number of values that affect the layout,
                           ;; so it's easier to reason about programmatically.
                           border-inset-top
                           border-inset-bottom
                           border-inset-left
                           border-inset-right
                           border-width-left
                           border-width-right
                           border-width-top
                           border-width-bottom
                           border-color-left
                           border-color-right
                           border-color-top
                           border-color-bottom
                           background-color

                           block-clip ; whether box boundary clips its contents

                           space-after ; space after a block, like a paragraph
                           space-before
                           
                           column-count
                           column-gap
                      
                           keep-first-lines
                           keep-last-lines
                           keep-all-lines

                           keep-with-next

                           line-align
                           line-align-last

                           first-line-indent

                           line-wrap

                           image-file
                           image-object
                           image-alt
                           image-height
                           image-width

                           page-width
                           page-height
                           page-size ; e.g., "letter"
                           page-orientation ; only applies to page-size dimensions
                           page-number-start ; 1

                           page-margin-top
                           page-margin-bottom
                           page-margin-left
                           page-margin-right

                           footer-display))

(define (takes-dimension-string? k)
  (and (memq k (list :page-width
                     :page-height
                     :page-margin-top
                     :page-margin-bottom
                     :page-margin-left
                     :page-margin-right
                     :column-gap
                     :inset-top
                     :inset-bottom
                     :inset-left
                     :inset-right
                     :border-inset-top
                     :border-inset-bottom
                     :border-inset-left
                     :border-inset-right
                     :border-width-left
                     :border-width-right
                     :border-width-top
                     :border-width-bottom
                     :space-before
                     :space-after
                     :image-height
                     :image-width
                     :font-size
                     :font-tracking
                     :font-baseline-shift
                     :line-height)) #true))