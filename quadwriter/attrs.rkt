#lang debug racket/base
(require (for-syntax racket/base racket/syntax syntax/strip-context)
         racket/match
         racket/sequence
         racket/string)
(provide (all-defined-out))

(define (list->attrs . kvs)
  (for/list ([kv (in-slice 2 kvs)])
    kv))

(define (pica->pts prefix [suffix #false])
  ;; both pieces of measurement are either positive or negative
  ((if (negative? prefix) - +) (+ (* (abs prefix) 12) (or suffix 0))))
(define (cm->in x) (/ x 2.54))
(define (in->pts x) (* 72 x))
(define (mm->cm x) (/ x 10.0))

(define (parse-dimension x [em-resolution-attrs #false])
  (define pica-pat (regexp "^(p|pica)(s)?$"))
  (define (unit->converter-proc unit)
    (match unit
      [(regexp #rx"^(pt|point)(s)?$") values] ; points
      [(regexp pica-pat) pica->pts] ; pica (no pts)
      [(regexp #rx"^inch(es)?|in(s)?$") in->pts] ; inches
      [(regexp #rx"^cm(s)?$") (compose1 in->pts cm->in)] ; cm
      [(regexp #rx"^mm(s)?$") (compose1 in->pts cm->in mm->cm)] ; mm
      [(regexp #rx"^em(s)?$")
       #:when em-resolution-attrs
       ;; if we don't have attrs for resolving the em string, we ignore it
       (λ (num) (* (hash-ref em-resolution-attrs :font-size) num))]
      [_ #false]))
  (define parsed-thing
    (match x
      [#false #false]
      [(? number? num) num]
      [(? string? str)
       (match (regexp-match #px"^(-?[0-9\\.]+)\\s*([a-z]+)([0-9\\.]+)?$" (string-downcase str))
         [(list str
                (app string->number num)
                (app unit->converter-proc converter-proc)
                #false) ; prefix measurement (suffix is #false)
          #:when (and converter-proc num)
          (converter-proc num)]
         [(list str
                (app string->number prefix-num)
                (and (regexp pica-pat) unit)
                (app string->number suffix-num))
          #:when (and prefix-num suffix-num) ; prefix + suffix measurement (only pica + point)
          (pica->pts prefix-num suffix-num)]
         [_ str])]))
  (match parsed-thing
    [(and (? integer?) (? inexact?)) (inexact->exact parsed-thing)]
    [_ parsed-thing]))

(module+ test
  (require rackunit)
  (check-equal? (parse-dimension "Foobar") "Foobar")
  (check-equal? (parse-dimension "72plink") "72plink")
  (check-equal? (parse-dimension "7..2pt") "7..2pt")
  (check-equal? (parse-dimension "72pt") 72)
  (check-equal? (parse-dimension "72.5pts") 72.5)
  (check-equal? (parse-dimension "-72point") -72)
  (check-equal? (parse-dimension "-72.5points") -72.5)
  (check-equal? (parse-dimension "72.5points3") "72.5points3")
  (check-equal? (parse-dimension "2in") 144)
  (check-equal? (parse-dimension "-2ins") -144)
  (check-equal? (parse-dimension "2.5inch") 180)
  (check-equal? (parse-dimension "-2.5inches") -180)
  (check-equal? (parse-dimension "2.54cm") (parse-dimension "1in"))
  (check-equal? (parse-dimension "-2.54cms") (parse-dimension "-1in"))
  (check-equal? (parse-dimension "25.4mm") (parse-dimension "1in"))
  (check-equal? (parse-dimension "-25.4mms") (parse-dimension "-1in"))
  (check-equal? (parse-dimension "2.5em" (hash :font-size 12)) 30)
  (check-equal? (parse-dimension "-2.5em" (hash :font-size 12)) -30)
  (check-equal? (parse-dimension "25.4em") "25.4em")
  (check-equal? (parse-dimension "-25.4ems") "-25.4ems")
  (check-equal? (parse-dimension "6pica3") 75)
  (check-equal? (parse-dimension "6.5picas3.5") 81.5)
  (check-equal? (parse-dimension "12p") 144)
  (check-equal? (parse-dimension "-6p3") -75)
  (check-equal? (parse-dimension "-6.5ps3.5") -81.5))

(define (copy-block-attrs source-hash dest-hash)
  (define new-hash (make-hasheq))
  (for ([(k v) (in-hash dest-hash)])
    (hash-set! new-hash k v))
  (for ([(k v) (in-hash source-hash)]
        #:when (memq k block-attrs))
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
               font-color
               font-features
               font-features-adjust
               font-italic
               font-bold
               font-underline
               font-tracking
               font-baseline-shift
               font-case
               bg
               link
               href
               line-height
               hyphenate
               list-index
               no-colbr
               no-pbr
               page-number
               doc-title

               pdf-title
               pdf-subject
               pdf-author
               pdf-keywords

               break

               draw
               width
               height
               parent
               anchor-from
               anchor-from-parent
               anchor-to
               string
               color
               stroke
               x
               y
               x1 ;; x & y are the top-left pos of the bounding box.
               y1 ;; x1 & y1 are the starting points of a line
               x2
               y2))
  

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

                           keep-with-next

                           line-align

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
                           page-side-start ; 'right

                           page-margin-top
                           page-margin-bottom
                           page-margin-left
                           page-margin-right
                           page-margin-gutter

                           footer-display
                           footer-text

                           ;; we want this distinct from anchor-parent
                           ;; because the two directives may overlap / contradict.
                           ;; for instance: repeat on every page,
                           ;; a quad that prints on the previous page on the second line.
                           repeat

                           draw-debug

                           fn-ref
                           fn-text
                           fn-text-start))

(define (takes-dimension-string? k)
  (and (memq k (list :page-width
                     :page-height
                     :page-margin-top
                     :page-margin-bottom
                     :page-margin-left
                     :page-margin-right
                     :page-margin-gutter
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
                     :line-height
                     :x
                     :y
                     :x1
                     :x2
                     :y1
                     :y2
                     :stroke
                     :width)) #true))

(define (has-case-sensitive-value? k)
  (and (memq k (list :pdf-title
                     :pdf-subject
                     :pdf-author
                     :pdf-keywords
                     :string
                     :footer-text)) #true))

(define (takes-path? k)
  (and (memq k (list :image-file)) #true))
