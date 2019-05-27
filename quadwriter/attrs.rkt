#lang debug racket/base
(require (for-syntax racket/base racket/syntax)
         racket/match
         racket/string)
(provide (all-defined-out))
#|
Naming guidelines
+ shorter is better
+ general to specific: border-color-left, not border-left-color or left-border-color
+ don't refer to specific output format, e.g. PDF or HTML
+ consistency with CSS style property names is OK if the concept is mostly the same, but usually it's not
+ default value for any missing attr is #false
+ measurement units are points by default

|#

(define (cm->in x) (/ x 2.54))
(define (in->pts x) (* 72 x))
(define (mm->cm x) (/ x 10))

(define (parse-dimension x [round? #f])
  (define val
    (match x
      [(? number?) x]
      [(? string? x)
       (match (cdr (regexp-match #rx"^(-?[0-9\\.]+)([a-z]+)$"  (string-downcase x)))
         [(list num-string unit)
          ((match unit
             [(regexp #rx"in(ch(es)?)?$") in->pts]
             [(regexp #rx"cm$") (compose1 in->pts cm->in)]
             [(regexp #rx"mm$") (compose1 in->pts cm->in mm->cm)]
             [_ (raise-argument-error 'parse-dimension "dimension string" x)]) (string->number num-string))])]))
  (if round? (inexact->exact (floor val)) val))

(define (copy-block-attrs source-hash dest-hash)
  (define new-hash (make-hasheq))
  (for ([(k v) (in-hash dest-hash)])
    (hash-set! new-hash k v))
  (for* ([k (in-list block-attrs)]
         [v (in-value (hash-ref source-hash k #f))]
         #:when v)
    (hash-set! new-hash k v))
  new-hash)

(define :font-size 'font-size)
(define :font-color 'font-color)
(define :character-tracking 'character-tracking)
(define :bg 'bg)
(define :link 'link)
(define :line-height 'line-height)
(define :hyphenate 'hyphenate)
(define :list-index 'list-index)
(define :no-colbr 'no-colbr)
(define :no-pbr 'no-pbr)
(define :page-number 'page-number)
(define :doc-title 'doc-title)

(define-syntax (define-attrs stx)
  (syntax-case stx ()
    [(_ ID (ATTR-NAME ...))
     (with-syntax ([(ATTR-ID ...) (for/list ([attr-id (in-list (syntax->list #'(ATTR-NAME ...)))])
                                    (format-id #'ID ":~a" (syntax-e attr-id)))])
     #'(begin
         (define ATTR-ID 'ATTR-NAME) ...
         (define ID (list ATTR-ID ...))))]))
  

(define-attrs block-attrs (
                           display
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

                           page-width
                           page-height
                           page-size ; e.g., "letter"
                           page-orientation ; only applies to page-size dimensions

                           page-margin-top
                           page-margin-bottom
                           page-margin-left
                           page-margin-right

                           footer-display
                           ))