#lang debug racket/base
(require racket/match
         txexpr/base
         racket/contract
         racket/file
         pitfall
         quad
         sugar/coerce
         sugar/debug
         "attrs.rkt"
         "param.rkt"
         "font.rkt"
         "layout.rkt")
(provide (all-defined-out))




(define default-page-size "letter")
(define default-page-orientation "tall")

(define (setup-pdf-path pdf-path-arg)
  (define fallback-path (build-path (find-system-path 'temp-dir) "quadwriter-temp.pdf"))
  (path->complete-path (simplify-path (expand-user-path (->path (or pdf-path-arg fallback-path))))))

(define para-break '(q ((break "para"))))
(define line-break '(q ((break "line"))))
(define page-break '(q ((break "page"))))
(define column-break '(q ((break "column"))))

(define (replace-breaks x)
  (map-elements (λ (el)
                  (match el
                    [(== para-break) pbr]
                    [(== line-break) lbr]
                    [(== column-break) colbr]
                    [(== page-break) pgbr]
                    [_ el])) x))

(define default-line-height-multiplier 1.42)
(define (setup-qs qx-arg pdf-path)
  [define qexpr (replace-breaks qx-arg)]
  [define the-quad
    (qexpr->quad  `(q ((font-family ,default-font-family)
                       (font-size ,(number->string default-font-size))
                       (line-height ,(number->string (floor (* default-line-height-multiplier default-font-size))))) ,qexpr))]
  (setup-font-path-table! pdf-path)
  [define atomized-qs
    (time-name atomize (atomize the-quad #:attrs-proc handle-cascading-attrs
                                #:missing-glyph-action 'fallback
                                #:fallback "fallback"
                                #:emoji "emoji"
                                #:math "math"
                                #:font-path-resolver resolve-font-path))]
  [define hyphenated-qs (time-name hyphenate (handle-hyphenate atomized-qs))]
  [define stringified-qs (map ->string-quad hyphenated-qs)]
  [define indented-qs (insert-first-line-indents stringified-qs)]
  indented-qs)

(define (setup-pdf qs pdf-path)
  ;; page size can be specified by name, or measurements.
  ;; explicit measurements from page-height and page-width supersede those from page-size.
  (match-define (list page-width page-height) (for/list ([k (list :page-width :page-height)])
                                                (match (quad-ref (car qs) k)
                                                  [#false #false]
                                                  [val (parse-dimension val 'round)])))
  ;; `make-pdf` will sort out conflicts among page dimensions
  (make-pdf #:compress #true
            #:auto-first-page #false
            #:output-path pdf-path
            #:width (or (debug-page-width) page-width)
            #:height (or (debug-page-height) page-height)
            #:size (quad-ref (car qs) :page-size default-page-size)
            #:orientation (quad-ref (car qs) :page-orientation default-page-orientation)))

(define (setup-margins qs pdf)
  (define default-side-margin (min (* 72 1.5) (floor (* .20 (pdf-width pdf)))))
  (define default-top-margin (min 72 (floor (* .10 (pdf-height pdf)))))

  ;; if only left or right margin is provided, copy other value in preference to default margin
  (define left
    (or (debug-x-margin)
        (quad-ref (car qs) :page-margin-left
                  (λ () (quad-ref (car qs) :page-margin-right default-side-margin)))))
  (define right
    (or (debug-x-margin)
        (quad-ref (car qs) :page-margin-right
                  (λ () (quad-ref (car qs) :page-margin-left default-side-margin)))))
  (define top
    (or (debug-y-margin)
        (quad-ref (car qs) :page-margin-top
                  (λ () (quad-ref (car qs) :page-margin-bottom default-top-margin)))))
  (define vert-optical-adjustment 10)
  (define bottom
    (or (debug-y-margin)
        (quad-ref (car qs) :page-margin-bottom
                  (λ () (+ vert-optical-adjustment (quad-ref (car qs) :page-margin-top (* default-top-margin 1.4)))))))
  (list left top right bottom))

(define default-column-count 1)
(define (setup-column-count qs)
  (define cc (or (debug-column-count) (quad-ref (car qs) :column-count default-column-count)))
  (unless (exact-nonnegative-integer? cc)
    (raise-argument-error 'render-pdf "positive integer" cc))
  cc)

(define default-column-gap 36)
(define (setup-column-gap qs)
  (or (debug-column-gap) (quad-ref (car qs) :column-gap default-column-gap)))


(define/contract (render-pdf qx-arg pdf-path-arg #:replace [replace? #t])
  ((qexpr? (or/c #false path? path-string?)) (#:replace any/c) . ->* . (or/c void? bytes?))

  (define pdf-path (setup-pdf-path pdf-path-arg))
  (when (and (not replace?) (file-exists? pdf-path))
    (raise-argument-error 'render-pdf "path that doesn't exist" pdf-path))
  
  (define qs (setup-qs qx-arg pdf-path))

  (parameterize ([current-pdf (setup-pdf qs pdf-path)]
                 [verbose-quad-printing? #false])
    (match-define (list left-margin top-margin right-margin bottom-margin) (setup-margins qs (current-pdf)))
    (define printable-width (- (pdf-width (current-pdf)) left-margin right-margin))
    (define printable-height (- (pdf-height (current-pdf)) top-margin bottom-margin))
    (define column-count (setup-column-count qs))
    (define column-gap (setup-column-gap qs))
    
    (define line-wrap-size (/ (- printable-width (* (sub1 column-count) column-gap)) column-count))
    (define line-qs (time-name line-wrap (apply-keeps (line-wrap qs line-wrap-size))))
    
    (define col-quad-prototype (struct-copy quad q:column
                                            [size (pt line-wrap-size printable-height)]))
    (define column-qs (time-name col-wrap (col-wrap line-qs printable-height column-gap col-quad-prototype)))
    
    (define page-quad-prototype (struct-copy quad q:page
                                             [shift (pt left-margin top-margin)]
                                             [size (pt line-wrap-size printable-height)]))
    (define page-qs (time-name page-wrap (page-wrap column-qs printable-width page-quad-prototype)))
    
    (define positioned-qs (time-name position (position (struct-copy quad q:doc [elems page-qs]))))
    (time-name draw (draw positioned-qs (current-pdf))))

  (if pdf-path-arg
      (displayln (format "wrote PDF to ~a" pdf-path))
      (begin0
        (file->bytes pdf-path)
        (delete-file pdf-path))))