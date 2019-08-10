#lang debug racket/base
(require (for-syntax racket/base racket/syntax)
         racket/match
         txexpr/base
         racket/contract
         racket/file
         racket/string
         racket/sequence
         racket/list
         pitfall
         quad
         hyphenate
         pollen/unstable/typography
         pollen/decode
         sugar/coerce
         sugar/debug
         sugar/list
         "attrs.rkt"
         "param.rkt"
         "font.rkt"
         "layout.rkt"
         "log.rkt")
(provide (all-defined-out))



(define (setup-pdf-path pdf-path-arg)
  (define fallback-path (build-path (find-system-path 'temp-dir) "quadwriter-temp.pdf"))
  (path->complete-path (simplify-path (expand-user-path (->path (or pdf-path-arg fallback-path))))))


(define-syntax (define-break-types stx)
  (syntax-case stx ()
    [(_ ALL-BREAKS-ID . TYPES)
     (with-syntax ([((TYPE-BREAK TYPE-STR Q:TYPE-BREAK) ...)
                    (for/list ([type (in-list (syntax->list #'TYPES))])
                      (list
                       (format-id #'TYPES "~a-break" type)
                       (symbol->string (syntax->datum type))
                       (format-id #'TYPES "q:~a-break" type)))])
       #'(begin
           (define TYPE-BREAK '(q ((break TYPE-STR)))) ...
           (define ALL-BREAKS-ID (list (cons TYPE-BREAK Q:TYPE-BREAK) ...))))]))

(define-break-types all-breaks para line page column hr section)

(define (replace-breaks x)
  (map-elements (λ (el)
                  (cond
                    [(assoc el all-breaks) => cdr]
                    [else el])) x))


(define (handle-hyphenate qs)
  ;; find quads that want hyphenation and split them into smaller pieces
  ;; do this before ->string-quad so that it can handle the sizing promises
  (apply append
         (for/list ([q (in-list qs)])
           (match (quad-ref q :hyphenate)
             [#true #:when (and (pair? (quad-elems q))
                                (andmap string? (quad-elems q)))
                    (for*/list ([str (in-list (quad-elems q))]
                                [hyphen-char (in-value #\u00AD)]
                                [hstr (in-value (hyphenate str hyphen-char
                                                           #:min-left-length 3
                                                           #:min-right-length 3))]
                                [substr (in-list (regexp-match* (regexp (string hyphen-char)) hstr #:gap-select? #t))])
                      (struct-copy quad q [elems (list substr)]))]
             [_ (list q)]))))


(define (string->feature-list str)
  (for/list ([kv (in-slice 2 (string-split str))])
    (cons (string->bytes/utf-8 (first kv)) (string->number (second kv)))))

(define (parse-font-features! attrs)
  (cond
    [(match (hash-ref attrs :font-features-adjust #f)
       [(? string? str)
        ;; override any existing features
        (define parsed-features (string->feature-list str))
        (hash-update! attrs :font-features (λ (fs) (remove-duplicates (append parsed-features fs) equal? #:key car)))
        ;; adjustment is incorporated, so delete it
        (hash-set! attrs :font-features-adjust #false)]
       [_ #false])]
    [else (match (hash-ref attrs :font-features #f)
            [(? string? str)
             (define parsed-features (string->feature-list str))
             (hash-set! attrs :font-features parsed-features)]
            [_ #false])]))


(define (parse-dimension-strings! attrs)
  (for ([k (in-hash-keys attrs)]
        #:when (takes-dimension-string? k))
    (hash-update! attrs k parse-dimension))
  attrs)

(define (complete-every-path! attrs)
  ;; relies on `current-directory` being parameterized to source file's dir
  (for ([k (in-hash-keys attrs)]
        #:when (takes-path? k))
    (hash-update! attrs k (compose1 path->string path->complete-path)))
  attrs)

(define (handle-cascading-attrs attrs)
  (parse-dimension-strings! attrs)
  (complete-every-path! attrs)
  (resolve-font-path! attrs)
  (resolve-font-size! attrs)
  (parse-font-features! attrs))

(define default-line-height-multiplier 1.42)
(define (setup-qs qx-arg pdf-path)
  (define qexpr (decode qx-arg
                        #:string-proc (λ (str) (smart-ellipses (smart-dashes str)))
                        #:txexpr-proc smart-quotes))
  (define super-qexpr (replace-breaks qexpr))
  (define the-quad
    (qexpr->quad (list 'q (list->attrs
                           :font-family default-font-family
                           :font-size (number->string default-font-size)
                           :line-height (number->string (floor (* default-line-height-multiplier default-font-size)))) super-qexpr)))
  (setup-font-path-table! pdf-path)
  (define atomized-qs (atomize the-quad
                               #:attrs-proc handle-cascading-attrs
                               #:missing-glyph-action 'fallback
                               #:fallback "fallback"
                               #:emoji "fallback-emoji"
                               #:math "fallback-math"
                               #:font-path-resolver resolve-font-path!))
  (define hyphenated-qs (time-log hyphenate (handle-hyphenate atomized-qs)))
  (define typed-quads (map generic->typed-quad hyphenated-qs))
  (define indented-qs (insert-first-line-indents typed-quads))
  indented-qs)

(define (setup-margins qs page-width page-height)
  (define default-side-margin (min (* 72 1.5) (floor (* .20 page-width))))
  (define default-top-margin (min 72 (floor (* .10 page-height))))

  ;; if only left or right margin is provided, copy other value in preference to default margin
  (define left
    (or (debug-x-margin)
        (quad-ref (car qs) :page-margin-left
                  (λ ()  (quad-ref (car qs) :page-margin-right default-side-margin)))))
  (define right
    (or (debug-x-margin)
        (quad-ref (car qs) :page-margin-right
                  (λ ()  (quad-ref (car qs) :page-margin-left default-side-margin)))))
  (define top
    (or (debug-y-margin)
        (quad-ref (car qs) :page-margin-top
                  (λ ()  (quad-ref (car qs) :page-margin-bottom default-top-margin)))))
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
  (or (debug-column-gap)  (quad-ref (car qs) :column-gap default-column-gap)))

(define/contract (render-pdf qx-arg pdf-path-arg
                             [base-dir-arg #false]
                             #:replace [replace-existing-file? #t]
                             #:compress [compress? #t])
  ((qexpr? (or/c #false path? path-string?))
   ((or/c #false path? path-string?)
    #:replace any/c
    #:compress any/c) . ->* . (or/c void? bytes?))

  (define base-dir (match base-dir-arg
                     [#false (current-directory)]
                     [path (define-values (dir name _) (split-path path))
                           dir]))

  (define pdf-path (setup-pdf-path pdf-path-arg))
  (unless replace-existing-file?
    (when (file-exists? pdf-path)
      (raise-argument-error 'render-pdf "path that doesn't exist" pdf-path)))

  (define the-pdf (make-pdf #:compress compress?
                            #:auto-first-page #false
                            #:output-path pdf-path))
  (parameterize ([current-pdf the-pdf]
                 ;; set `current-directory` so that ops like `path->complete-path`
                 ;; will be handled relative to the original directory
                 [current-directory base-dir]
                 [current-page-count 0]
                 [verbose-quad-printing? #false])
    (define qs (time-log setup-qs (setup-qs qx-arg pdf-path)))
    (define sections
      (for/list ([qs (in-list (filter-split qs section-break-quad?))])
        (match-define (list page-width page-height) (parse-page-size (and (pair? qs) (car qs))))
        (match-define (list left-margin top-margin right-margin bottom-margin)
          (setup-margins qs page-width page-height))
        (define printable-width (- page-width left-margin right-margin))
        (define printable-height (- page-height top-margin bottom-margin))
        (define column-count (setup-column-count qs))
        (define column-gap (setup-column-gap qs))
    
        (define line-wrap-size (/ (- printable-width (* (sub1 column-count) column-gap)) column-count))
        (define line-qs (time-log line-wrap (apply-keeps (line-wrap qs line-wrap-size))))
    
        (define col-quad-prototype (struct-copy quad q:column
                                                [size (pt line-wrap-size printable-height)]))
        (define column-qs (time-log column-wrap (column-wrap line-qs printable-height column-gap col-quad-prototype)))
    
        (define page-quad-prototype (struct-copy quad q:page
                                                 [shift (pt left-margin top-margin)]
                                                 [size (pt line-wrap-size printable-height)]))

        (define next-page-side (if (even? (add1 (current-page-count))) 'left 'right))
        (define insert-blank-page?
          (and (pair? qs)
               (let ([section-starting-side (string->symbol (quad-ref (car qs) :page-side-start "right"))])
                 ;; if we need a 'left page and will get 'right (or vice versa) then insert page
                 (not (eq? section-starting-side next-page-side)))))
        
        (define page-qs
          (match (time-log page-wrap (page-wrap column-qs printable-width page-quad-prototype))
            [ps #:when insert-blank-page?
                (define blank-page (struct-copy quad (car ps) [elems null]))
                (cons blank-page ps)]
            [ps ps]))
        
        (begin0        
          (struct-copy quad q:section [elems page-qs])
          (current-page-count (+ (current-page-count) (length page-qs))))))

    (define doc (time-log position (position (struct-copy quad q:doc [elems sections]))))
    (time-log draw (draw doc (current-pdf))))

  (if pdf-path-arg
      (log-quadwriter-info (format "wrote PDF to ~a" pdf-path))
      (begin0
        (file->bytes pdf-path)
        (delete-file pdf-path))))
