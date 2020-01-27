#lang debug racket/base
(require racket/match
         racket/contract
         racket/file
         racket/string
         racket/sequence
         racket/list
         racket/dict
         pitfall
         quad
         hyphenate
         pollen/decode
         sugar/coerce
         sugar/list
         "attrs.rkt"
         "param.rkt"
         "font.rkt"
         "layout.rkt"
         "log.rkt")
(provide (all-defined-out))



(define (setup-pdf-path pdf-path-arg)
  ;; convert pathlike arg into nice complete path.
  (path->complete-path
   (simplify-path
    (expand-user-path
     (->path
      (match pdf-path-arg
        ;; a #false arg signals that we're going to return the bytes directly,
        ;; so we use a temp path to write the file, and we'll delete it later.
        [#false (build-path (find-system-path 'temp-dir) "quadwriter-temp.pdf")]
        [path path]))))))


(define (handle-hyphenate q)
  ;; find quads that want hyphenation and split them into smaller pieces
  ;; do this before ->string-quad so that it can handle the sizing promises
  (cond
    [(and (quad-ref q :hyphenate) (pair? (quad-elems q)) (andmap string? (quad-elems q)))
     (for*/list ([str (in-list (quad-elems q))]
                 [hyphen-char (in-value #\u00AD)]
                 [hstr (in-value (hyphenate str hyphen-char
                                            #:min-left-length 3
                                            #:min-right-length 3))]
                 [substr (in-list (regexp-match* (regexp (string hyphen-char)) hstr #:gap-select? #t))])
                (quad-copy q [elems (list substr)]))]
    [else (list q)]))


(define (string->feature-list str)
  (for/list ([kv (in-slice 2 (string-split str))])
            (cons (string->bytes/utf-8 (first kv)) (string->number (second kv)))))

(define (parse-font-features! attrs)
  ;; `font-features` are OpenType font feature specifiers.
  (match (hash-ref attrs :font-features-adjust #false)
    [(? string? str)
     ;; adjustment: parse the feature string and append to the current feature set
     (define parsed-features (string->feature-list str))
     (hash-update! attrs :font-features (λ (fs) (remove-duplicates (append parsed-features fs) equal? #:key car)))
     ;; once adjustment is incorporated, delete it
     (hash-set! attrs :font-features-adjust #false)]
    [_ (match (hash-ref attrs :font-features #false)
         ;; override: parse features & replace current set
         [(? string? str)
          (define parsed-features (string->feature-list str))
          (hash-set! attrs :font-features parsed-features)]
         [_ (void)])]))


(define (parse-dimension-strings! attrs)
  ;; certain attributes can be "dimension strings", which are strings like "3in" or "4.2cm"
  ;; we parse them into the equivalent measurement in points.
  (for ([k (in-hash-keys attrs)]
        #:when (takes-dimension-string? k))
       (hash-update! attrs k parse-dimension))
  attrs)

(define (downcase-values! attrs)
  ;; make attribute values lowercase, unless they're case-sensitive
  ;; so we can check them more easily later.
  (for ([k (in-hash-keys attrs)]
        #:unless (has-case-sensitive-value? k))
       (hash-update! attrs k (λ (val) (match val
                                        [(? string? str) (string-downcase str)]
                                        [_ val]))))
  attrs)

(define (complete-every-path! attrs)
  ;; convert every pathlike thing to a complete path (string, because it's inside an attr)
  ;; so we don't get tripped up later by relative paths
  ;; relies on `current-directory` being parameterized to source file's dir
  (for ([k (in-hash-keys attrs)]
        #:when (takes-path? k))
       (hash-update! attrs k (compose1 path->string path->complete-path)))
  attrs)

(define (handle-cascading-attrs attrs)
  ;; various housekeeping on attributes as they are propagated downward during atomization.
  (for ([proc (in-list (list downcase-values!
                             parse-dimension-strings!
                             complete-every-path!
                             resolve-font-path!
                             resolve-font-size!
                             resolve-font-tracking!
                             resolve-line-height!
                             parse-font-features!))])
       (proc attrs)))

(define (drop-leading-breaks qs)
  ;; any leading breaks are pointless at the start of the doc, so drop them.
  ;; How would we get these?
  ;; we might, for instance, have a first-level heading style that is specified with page break before.
  ;; so if we invoke that style first, we will get a page break.
  (dropf qs break-quad?))

(define default-line-height-multiplier 1.42)
(define (setup-qs qx-arg base-dir)
  ;; convert our input Q-expression into a useful form.

  ;; some typographic niceties
  (define qexpr (decode qx-arg
                        #:string-proc (compose1 smart-ellipses smart-dashes)
                        #:txexpr-proc smart-quotes))

  ;; apply some default styling attributes.
  ;; These will only be used if the underlying q-expression hasn't specified its own values,
  ;; which will naturally override these.
  (define q
    (qexpr->quad (list 'q (list->attrs
                           :font-family default-font-family
                           :font-size (number->string default-font-size)
                           :line-height (number->string (floor (* default-line-height-multiplier default-font-size)))) qexpr)))
  (setup-font-path-table! base-dir)
  (let* ([qs (atomize q
                      #:attrs-proc handle-cascading-attrs
                      #:missing-glyph-action 'fallback
                      #:fallback "fallback"
                      #:emoji "fallback-emoji"
                      #:math "fallback-math"
                      #:font-path-resolver resolve-font-path!)]
         [qs (time-log hyphenate (apply append (map handle-hyphenate qs)))]
         [qs (map generic->typed-quad qs)]
         [qs (drop-leading-breaks qs)]
         [qs (insert-first-line-indents qs)])
    qs))

(define (setup-margins qs page-width page-height)
  ;; if only left or right margin is provided, copy other value in preference to default margin
  (define q (car qs))
  (define default-side-margin (min (* 72 1.5) (floor (* .20 page-width))))
  (define default-top-margin (min 72 (floor (* .10 page-height))))
  
  (define left (cond
                 [(debug-x-margin)]
                 [(quad-ref q :page-margin-left)]
                 [(quad-ref q :page-margin-right)]
                 [else default-side-margin]))
  
  (define right (cond
                  [(debug-x-margin)]
                  [(quad-ref q :page-margin-right)]
                  [(quad-ref q :page-margin-left)]
                  [else default-side-margin]))
  
  (define top (cond
                [(debug-y-margin)]
                [(quad-ref q :page-margin-top)]
                [(quad-ref q :page-margin-bottom)]
                [else default-top-margin]))
  
  (define bottom (cond
                   [(debug-y-margin)]
                   [(quad-ref q :page-margin-bottom)]
                   [else
                    (define vert-optical-adjustment 10)
                    (+ vert-optical-adjustment
                       (cond
                         [(quad-ref q :page-margin-top)]
                         [else (* default-top-margin 1.4)]))]))
  
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

(define (setup-pdf-metadata! qs pdf)
  (define kv-dict
    (cons
     (cons 'Creator (format "Racket ~a [Quad library]" (version)))
     (for*/list ([(k pdf-k) (in-dict (list (cons :pdf-title 'Title)
                                           (cons :pdf-author 'Author)
                                           (cons :pdf-subject 'Subject)
                                           (cons :pdf-keywords 'Keywords)))]
                 [str (in-value (and (pair? qs) (quad-ref (car qs) k)))]
                 #:when str)
                (cons pdf-k str))))
  (for ([(k v) (in-dict kv-dict)])
       (hash-set! (pdf-info pdf) k v)))

(define (footnote-flow? q) (equal? (quad-ref q 'flow) "footnote"))

(define (make-lines qs line-wrap-size)
  (define-values (fn-qs main-qs) (partition footnote-flow? qs))
  (define line-qs (time-log line-wrap (apply-keeps (line-wrap main-qs line-wrap-size))))
  (define fn-line-qs (time-log fn-line-wrap (apply-keeps (line-wrap fn-qs line-wrap-size))))
  (values line-qs fn-line-qs))

(define (make-columns line-qs fn-line-qs line-wrap-size printable-height column-gap)
  (define col-quad-prototype (quad-copy q:column
                                        [size (pt line-wrap-size printable-height)]))
  (time-log column-wrap (column-wrap line-qs fn-line-qs printable-height column-gap col-quad-prototype)))

(define (make-pages column-qs
                    left-margin
                    top-margin
                    gutter-margin
                    line-wrap-size
                    printable-width
                    printable-height)
  (define (page-quad-prototype page-count)
    (define left-shift (+ left-margin (if (odd? page-count) gutter-margin 0)))
    (quad-copy q:page
               [shift (pt left-shift top-margin)]
               [size (pt line-wrap-size printable-height)]))
  (time-log page-wrap (page-wrap column-qs printable-width page-quad-prototype)))

(define (make-sections qs)
  (for/fold ([sections-acc null]
             #:result (reverse sections-acc))
            ([qs (in-list (filter-split qs section-break-quad?))])
    ;; section properties
    (match-define (list page-width page-height) (parse-page-size (and (pair? qs) (car qs))))
    (match-define (list left-margin top-margin right-margin bottom-margin)
      (setup-margins qs page-width page-height))
    (define gutter-margin (and (pair? qs) (quad-ref (car qs) :page-margin-gutter 0)))
    (define printable-width (- page-width left-margin right-margin gutter-margin))
    (define printable-height (- page-height top-margin bottom-margin))
    (define column-count (setup-column-count qs))
    (define column-gap (setup-column-gap qs))
    (define line-wrap-size (/ (- printable-width (* (sub1 column-count) column-gap)) column-count))

    ;; layout actions
    (define-values (line-qs fn-line-qs) (make-lines qs line-wrap-size))
    (define column-qs (make-columns line-qs fn-line-qs line-wrap-size printable-height column-gap))
    (define section-starting-side (string->symbol (quad-ref (car qs) :page-side-start "right")))
    (define insert-blank-page?
      (and (pair? qs)
           ;; if we need a 'left page and will get 'right (or vice versa) then insert page
           (let ([next-page-side (if (even? (add1 (section-pages-used))) 'left 'right)])
             (not (eq? section-starting-side next-page-side)))))

    ;; update page count before starting page wrap
    (when insert-blank-page?
      (section-pages-used (add1 (section-pages-used))))
    
    (define section-pages (make-pages column-qs
                                      left-margin
                                      top-margin
                                      gutter-margin
                                      line-wrap-size
                                      printable-width
                                      printable-height))
        
    (begin0
      (cond
        ;; ignore empty section
        [(zero? (length section-pages)) sections-acc]
        [insert-blank-page?
         (match section-starting-side
           ['left
            ;; blank page goes at beginning of current section
            (define page-from-current-section (car section-pages))
            (define blank-page (quad-copy page-from-current-section [elems null]))
            (define new-section (quad-copy q:section [elems (cons blank-page section-pages)]))
            (cons new-section sections-acc)]
           [_ ;; must be 'right
            ;; blank page goes at end of previous section (if it exists)
            (define new-section (quad-copy q:section [elems section-pages]))
            (match sections-acc
              [(cons previous-section other-sections)
               (define previous-section-pages (quad-elems previous-section))
               ;; we know previous section has pages because we ignore empty sections
               (define page-from-previous-section (car previous-section-pages))
               (define blank-page (quad-copy page-from-previous-section [elems null]))
               (define updated-previous-section
                 (quad-update! previous-section
                               [elems (append previous-section-pages (list blank-page))]))
               (list* new-section updated-previous-section other-sections)]
              [_ (list new-section)])])]
        [else (define new-section (quad-copy q:section [elems section-pages]) )
              (cons new-section sections-acc)])
      (section-pages-used (+ (section-pages-used) (length section-pages))))))

(define (correct-line-alignment doc)
  ;; correct lines with inner / outer alignment
  (for* ([(page page-idx) (in-indexed (for*/list ([section (in-list (quad-elems doc))]
                                                  [page (in-list (quad-elems section))])
                                                 page))]
         [col (in-list (quad-elems page))]
         [block (in-list (quad-elems col))]
         [line (in-list (quad-elems block))])
        ;; all inner / outer lines are initially filled as if they were right-aligned
        (define zero-filler-side (if (odd? (add1 page-idx)) "inner" "outer"))
        (when (equal? zero-filler-side (quad-ref line :line-align))
          (match (quad-elems line)
            [(cons (? filler-quad? fq) _) (set-quad-size! fq (pt 0 0))]
            [_ (void)])))
  doc)

(define/contract (render-pdf qx-arg
                             [pdf-path-arg #false]
                             [base-dir-arg #false]
                             #:replace [replace-existing-file? #t]
                             #:compress [compress? #t])
  ((qexpr?) ((or/c #false path? path-string?)
             (or/c #false path? path-string?)
             #:replace boolean? #:compress boolean?) . ->* . (or/c void? bytes?))
  
  ;; The principal public interface to rendering.
  ;; `qx-arg` is the Q-expression to be rendered.
  ;; `pdf-path-arg` is the destination of the generated PDF.
  ;; #false signals that we should return the PDF bytes rather than saving.
  ;; `base-dir-arg` is the starting point for resolving any relative pathnames,
  ;; and looking for fonts and other assets.

  (match-define-values (base-dir _ _) (split-path
                                       (cond
                                         ;; for reasons unclear, DrRacket sometimes sneaks
                                         ;; an "unsaved editor" into this arg, despite efforts to prevent
                                         ;; probably my fault
                                         [(equal? base-dir-arg "unsaved editor") pdf-path-arg]
                                         [base-dir-arg]
                                         [pdf-path-arg]
                                         ;; pdf-path-arg is #false when we're returning bytes,
                                         ;; but we still need some directory, so current-directory is last resort.
                                         [else (current-directory)])))
  
  (unless (directory-exists? base-dir)
    (raise-argument-error 'render-pdf "existing directory" base-dir))
  
  (define pdf-path (setup-pdf-path pdf-path-arg))
  (unless replace-existing-file?
    (when (file-exists? pdf-path)
      (raise-argument-error 'render-pdf "path that doesn't exist" pdf-path)))

  ;; `make-pdf` creates a PDF data structure using the pitfall library.
  ;; this structure provides some services as we lay out the document,
  ;; and then when we render, we'll rely on pitfall's PDF-drawing routines.
  (parameterize ([current-pdf (make-pdf #:compress compress?
                                        #:auto-first-page #false
                                        #:output-path pdf-path)]
                 ;; set `current-directory` so that ops like `path->complete-path`
                 ;; will be handled relative to the original directory
                 [current-directory base-dir]
                 ;; a lot of operations need to look at pages used so it's easier to
                 ;; make it a parameter than endlessly pass it around as an argument.
                 [section-pages-used 0]
                 [verbose-quad-printing? #false]) ; for ease of debugging; not mandatory
    (define qs (time-log setup-qs (setup-qs qx-arg base-dir)))
    (setup-pdf-metadata! qs (current-pdf))
    ;; all the heavy lifting happens inside `make-sections`
    ;; which calls out to `make-pages`, `make-columns`, and so on.
    (define doc (correct-line-alignment (quad-copy q:doc [elems (make-sections qs)])))
    ;; call `position` and `draw` separately so we can print a timer for each
    (define positioned-doc (time-log position (position doc)))
    ;; drawing implies that a PDF is written to disk
    (time-log draw (draw positioned-doc (current-pdf))))

  (if pdf-path-arg
      (log-quadwriter-info (format "wrote PDF to ~a" pdf-path))
      (begin0
        (file->bytes pdf-path)
        (delete-file pdf-path))))
