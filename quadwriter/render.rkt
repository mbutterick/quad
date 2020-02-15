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
         "struct.rkt"
         "break.rkt"
         "draw.rkt"
         "string.rkt"
         "image.rkt"
         "log.rkt"
         "line.rkt"
         "page.rkt"
         "para.rkt"
         "section.rkt"
         "doc.rkt"
         "column.rkt"
         "keep.rkt"
         "debug.rkt"
         "query.rkt")
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
                (struct-copy quad q [elems (list substr)]))]
    [else (list q)]))


(define (string->feature-list str)
  (define pcs (string-split str))
  (unless (even? (length pcs))
    (raise-argument-error 'string->feature-list "even number of tags and values" pcs))
  (for/list ([kv (in-slice 2 pcs)])
            (cons (match (first kv)
                    [(? string? k) (string->bytes/utf-8 k)]
                    [k (raise-argument-error 'string->feature-list "string" k)])
                  (match (string->number (second kv))
                    [(? number? num) num]
                    [v (raise-argument-error 'string->feature-list "number string" v)]))))

(define (parse-font-features! attrs)
  ;; `font-features` are OpenType font feature specifiers.
  (define font-features-previous-key 'font-features-previous)
  (define features-previous (hash-ref attrs font-features-previous-key empty))
  (define val (hash-ref attrs :font-features #false))
  (when (string? val)
    (hash-set! attrs :font-features
               (cond
                 [(regexp-match #px"^\\s*\\+ " val)
                  ;; adjustment: parse the feature string and append to the previous feature set
                  (define parsed-features (string->feature-list (string-trim (string-trim val) "+")))
                  (remove-duplicates (append parsed-features features-previous) bytes=? #:key car)]
                 ;; replacement of previous feature string
                 [else (string->feature-list val)]))
    (hash-set! attrs font-features-previous-key (hash-ref attrs :font-features))))

(module+ test
  (require rackunit)
  ;; feature replacement
  (define attrs (make-hash '((font-features-previous '((#"smcp" . 1))))))
  (hash-set! attrs :font-features " liga 0 ")
  (parse-font-features! attrs)
  (check-equal? (hash-ref attrs :font-features) '((#"liga" . 0)))
  ;; feature append
  (hash-set! attrs :font-features "  + calt 1 ")
  (parse-font-features! attrs)
  (check-equal? (sort (hash-ref attrs :font-features) bytes<? #:key car) '((#"calt" . 1)(#"liga" . 0))))

(define (parse-dimension-strings! attrs)
  ;; certain attributes can be "dimension strings", which are strings like "3in" or "4.2cm"
  ;; we parse them into the equivalent measurement in points.
  (for ([k (in-hash-keys attrs)]
        #:when (takes-dimension-string? k))
       (hash-update! attrs k (λ (val) (parse-dimension val attrs))))
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
                             complete-every-path!
                             resolve-font-path!
                             resolve-font-size!
                             ;; we resolve dimension strings after font size
                             ;; because they can be denoted relative to em size
                             parse-dimension-strings!
                             parse-font-features!))])
       (proc attrs)))

(define (drop-leading-breaks qs)
  ;; any leading breaks are pointless at the start of the doc, so drop them.
  ;; How would we get these?
  ;; we might, for instance, have a first-level heading style that is specified with page break before.
  ;; so if we invoke that style first, we will get a page break.
  (dropf qs break-quad?))


(define (generic->typed-quad q)
  ;; replaces quads representing certain things
  ;; with special typed quads representing those things.
  ;; Because typed quads have their own predicates,
  ;; it's faster to find them in wrapping operations
  (cond
    [(convert-break-quad q)]
    [(convert-draw-quad q)]
    [(quad-ref q :image-file) (convert-image-quad q)]
    [else (convert-string-quad q)]))

#;(define (extract-defined-quads qs)
    (define (get-define-val q) (quad-ref q 'define))
    (define-values (dqs not-dqs) (partition get-define-val qs))
    (for ([dq-group (in-list (group-by get-define-val dqs))])
         (hash-set! (current-named-quads) (get-define-val (car dq-group)) dq-group))
    not-dqs)

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
                           :font-features default-font-features
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
         #;[qs (extract-defined-quads qs)]
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
     (cons 'Creator (format "Racket ~a [Quad ~a]" (version) (pkg-checksum "quad" #:short #true)))
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
  (define col-quad-prototype (quad-copy column-quad q:column
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
    (quad-copy page-quad q:page
               [shift (pt left-shift top-margin)]
               [size (pt line-wrap-size printable-height)]))
  (time-log page-wrap (page-wrap column-qs printable-width page-quad-prototype)))


(define (append-doc-repeaters secs repeaters)
  (for ([page (in-list (for*/list ([sec (in-list secs)]
                                   [elem (in-list (quad-elems sec))]
                                   #:when (page-quad? elem))
                                  elem))]
        [page-num (in-naturals 1)]
        [page-side (in-cycle '(right left))])
       (define repeaters-for-this-page
         (for/list ([repeater (in-list repeaters)]
                    #:when (let* ([val (quad-ref repeater :page-repeat)]
                                  [sym (string->symbol val)])
                             (or (eq? sym 'all)
                                 (eq? sym page-side)
                                 (eq? sym (if (= page-num 1) 'first 'rest)))))
                   repeater))
       (when (pair? repeaters-for-this-page)
         (set-quad-elems! page (append repeaters-for-this-page (quad-elems page)))))
  secs)

(define (make-sections all-qs)
  (define-values (doc-repeaters nonrepeating-qs)
    (partition  (λ (q) (member (quad-ref q :page-repeat) '("all" "left" "right" "first" "rest"))) all-qs))
  (for/fold ([sections-acc null]
             #:result (append-doc-repeaters (reverse sections-acc) doc-repeaters))
            ([all-section-qs (in-list (filter-split nonrepeating-qs section-break-quad?))])
    
    (define-values (section-repeaters qs)
      (partition  (λ (q) (member (quad-ref q :page-repeat) '("section" "section all" "section left" "section right" "section first" "section rest"))) all-section-qs))
    
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
    
    (define section-pages-without-repeaters (make-pages column-qs
                                                        left-margin
                                                        top-margin
                                                        gutter-margin
                                                        line-wrap-size
                                                        printable-width
                                                        printable-height))

    ;; put in quads that repeat within the section
    (define section-pages
      (for/list ([page (in-list section-pages-without-repeaters)]
                 [page-num (in-naturals 1)]
                 [page-side (in-cycle ((if (eq? section-starting-side 'right) values reverse) '(right left)))])
                (define section-repeaters-for-this-page
                  (for/list ([repeater (in-list section-repeaters)]
                             #:when (let* ([val (quad-ref repeater :page-repeat)]
                                           [sym (string->symbol (string-trim val #px"section\\s"))])
                                      (or (eq? sym 'section)
                                          (eq? sym 'all)
                                          (eq? sym page-side)
                                          (eq? sym (if (= page-num 1) 'first 'rest)))))
                            repeater))
                (cond
                  [(null? section-repeaters-for-this-page) page]
                  [else
                   (quad-copy page-quad page
                              [elems (append section-repeaters-for-this-page (quad-elems page))])])))
        
    (begin0
      (cond
        ;; ignore empty section
        [(zero? (length section-pages)) sections-acc]
        [insert-blank-page?
         (match section-starting-side
           ['left
            ;; blank page goes at beginning of current section
            (define page-from-current-section (car section-pages))
            (define blank-page (quad-copy page-quad page-from-current-section [elems null]))
            (define new-section (quad-copy section-quad q:section [elems (cons blank-page section-pages)]))
            (cons new-section sections-acc)]
           [_ ;; must be 'right
            ;; blank page goes at end of previous section (if it exists)
            (define new-section (quad-copy section-quad q:section [elems section-pages]))
            (match sections-acc
              [(cons previous-section other-sections)
               (define previous-section-pages (quad-elems previous-section))
               ;; we know previous section has pages because we ignore empty sections
               (define page-from-previous-section (car previous-section-pages))
               (define blank-page (quad-copy page-quad page-from-previous-section [elems null]))
               (define updated-previous-section
                 (quad-update! previous-section
                               [elems (append previous-section-pages (list blank-page))]))
               (list* new-section updated-previous-section other-sections)]
              [_ (list new-section)])])]
        [else (define new-section (quad-copy section-quad q:section [elems section-pages]) )
              (cons new-section sections-acc)])
      (section-pages-used (+ (section-pages-used) (length section-pages))))))

(define (wants-parent? x) (and (quad? x) (quad-ref x :anchor-parent)))
(define (resolve-parents doc)
  ;; we make our index now so that it includes the quads that want parents
  ;; so if we come across `this` as a subscript, we can resolve it
  ;; by reference to the parent-wanting quad
  (define qi (make-query-index doc))
  ;; extract the quads that want parents
  (define parent-wanter-acc null)
  (let loop ([x doc])
    (match x
      [(? quad?) (define-values (parent-wanters others)
                   (partition wants-parent? (quad-elems x)))
                 (when (pair? parent-wanters)
                   (set! parent-wanter-acc (append parent-wanter-acc parent-wanters))
                   (quad-update! x [elems others]))
                 (map loop others)]
      [_ x]))
  ;; then put them where they want to go
  ;; if the query has no result, then the quad doesn't get replaced (ie. disappears)
  ;; which seems like the right outcome.
  (for* ([wp (in-list parent-wanter-acc)]
         [query-str (in-value (quad-ref wp :anchor-parent))]
         #:when query-str
         [parent (in-value (query qi query-str wp))]
         #:when parent)
        (quad-update! parent [elems (append (quad-elems parent) (list wp))]))
  doc)

(define (correct-line-alignment doc)
  ;; correct lines with inner / outer alignment
  ;; all inner / outer lines are initially filled as if they were right-aligned
  ;; on odd (right-hand) pages, inner becomes 0 (and on left-hand, outer becomes 0)
  (for* ([section (in-list (quad-elems doc))]
         [(page page-idx) (in-indexed (quad-elems section))]
         #:when (page-quad? page))
        (define right-side? (odd? (add1 page-idx)))
        (define zero-filler-side (if right-side? "inner" "outer"))
        (let loop ([x page])
          (cond
            [(and (line-quad? x)
                  (equal? zero-filler-side (quad-ref x :line-align))
                  (filler-quad? (car (quad-elems x))))
             ;; collapse the filler quad by setting size to 0
             (set-quad-size! (car (quad-elems x)) (pt 0 0))]
            [(quad? x) (for-each loop (quad-elems x))])))
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

  (define base-dir (let ([maybe-dir (cond
                                      ;; for reasons unclear, DrRacket sometimes sneaks
                                      ;; an "unsaved editor" into base-dir-arg, despite efforts
                                      ;; probably my fault 
                                      [(equal? base-dir-arg "unsaved editor") pdf-path-arg]
                                      [base-dir-arg]
                                      [pdf-path-arg]
                                      [else (current-directory)])])
                     (match maybe-dir
                       [(? directory-exists? dir) dir]
                       [_ (define-values (dir name _) (split-path maybe-dir))
                          dir])))
  
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
                 [verbose-quad-printing? #false]
                 #;[current-named-quads (make-hash)]) ; for ease of debugging; not mandatory
    (define qs (time-log setup-qs (setup-qs qx-arg base-dir)))
    (setup-pdf-metadata! qs (current-pdf))
    ;; all the heavy lifting happens inside `make-sections`
    ;; which calls out to `make-pages`, `make-columns`, and so on.
    (define doc (let ([doc (quad-update! q:doc [elems (make-sections qs)])])
                  (time-log prep-doc (let* ([doc (correct-line-alignment doc)]
                                            [doc (resolve-parents doc)])
                                       doc))))
    ;; call `position` and `draw` separately so we can print a timer for each
    (define positioned-doc (time-log position (position doc)))
    ;; drawing implies that a PDF is written to disk
    (time-log draw (draw positioned-doc (current-pdf))))

  (if pdf-path-arg
      (log-quadwriter-info (format "wrote PDF to ~a" pdf-path))
      (begin0
        (file->bytes pdf-path)
        (delete-file pdf-path))))
