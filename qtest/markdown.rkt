#lang debug racket/base
(require (for-syntax racket/base) txexpr racket/runtime-path racket/string racket/promise racket/match racket/list
         pitfall quad sugar/debug pollen/tag)
(provide (except-out (all-from-out racket/base) #%module-begin)
         (rename-out [mb #%module-begin])
         p id strong em attr-list h1 h2 code pre a blockquote)

(define-tag-function (p attrs exprs)
  (qexpr attrs exprs))

(define-tag-function (blockquote attrs exprs)
  (qexpr (cons '(container "bq") attrs) exprs))

(define id (default-tag-function 'id))
(define class (default-tag-function 'class))

(define-tag-function (strong attrs exprs)
  (qexpr (cons '(font "charter-bold") attrs) exprs))

(define-tag-function (a attrs exprs)
  (qexpr `((link ,(cadr (assoc 'href attrs)))(color "MediumVioletRed")) exprs))

(define-tag-function (em attrs exprs)
  (qexpr (cons '(font "charter-italic") attrs) exprs))

(define-syntax-rule (attr-list . attrs) 'attrs)

(define-tag-function (h1 attrs exprs)
  (qexpr (append  '((font "fira")(fontsize "36")(line-height "48")) attrs) exprs))

(define-tag-function (h2 attrs exprs)
  (qexpr (append '((font "fira")(fontsize "24")(line-height "36")) attrs)  exprs))

(define-tag-function (code attrs exprs)
  (qexpr (append '((font "fira-mono")(fontsize "11")(bg "aliceblue")) attrs)  exprs))

(define-tag-function (pre attrs exprs)
  ;; pre needs to convert white space to equivalent layout elements
  (define new-exprs (add-between
                     (for*/list ([expr (in-list exprs)]
                                 [str (in-list (string-split (car (get-elements expr)) "\n"))])
                                `(,(get-tag expr) ,(get-attrs expr) ,str))
                     lbr))
  (qexpr attrs new-exprs))

(define q:string (q #:in 'bi
                    #:out 'bo ;; align to baseline
                    ;; printable unless single space, which is not printable at start or end
                    #:printable (λ (q [sig #f])
                                  (case (car (quad-elems q))
                                    [(" " #\space) (not (memq sig '(start end)))]
                                    [else #true]))
                    ;; draw with pdf text routine
                    #:draw (λ (q doc)
                             (font doc (path->string (hash-ref (quad-attrs q) 'font)))
                             (font-size doc (string->number (hash-ref (quad-attrs q) 'fontsize "12")))
                             (fill-color doc (hash-ref (quad-attrs q) 'color "black"))
                             (match-define (list str) (quad-elems q))
                             (match-define (list x y) (quad-origin q))
                             (text doc str x y #:bg (hash-ref (quad-attrs q) 'bg #f)
                                   #:link (hash-ref (quad-attrs q) 'link #f)))
                    #:draw-end (λ (q doc)  (draw-debug q doc "#99f" "#ccf"))))

(define-runtime-path charter "fonts/charter.ttf")
(define-runtime-path charter-bold "fonts/charter-bold.ttf")
(define-runtime-path charter-italic "fonts/charter-italic.ttf")
(define-runtime-path fira "fonts/fira-light.ttf")
(define-runtime-path fira-mono "fonts/fira-mono.ttf")

(define (->string-quad doc q)
  (cond
    [(line-break? q) q]
    [else
     (struct-copy
      quad q:string
      [attrs (let ([attrs (quad-attrs q)])
               ;; attrs hashes are shared between many quads.
               ;; so the first update will change every reference to the shared hash
               ;; hence why we ignore if val is already a path
               ;; but this op should ideally happen earlier
               (hash-update! attrs 'font
                             (λ (val) (if (path? val)
                                          val
                                          (match (string-downcase (string-replace val " " "-"))
                                            ["charter" charter]
                                            ["charter-bold" charter-bold]
                                            ["charter-italic" charter-italic]
                                            ["fira" fira]
                                            ["fira-mono" fira-mono]))))
               attrs)]
      [elems (quad-elems q)]
      [size (delay
              (define fontsize (string->number (hash-ref (quad-attrs q) 'fontsize)))
              (font-size doc fontsize)
              (font doc (path->string (hash-ref (quad-attrs q) 'font)))
              (define str (car (quad-elems q)))
              (pt (string-width doc str) (current-line-height doc)))])]))

(define draw? #f)
(define (draw-debug q doc [fill-color "#f99"] [stroke-color "#fcc"])
  (when draw?
    (save doc)
    (line-width doc 0.5)
    (apply rect doc (append (quad-origin q) (size q)))
    (stroke doc stroke-color)
    (apply rect doc (append (quad-origin q) (size q)))
    (clip doc)
    (circle doc (pt-x (in-point q)) (pt-y (in-point q)) 3)
    (circle doc (pt-x (out-point q)) (pt-y (out-point q)) 3)
    (fill doc fill-color)  
    (restore doc)))

(define line-height 20)
(define q:line (q #:size (pt 380 line-height)
                  #:in 'nw
                  #:inner 'sw ; puts baseline at lower right corner of line box
                  #:out 'sw
                  #:printable #true
                  #:draw-start draw-debug))
(struct line-spacer quad () #:transparent)
(define q:line-spacer (q #:type line-spacer
                         #:size (pt 380 (* line-height 0.6))
                         #:out 'sw
                         #:printable (λ (q sig)
                                       (not (memq sig '(start end))))
                         #:draw-start draw-debug))

(define softies (map string '(#\space #\- #\u00AD)))
(define (soft-break-for-line? q)
  (member (car (quad-elems q)) softies))

(define (consolidate-runs pcs)
  (for/fold ([runs empty]
             [pcs pcs]
             #:result (reverse runs))
            ([i (in-naturals)]
             #:break (empty? pcs))
    (define-values (run-pcs rest) (splitf-at pcs (λ (p) (same-run? (car pcs) p))))
    (define new-run (struct-copy quad q:string
                                 [attrs (quad-attrs (car pcs))]
                                 [elems (merge-adjacent-strings (apply append (for/list ([pc (in-list run-pcs)])
                                                                                        (quad-elems pc))))]
                                 [size (delay (pt (for/sum ([pc (in-list run-pcs)])
                                                           (pt-x (size pc)))
                                                  (pt-y (size (car pcs)))))]))
    (values (cons new-run runs) rest)))

(define-quad line-break quad ())
(define lbr (make-line-break #:printable #f))
;; treat paragraph break as special kind of line break
(define-quad para-break line-break ())
(define pbr (make-para-break #:printable #f))

(module+ test
  (check-true (line-break? (second (quad-elems (q "foo" pbr "bar")))))
  (check-true (line-break? (second (atomize (q "foo" pbr "bar"))))))

(define (line-wrap xs size)
  (wrap xs size
        #:hard-break line-break?
        #:soft-break soft-break-for-line?
        #:finish-wrap (λ (pcs q idx)
                        (define new-elems (consolidate-runs pcs))
                        (append
                         (list (struct-copy quad q:line
                                            [attrs (let ([attrs (hash-copy (quad-attrs q:line))])
                                                     (define container-val (hash-ref (quad-attrs (car new-elems)) 'container #f))
                                                     (when (and container-val
                                                                (for/and ([elem (in-list (cdr new-elems))])
                                                                         (equal? (hash-ref (quad-attrs elem) 'container #f)
                                                                                 container-val)))
                                                       (hash-set! attrs 'container container-val))
                                                     attrs)]
                                            [size (let ()
                                                    (define line-heights
                                                      (filter-map
                                                       (λ (q) (string->number (hash-ref (quad-attrs q) 'line-height "NaN")))
                                                       pcs))
                                                    (match-define (list w h) (quad-size q:line))
                                                    ;; when `line-heights` is empty, this is just h
                                                    (pt w (apply max (cons h line-heights))))]
                                            [elems new-elems]))
                         (if (para-break? q)
                             (list q:line-spacer)
                             null)))))

(define top-margin 60)
(define bottom-margin 120)
(define side-margin 120)
(define page-offset (pt side-margin top-margin))
(require racket/date)
(define q:page (q #:offset page-offset
                  #:draw-start (λ (q doc) (add-page doc))
                  #:draw-end (λ (q doc)
                               (font-size doc 10)
                               (font doc charter)
                               (fill-color doc "black")
                               (text doc (format "~a · ~a at ~a" (hash-ref (quad-attrs q) 'page-number)
                                                 (hash-ref (quad-attrs q) 'doc-title)
                                                 (date->string (current-date) #t))
                                     side-margin
                                     (+ (- (pdf-height doc) bottom-margin) 20)))))

(define q:doc (q #:draw-start (λ (q doc) (start-doc doc))
                 #:draw-end (λ (q doc) (end-doc doc))))

(define (make-blockquote pcs)
  (q #:attrs (hasheq 'type "bq")
     #:in 'nw
     #:out 'sw
     #:elems pcs
     #:size (delay (pt (pt-x (size (car pcs)))
                       (for/sum ([pc (in-list pcs)])
                                (pt-y (size pc)))))
     #:draw-start (λ (q doc)
                    (save doc)
                    (match-define (list left top) (quad-origin q))
                    (match-define (list right bottom) (size q))
                    (rect doc (- left 4) (+ top 6) right (+ bottom 2))
                    (line-width doc 1)
                    (fill-and-stroke doc "#eee" "#999")
                    (restore doc))))

(define (contiguous-group-by pred xs)
  ;; like `group-by`, but only groups together contiguous xs with the same pred value.
  (let loop ([xs xs][groups null])
    (match xs
      [(== empty) (reverse groups)]
      [(cons first-x other-xs)
       (define equivalence-val (pred first-x))
       (define-values (group-members rest) (splitf-at other-xs (λ (x) (equal? (pred x) equivalence-val))))
       (define new-group (cons first-x group-members)) ; group-members might be empty
       (loop rest (cons new-group groups))])))

(module+ test
  (require rackunit)
  (check-equal?
   (contiguous-group-by values '(1 1 2 2 2 3 4 5 5 6 6 7 8 9))
   '((1 1) (2 2 2) (3) (4) (5 5) (6 6) (7) (8) (9))))

(define (page-wrap xs vertical-height path)
  (wrap xs vertical-height
        #:soft-break line-spacer?
        #:wrap-anywhere? #t
        #:finish-wrap (λ (lns q idx)
                        (list (struct-copy quad q:page
                                           [attrs (let ([page-number idx]
                                                        [h (hash-copy (quad-attrs q:page))])
                                                    (hash-set! h 'page-number page-number)
                                                    (define-values (dir name _)
                                                      (split-path (path-replace-extension path #"")))
                                                    (hash-set! h 'doc-title (string-titlecase (path->string name)))
                                                    h)]
                                           [elems lns])))))

(define (insert-containers pages)
  ;; container recomposition happens after page composition because page breaks can happen between lines.
  ;; iow, the lines within a container may be split over multiple pages, each of which should be drawn
  ;; as a separate container
  (for/list ([page (in-list pages)])
            (define lns (quad-elems page))             
            (define groups (contiguous-group-by (λ (x) (hash-ref (quad-attrs x) 'container #f)) lns))
            (define lns-and-containers (append* (for/list ([grp (in-list groups)])
                                                          (match (hash-ref (quad-attrs (car grp)) 'container #f)
                                                            ["bq" (list (make-blockquote grp))]
                                                            [_ grp]))))
            (struct-copy quad page [elems lns-and-containers])))

(define (run xs path)
  (define pdf (time-name make-pdf (make-pdf #:compress #t
                                            #:auto-first-page #f
                                            #:output-path path
                                            #:size "letter")))
  (define line-width (- (pdf-width pdf) (* 2 side-margin)))
  (define vertical-height (- (pdf-height pdf) top-margin bottom-margin))
  (let* ([x (time-name atomize (atomize (qexpr->quad xs)))]
         [x (time-name ->string-quad (map (λ (x) (->string-quad pdf x)) x))]
         [x (time-name line-wrap (line-wrap x line-width))]
         [x (time-name page-wrap (page-wrap x vertical-height path))]
         [x (time-name insert-containers (insert-containers x))]
         [x (time-name position (position (struct-copy quad q:doc [elems x])))])
    (time-name draw (draw x pdf))))

(define-syntax (mb stx)
  (syntax-case stx ()
    [(_ PDF-PATH . STRS)
     #'(#%module-begin
        (define qx (list* 'q '((font "Charter") (fontsize "12")) (add-between (list . STRS) pbr)))
        (run qx PDF-PATH))]))

(module+ reader
  (require scribble/reader syntax/strip-context (only-in markdown parse-markdown)
           racket/match txexpr)
  (provide (rename-out [quad-read-syntax read-syntax]))

 
  (define (xexpr->parse-tree x)
    ;; an ordinary txexpr can't serve as a parse tree because of the attrs list fails when passed to #%app.
    ;; so stick an `attr-list` identifier on it which can hook into the expander.
    ;; sort of SXML-ish.
    (let loop ([x x])
      (match x
        [(txexpr tag attrs elems) (list* tag (cons 'attr-list attrs) (map loop elems))]
        [(? list? xs) (map loop xs)]
        [_ x])))
  
  (define (quad-read-syntax path-string p)
    (define quad-at-reader (make-at-reader
                            #:syntax? #t 
                            #:inside? #t
                            #:command-char #\◊))
    (define stx (quad-at-reader path-string p))
    (define parsed-stx (datum->syntax stx (xexpr->parse-tree (parse-markdown (apply string-append (syntax->datum stx))))))
    (strip-context
     (with-syntax ([PT parsed-stx]
                   [PDF-PATH (path-replace-extension path-string #".pdf")])
       #'(module _ qtest/markdown
           PDF-PATH
           . PT)))))
