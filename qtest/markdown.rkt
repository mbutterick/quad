#lang debug racket/base
(require (for-syntax racket/base) txexpr racket/runtime-path racket/string racket/promise racket/match racket/list
         pitfall quad sugar/debug pollen/tag racket/unsafe/ops)
(provide (except-out (all-from-out racket/base) #%module-begin)
         (rename-out [mb #%module-begin])
         p id strong em attr-list h1 h2 h3 h4 h5 h6 
         ol li ul rsquo lsquo rdquo ldquo hellip ndash mdash
         hr
         code pre a blockquote)

(define rsquo "’")
(define rdquo "”")
(define lsquo "‘")
(define ldquo "“")
(define hellip "…")
(define ndash "–")
(define mdash "—")

(define-tag-function (p attrs exprs)
  (qexpr attrs exprs))

(define-tag-function (hr attrs exprs)
  hrbr)

(define-tag-function (blockquote attrs exprs)
  (qexpr (list* '(display "block")
                '(background-color "#eee")
                '(font "fira") '(fontsize "10") '(line-height "15")
                '(border-width-top "0.5") '(border-color-top "gray") '(border-inset-top "8")
                '(border-width-left "3") '(border-color-left "gray") '(border-inset-left "20")
                '(border-width-bottom "0.5") '(border-color-bottom "gray") '(border-inset-bottom "-2")
                '(border-width-right "0.5") '(border-color-right "gray") '(border-inset-right "20")
                '(inset-top "10") '(inset-bottom "8") '(inset-left "30") '(inset-right "30")
                attrs) exprs))

(define id (default-tag-function 'id))
(define class (default-tag-function 'class))

(define-tag-function (strong attrs exprs)
  (qexpr (cons '(font "charter-bold") attrs) exprs))

(define-tag-function (a attrs exprs)
  (qexpr `((link ,(cadr (assoc 'href attrs)))(color "MediumVioletRed")) exprs))

(define-tag-function (em attrs exprs)
  (qexpr (cons '(font "charter-italic") attrs) exprs))

(define-syntax-rule (attr-list . attrs) 'attrs)

(define (heading-base font-size attrs exprs)
  (qexpr (append `((font "fira-light")(fontsize ,(number->string font-size))(line-height ,(number->string (* 1.2 font-size)))) attrs) exprs))

(define-tag-function (h1 attrs exprs) (heading-base 20 attrs exprs))
(define-tag-function (h2 attrs exprs) (heading-base 16 attrs exprs))
(define-tag-function (h3 attrs exprs) (heading-base 14 attrs exprs))

(define h4 h3)
(define h5 h3)
(define h6 h3)

(define-tag-function (code attrs exprs)
  (qexpr (append '((font "fira-mono")(fontsize "10")(bg "aliceblue")) attrs)  exprs))

(define-tag-function (pre attrs exprs)
  ;; pre needs to convert white space to equivalent layout elements
  (define new-exprs (add-between
                     (for*/list ([expr (in-list exprs)]
                                 [str (in-list (string-split (string-join (get-elements expr) "") "\n"))])
                       `(,(get-tag expr) ,(get-attrs expr) ,str))
                     lbr))
  (qexpr (list* '(display "block") '(background-color "aliceblue")
                '(font "fira-mono") '(fontsize "11") '(line-height "14")
                '(border-inset-top "10")
                '(border-width-left "2") '(border-color-left "#669") '(border-inset-left "0")
                '(border-inset-right "10") '(border-inset-bottom "-4")
                '(inset-left "12") '(inset-right "12")  '(inset-top "12") '(inset-bottom "12")
                attrs) new-exprs))


(define draw-debug? #t)
(define draw-debug-line? #f)
(define draw-debug-block? #f)
(define draw-debug-string? #f)


(define (list-base attrs exprs [bullet-val #f])
  (qexpr (list* '(inset-left "20") attrs)
         (add-between
          (for/list ([(expr idx) (in-indexed exprs)])
            (list* (get-tag expr) (cons (list 'list-index (or bullet-val (format "~a" (add1 idx)))) (get-attrs expr)) (get-elements expr)))
          pbr)))

(define-tag-function (ol attrs exprs) (list-base attrs exprs))
(define-tag-function (ul attrs exprs) (list-base attrs exprs "•"))
(define-tag-function (li attrs exprs) (qexpr attrs exprs))  

(define q:string (q #:in 'bi
                    #:out 'bo ;; align to baseline
                    ;; printable unless single space, which is not printable at start or end
                    #:printable (λ (q [sig #f])
                                  (match (quad-elems q)
                                    [(cons elem _)
                                     (case elem
                                       [(" " #\space) (not (memq sig '(start end)))]
                                       [else #true])]
                                    [_ #true]))
                    ;; draw with pdf text routine
                    #:draw (λ (q doc)
                             (when (pair? (quad-elems q))
                               (font doc (path->string (hash-ref (quad-attrs q) 'font)))
                               (font-size doc (hash-ref (quad-attrs q) 'fontsize 12))
                               (fill-color doc (hash-ref (quad-attrs q) 'color "black"))
                               (define str (unsafe-car (quad-elems q)))
                               (match-define (list x y) (quad-origin q))
                               (text doc str x y
                                     #:bg (hash-ref (quad-attrs q) 'bg #f)
                                     #:features (list (cons #"tnum" 1))
                                     #:link (hash-ref (quad-attrs q) 'link #f))))
                    #:draw-end (if draw-debug-string?
                                   (λ (q doc) (draw-debug q doc "#99f" "#ccf"))
                                   void)))

(define-runtime-path charter "fonts/charter.ttf")
(define-runtime-path charter-bold "fonts/charter-bold.ttf")
(define-runtime-path charter-italic "fonts/charter-italic.ttf")
(define-runtime-path fira "fonts/fira.ttf")
(define-runtime-path fira-light "fonts/fira-light.ttf")
(define-runtime-path fira-mono "fonts/fira-mono.ttf")

(define default-font-face charter)
(define default-font-size 12)

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
                                            ["fira-light" fira-light]
                                            ["fira-mono" fira-mono])))
                             default-font-face)
               (hash-ref! attrs 'fontsize default-font-size)
               attrs)]
      [elems (quad-elems q)]
      [size (delay
              (define fontsize (hash-ref (quad-attrs q) 'fontsize))
              (font-size doc fontsize)
              (font doc (path->string (hash-ref (quad-attrs q) 'font)))
              (define str (if (pair? (quad-elems q)) (unsafe-car (quad-elems q)) ""))
              (define line-height (cond
                                    [(and (pair? (quad-elems q)) (quad-ref q 'line-height))]
                                    [else (current-line-height doc)]))
              (pt (string-width doc str) line-height))])]))


(define (draw-debug q doc [fill-color "#f99"] [stroke-color "#fcc"])
  (when draw-debug?
    (save doc)
    (line-width doc 0.5)
    (apply rect doc (append (quad-origin q) (size q)))
    (stroke doc stroke-color)
    (circle doc (pt-x (in-point q)) (pt-y (in-point q)) 2)
    (circle doc (pt-x (out-point q)) (pt-y (out-point q)) 2)
    (fill doc fill-color)  
    (rect-centered doc (pt-x (inner-point q)) (pt-y (inner-point q)) 2)
    (fill doc stroke-color)  
    (restore doc)))

(define line-height 20)
(define dumb-hardcoded-value 380.1234)
(define q:line (q #:size (pt dumb-hardcoded-value line-height)
                  #:in 'nw
                  #:inner 'sw
                  #:out 'sw
                  #:printable #true
                  #:draw-start (if draw-debug-line? draw-debug void)))
(struct line-spacer quad () #:transparent)
(define q:line-spacer (q #:type line-spacer
                         #:size (pt dumb-hardcoded-value (* line-height 0.6))
                         #:out 'sw
                         #:printable (λ (q sig) (not (memq sig '(start end))))
                         #:draw-start (if draw-debug-line? draw-debug void)))

(define softies (map string '(#\space #\- #\u00AD)))

(define (soft-break-for-line? q)
  (and (pair? (quad-elems q))
       (member (unsafe-car (quad-elems q)) softies)))

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
(define-quad hr-break para-break ())
(define hrbr (make-hr-break #:printable #t))

(module+ test
  (check-true (line-break? (second (quad-elems (q "foo" pbr "bar")))))
  (check-true (line-break? (second (atomize (q "foo" pbr "bar"))))))

(define (copy-block-attrs source-hash dest-hash)
  (define block-attrs '(display
                        inset-top inset-bottom inset-left inset-right
                        border-inset-top  border-inset-bottom border-inset-left border-inset-right
                        border-width-left border-width-right border-width-top border-width-bottom
                        border-color-left border-color-right border-color-top border-color-bottom
                        background-color))
  (for* ([k (in-list block-attrs)]
         [v (in-value (hash-ref source-hash k #f))]
         #:when v)
    (hash-set! dest-hash k v))
  dest-hash)

(define (line-wrap xs wrap-size)
  (wrap xs (λ (q idx) (- wrap-size
                         (quad-ref q 'inset-left 0)
                         (quad-ref q 'inset-right 0)))                        
        #:hard-break line-break?
        #:soft-break soft-break-for-line?
        #:wrap-count (λ (idx q) (if (para-break? q)
                                    1
                                    (add1 idx)))
        #:finish-wrap
        (λ (pcs opening-q ending-q idx)
          (append
           (cond
             [(empty? pcs) null]
             [(hr-break? ending-q)
              (list (struct-copy quad q:line
                                 [draw-start (λ (dq doc)
                                               (match-define (list left top) (quad-origin dq))
                                               (match-define (list right bottom)(size dq))
                                               (translate doc left (+ top (/ bottom 2)))
                                               (move-to doc 0 0)
                                               (line-to doc right 0)
                                               (line-width doc 3)
                                               (stroke doc "#999"))]))]
             [else
              (match (consolidate-runs pcs)
                [(? pair? elems)
                 (define elem (unsafe-car elems))
                 (list (struct-copy quad q:line
                                    ;; move block attrs up, so they are visible in page wrap
                                    [attrs (copy-block-attrs (quad-attrs elem)
                                                             (hash-copy (quad-attrs q:line)))]
                                    ;; line width is static
                                    ;; line height is the max 'line-height value or the natural height of q:line
                                    [size (let ()
                                            (define line-heights
                                              (filter-map (λ (q) (quad-ref q 'line-height)) pcs))
                                            (match-define (list w h) (quad-size q:line))
                                            (pt w (if (empty? line-heights) h (apply max line-heights))))]
                                    ;; handle list indexes. drop new quad into line to hold list index
                                    [elems (append
                                            (match (and (= idx 1) (quad-ref elem 'list-index))
                                              [#false null]
                                              [bullet (list (make-quad
                                                             ;; wart: iffy to rely on `(car elems)` here
                                                             ;; what if first elem is not a string quad?
                                                             #:elems (list (struct-copy quad (car elems)
                                                                                        [elems (list (if (number? bullet)
                                                                                                         (format "~a." bullet)
                                                                                                         bullet))]))))])
                                            (list (make-quad
                                                   #:offset (pt (quad-ref elem 'inset-left 0) 0)
                                                   #:elems elems)))]))]
                [_ null])])
           (if (and (para-break? ending-q) (not (hr-break? ending-q)))
               (list q:line-spacer)
               null)))))

(define zoom-mode? #f)
(define top-margin 60)
(define bottom-margin 120)
(define side-margin 120)
(define page-offset (pt (/ side-margin (if zoom-mode? 3 1))
                        (/ top-margin (if zoom-mode? 3 1))))
(require racket/date)
(define q:page (q #:offset page-offset
                  #:draw-start (λ (q doc) (add-page doc)
                                 (scale doc (if zoom-mode? 3 1) (if zoom-mode? 3 1)))
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

(define (block-wrap lines)
  (define first-line (car lines)) 
  (q #:in 'nw
     #:out 'sw
     #:offset (pt 0 (+ (quad-ref first-line 'inset-top 0)))
     #:elems lines
     #:size (delay (pt (pt-x (size first-line)) ; 
                       (+ (for/sum ([line (in-list lines)])
                            (pt-y (size line)))
                          (quad-ref first-line 'inset-top 0)
                          (quad-ref first-line 'inset-bottom 0))))
     #:draw-start (λ (q doc)
                    ;; adjust drawing coordinates for border inset
                    (match-define (list bil bit bir bib)
                      (map (λ (k) (quad-ref first-line k 0))
                           '(border-inset-left border-inset-top border-inset-right border-inset-bottom)))
                    (match-define (list left top) (map + (quad-origin q) (list bil bit)))
                    (match-define (list width height) (map - (size q) (list (+ bil bir) (+ bit bib))))
                    ;; fill rect
                    (cond 
                      [(quad-ref first-line 'background-color)
                       => (λ (bgcolor)
                            (rect doc left top width height)
                            (fill doc bgcolor))])
                    ;; draw border
                    (match-define (list bw-left bw-top bw-right bw-bottom)
                      (map (λ (k) (max 0 (quad-ref first-line k 0))) '(border-width-left
                                                                       border-width-top
                                                                       border-width-right
                                                                       border-width-bottom)))
                    ;; adjust start and end points based on adjacent border width
                    ;; so all borders overlap rectangularly
                    (define (half x) (/ x 2.0))
                    (define right (+ left width))
                    (define bottom (+ top height))
                    (define (box-side x1 y1 x2 y2 color stroke-width)
                      (when (positive? stroke-width)
                        (move-to doc x1 y1)
                        (line-to doc x2 y2)
                        (stroke doc (or color "black") stroke-width)))
                    (box-side (- left (half bw-left)) top (+ right (half bw-right)) top
                              (quad-ref first-line 'border-color-top) bw-top)
                    (box-side right (- top (half bw-top)) right (+ bottom (half bw-bottom))
                              (quad-ref first-line 'border-color-right) bw-right)
                    (box-side (+ right (half bw-right)) bottom (- left (half bw-left)) bottom
                              (quad-ref first-line 'border-color-bottom) bw-bottom)
                    (box-side left (+ bottom (half bw-bottom)) left (- top (half bw-top))
                              (quad-ref first-line 'border-color-left) bw-left))
     #:draw-end (if draw-debug-block?
                    (λ (q doc) (draw-debug q doc "#6c6" "#9c9"))
                    void))) 


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
        #:distance (λ (q dist-so-far wrap-qs)
                     ;; do trial block insertions
                     (for/sum ([x (in-list (insert-blocks wrap-qs))])
                       (pt-y (size x))))                     
        #:finish-wrap (λ (lns q0 q idx)
                        (list (struct-copy quad q:page
                                           [attrs (let ([page-number idx]
                                                        [h (hash-copy (quad-attrs q:page))])
                                                    (hash-set! h 'page-number page-number)
                                                    (define-values (dir name _)
                                                      (split-path (path-replace-extension path #"")))
                                                    (hash-set! h 'doc-title (string-titlecase (path->string name)))
                                                    h)]
                                           [elems (insert-blocks lns)])))))

(define (insert-blocks lines)
  (define groups-of-lines (contiguous-group-by (λ (x) (quad-ref x 'display)) lines))
  (append* (for/list ([line-group (in-list groups-of-lines)])
             (match (quad-ref (car line-group) 'display)
               ["block" (list (block-wrap line-group))]
               [_ line-group]))))


(define (run xs path)
  (define pdf (time-name make-pdf (make-pdf #:compress #t
                                            #:auto-first-page #f
                                            #:output-path path
                                            #:width (if zoom-mode? 350 612)
                                            #:height (if zoom-mode? 400 792))))
  (define line-width (- (pdf-width pdf) (* 2 side-margin)))
  (define vertical-height (- (pdf-height pdf) top-margin bottom-margin))
  (let* ([x (time-name atomize (atomize (qexpr->quad xs)))]
         [x (time-name ->string-quad (map (λ (x) (->string-quad pdf x)) x))]
         [x (time-name line-wrap (line-wrap x line-width))]
         [x (time-name page-wrap (page-wrap x vertical-height path))]
         [x (time-name position (position (struct-copy quad q:doc [elems x])))])
    (time-name draw (draw x pdf))))

(define-syntax (mb stx)
  (syntax-case stx ()
    [(_ PDF-PATH . STRS)
     #'(#%module-begin
        ;; stick an nbsp in the strings so we have one printing char
        (define strs (match (list . STRS)
                       [(? null?) '(" ")]
                       [strs strs]))
        (define qx (list* 'q null (add-between strs pbr)))
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
