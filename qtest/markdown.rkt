#lang debug racket/base
(require (for-syntax racket/base) txexpr racket/runtime-path racket/path racket/string racket/promise racket/match racket/list
         pitfall quad sugar/debug pollen/tag racket/unsafe/ops hyphenate)
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

(define (root attrs exprs)
  (qexpr (append `(#;(first-line-indent "12")
                   #;(line-align "center")
                   (line-wrap "kp")
                   #;(line-align-last "center")) attrs) exprs))

(define-tag-function (p attrs exprs)
  ;; no font-family so that it adopts whatever the surrounding family is
  (qexpr (append `((keep-first "2")(keep-last "3") (font-size-adjust "100%") (character-tracking "0") (hyphenate "true") (display ,(symbol->string (gensym)))) attrs) exprs))

(define-tag-function (hr attrs exprs)
  hrbr)

(define-tag-function (blockquote attrs exprs)
  (qexpr (append '((display "block") 
                   (first-line-indent "0") 
                   (background-color "#eee") 
                   (font-family "fira") (font-size "10") (line-height "15")
                   (border-width-top "0.5") (border-color-top "gray") (border-inset-top "8")
                   (border-width-left "3") (border-color-left "gray") (border-inset-left "20")
                   (border-width-bottom "0.5") (border-color-bottom "gray") (border-inset-bottom "-2")
                   (border-width-right "0.5") (border-color-right "gray") (border-inset-right "20")
                   (inset-top "10") (inset-bottom "8") (inset-left "30") (inset-right "30")
                   (keep-lines "yes"))
                 attrs) exprs))

(define id (default-tag-function 'id))
(define class (default-tag-function 'class))

(define-tag-function (strong attrs exprs)
  (qexpr (list* '(font-bold "true") '(font-size-adjust "100%") attrs) exprs))

(define-tag-function (a attrs exprs)
  (qexpr `((link ,(cadr (assoc 'href attrs)))(color "MediumVioletRed")) exprs))

(define-tag-function (em attrs exprs)
  (qexpr (list* '(font-italic "true") '(font-size-adjust "100%") attrs) exprs))

(define-syntax-rule (attr-list . attrs) 'attrs)

(define (heading-base font-size attrs exprs)
  (qexpr (append `((font-family "fira-light") (first-line-indent "0") (display "block") (font-size ,(number->string font-size))(line-height ,(number->string (* 1.2 font-size))) (border-width-top "0.5")(border-inset-top "9")(border-inset-right "12") (inset-bottom "-3") (inset-top "6") (keep-with-next "true")) attrs) exprs))

(define-tag-function (h1 attrs exprs)
  (heading-base 20 (append '() attrs) exprs))

(define-tag-function (h2 attrs exprs) (heading-base 16 attrs exprs))
(define-tag-function (h3 attrs exprs) (heading-base 14 attrs exprs))

(define h4 h3)
(define h5 h3)
(define h6 h3)

(define-tag-function (code attrs exprs)
  (qexpr (append '((font-family "fira-mono")#;(line-align "right")(font-size "10")(bg "aliceblue")) attrs)  exprs))

(define-tag-function (pre attrs exprs)
  ;; pre needs to convert white space to equivalent layout elements
  (define new-exprs (add-between
                     (for*/list ([expr (in-list exprs)]
                                 [str (in-list (string-split (string-join (get-elements expr) "") "\n"))])
                       `(,(get-tag expr) ,(get-attrs expr) ,(string-replace str " " " ")))
                     lbr))
  (qexpr (list* '(display "block") '(background-color "aliceblue")
                '(font-family "fira-mono") '(font-size "11") '(line-height "14")
                '(border-inset-top "10")
                '(border-width-left "2") '(border-color-left "#669") '(border-inset-left "0")
                '(border-inset-right "10") '(border-inset-bottom "-4")
                '(inset-left "12") '(inset-right "12")  '(inset-top "12") '(inset-bottom "8")
                attrs) new-exprs))


(define draw-debug? #t)
(define draw-debug-line? #t)
(define draw-debug-block? #t)
(define draw-debug-string? #f)

(define (list-base attrs exprs [bullet-val #f])
  (qexpr (list* '(inset-left "20") attrs)
         (add-between
          (for/list ([(expr idx) (in-indexed exprs)])
            (list* (get-tag expr) (cons (list 'list-index (or bullet-val (format "~a" (add1 idx)))) (get-attrs expr)) (get-elements expr)))
          pbr)))

(define-tag-function (ol attrs exprs) (list-base attrs exprs))
(define-tag-function (ul attrs exprs) (list-base attrs exprs "•"))
(define-tag-function (li attrs exprs) (qexpr (cons '(first-line-indent "0") attrs) exprs))  

(define-quad string-quad quad ())
(define q:string (q #:type string-quad
                    #:in 'bi
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
                               (font doc (path->string (quad-ref q font-path-key)))
                               (font-size doc (quad-ref q 'font-size 12))
                               (fill-color doc (quad-ref q 'color "black"))
                               (define str (unsafe-car (quad-elems q)))
                               (match-define (list x y) (quad-origin q))
                               (text doc str x y
                                     #:tracking (quad-ref q 'character-tracking 0)
                                     #:bg (quad-ref q 'bg #f)
                                     #:features (list (cons #"tnum" 1))
                                     #:link (quad-ref q 'link #f))))
                    #:draw-end (if draw-debug-string?
                                   (λ (q doc) (draw-debug q doc "#99f" "#ccf"))
                                   void)))

(define-runtime-path default-font-face "fonts/charter/charter.otf")
(define default-font-family "charter")
(define default-font-size 12)

(define current-doc (make-parameter #f))

(define (make-size-promise q [str-arg #f])
  (delay
    (define doc (current-doc))
    (define str
      (cond
        [str-arg]
        [(pair? (quad-elems q)) (unsafe-car (quad-elems q))]
        [else #false]))
    (define string-size
      (cond
        [str
         (font-size doc (quad-ref q 'font-size default-font-size))
         (font doc (path->string (quad-ref q font-path-key default-font-face)))
         (+ (string-width doc str
                          #:tracking (quad-ref q 'character-tracking 0))
            ;; add one more dose because `string-width` only adds it intercharacter,
            ;; and this quad will be adjacent to another
            ;; (so we need to account for the "inter-quad" space
            (quad-ref q 'character-tracking 0))]
        [else 0]))
    (list string-size (quad-ref q 'line-height (current-line-height doc)))))

(define (->string-quad q)
  (cond
    [(line-break? q) q]
    [else
     (struct-copy
      quad q:string
      [attrs (let ([attrs (quad-attrs q)])
               (hash-ref! attrs 'font-size default-font-size)
               attrs)]
      [elems (quad-elems q)]
      [size (make-size-promise q)])]))


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
(define dumb-hardcoded-value 372)
(define q:line (q #:size (pt dumb-hardcoded-value line-height)
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
(define q:line-spacer-unbreakable
  (struct-copy line-spacer q:line-spacer
               [attrs #:parent quad
                      (make-hasheq '((keep-lines . #true)))]))

(define softies (map string '(#\space #\- #\u00AD)))

(define (soft-break-for-line? q)
  (and (pair? (quad-elems q))
       (member (unsafe-car (quad-elems q)) softies)))

(define (consolidate-runs pcs ending-q)
  (for/fold ([runs empty]
             [pcs pcs]
             #:result (reverse runs))
            ([i (in-naturals)]
             #:break (empty? pcs))
    (match pcs
      [(cons (? string-quad? strq) rest)
       (define-values (run-pcs rest) (splitf-at pcs (λ (p) (same-run? strq p))))
       (define new-run (struct-copy quad q:string
                                    [attrs (quad-attrs strq)]
                                    [elems (merge-adjacent-strings (apply append (for/list ([pc (in-list run-pcs)])
                                                                                   (quad-elems pc))))]
                                    [size (delay (pt (for/sum ([pc (in-list run-pcs)])
                                                       (pt-x (size pc)))
                                                     (pt-y (size strq))))]))
       (values (cons new-run runs) rest)]
      [(cons first rest) (values (cons first runs) rest)])))

(define (render-hyphen qs ending-q)
  ;; naive handling of soft hyphen:
  ;; if soft hyphen cause the break, then append a printing hyphen to the end of the run.
  ;; this assumes that there is room for the hyphen on the line
  ;; and does not take into account hyphen-break transformations found in other languages.
  ;; However we do want the hyphen joined into the string so the final shaping / positioning is correct
  ;; for instance, kerning between last letter and hyphen.
  (match (and ending-q (equal? (quad-elems ending-q) '("\u00AD")) qs)
    [(list head ... last-q)
     (define str+hyphen (string-append (car (quad-elems last-q)) "-"))
     (append head
             (list (struct-copy quad last-q
                                [elems (list str+hyphen)]
                                [size (make-size-promise last-q str+hyphen)])))]
    [_ qs]))

(define-quad line-break quad ())
(define lbr (make-line-break #:printable #f))
;; treat paragraph break as special kind of line break
(define-quad para-break line-break ())
(define pbr (make-para-break #:printable #f))
(define-quad hr-break line-break ())
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
                        background-color
                        keep-lines keep-first keep-last keep-all keep-with-next
                        line-align line-align-last first-line-indent
                        line-wrap))
  (for* ([k (in-list block-attrs)]
         [v (in-value (hash-ref source-hash k #f))]
         #:when v)
    (hash-set! dest-hash k v))
  dest-hash)

(define (handle-hyphenate qs)
  ;; find quads that want hyphenation and split them into smaller pieces
  ;; do this before ->string-quad so that it can handle the sizing promises
  (apply append (for/list ([q (in-list qs)])
                  (match (quad-ref q 'hyphenate #false)
                    [(or #false "false") (list q)]
                    [_ (for*/list ([str (in-list (quad-elems q))]
                                   [hyphen-char (in-value #\u00AD)]
                                   [hstr (in-value (hyphenate str hyphen-char
                                                              #:min-left-length 4
                                                              #:min-right-length 3))]
                                   [substr (in-list (regexp-match* (regexp (string hyphen-char)) hstr #:gap-select? #t))])
                         (struct-copy quad q [elems (list substr)]))]))))

(require sugar/list)
(define-quad filler quad ())
(define (fill-wrap qs ending-q)
  (match (and (pair? qs) (quad-ref (car qs) (if ending-q
                                                'line-align
                                                'line-align-last) "left"))
    [(or #false "left") qs] ; default is left aligned, no filling needed 
    [align-value
     (define word-sublists (filter-split qs (λ (q) (match (quad-elems q)
                                                     [(cons " " _) #true]
                                                     [_ #false]))))
     (match (length word-sublists)
       [1 #:when (equal? align-value "justify") qs] ; can't justify single word
       [word-count
        (match-define (list line-width line-height) (quad-size q:line))
        ;; words may still be in hyphenated fragments
        ;; (though soft hyphens would have been removed)
        ;; so group them (but no need to consolidate — that happens elsewhere)
        (define occupied-width (match align-value
                                 ;; for justified line, we care about size of words without spaces
                                 ["justify" (for*/sum ([word-sublist (in-list word-sublists)]
                                                       [word (in-list word-sublist)])
                                              (pt-x (size word)))]
                                 ;; for others, we care about size with spaces
                                 [_ (for/sum ([q (in-list qs)])
                                      (pt-x (size q)))]))
        (define empty-hspace (- line-width
                                (quad-ref (car qs) 'inset-left 0)
                                occupied-width
                                (quad-ref (car qs) 'inset-right 0)))
        (match align-value
          ["justify" 
           (define space-width (/ empty-hspace (sub1 word-count)))
           (apply append (add-between word-sublists (list (make-quad #:size (pt space-width line-height)))))]
          [_
           (define space-multiplier (match align-value
                                      ["center" 0.5]
                                      ["right" 1]))
           (cons (make-quad #:type filler
                            #:size (pt (* empty-hspace space-multiplier) line-height)
                            #:attrs (quad-attrs (car qs))) qs)])])]))

(define-quad offsetter quad ())

(define (finish-line-wrap pcs-in opening-q ending-q idx)
  ;; remove unused soft hyphens so they don't affect final shaping
  (define pcs-printing
    (for/list ([pc (in-list pcs-in)]
               #:unless (equal? (quad-elems pc) '("\u00AD")))
      pc))
  (append
   (cond
     [(empty? pcs-printing) null]
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
      ;; render hyphen first so that all printable characters are available for size-dependent ops.
      (define pcs-with-hyphen (render-hyphen pcs-printing ending-q))
      ;; fill wrap so that consolidate-runs works properly
      ;; (justified lines won't be totally consolidated)
      (define pcs (fill-wrap pcs-with-hyphen ending-q))
      (match (consolidate-runs pcs ending-q)
        [(? pair? elems)
         (define elem (unsafe-car elems))
         (match-define (list line-width line-height) (quad-size q:line))
         (define new-size (let ()
                            (define line-heights
                              (filter-map (λ (q) (quad-ref q 'line-height)) pcs))
                            (pt line-width (if (empty? line-heights) line-height (apply max line-heights)))))
         (list
          (struct-copy
           quad q:line
           ;; move block attrs up, so they are visible in page wrap
           [attrs (copy-block-attrs (quad-attrs elem)
                                    (hash-copy (quad-attrs q:line)))]
           ;; line width is static
           ;; line height is the max 'line-height value or the natural height of q:line
           [size new-size]
           ;; handle list indexes. drop new quad into line to hold list index
           ;; could also use this for line numbers
           [elems
            (append
             (match (and (eq? idx 1) (quad-ref elem 'list-index))
               [#false null]
               [bullet
                (define bullet-indent (* 3 (quad-ref elem 'font-size default-font-size)))
                (list (make-quad
                       #:elems (list
                                (struct-copy quad (car elems)
                                             [elems (list (if (number? bullet)
                                                              (format "~a." bullet)
                                                              bullet))]))))
                (list (struct-copy quad elem
                             [elems (list (if (number? bullet)
                                              (format "~a." bullet)
                                              bullet))]
                             [size (pt bullet-indent 0)]))])
             (list (make-quad
                    #:type offsetter
                    #:offset (pt (quad-ref elem 'inset-left 0) 0)
                    #:elems elems)))]))]
        [_ null])])
   (cond
     [ending-q null]
     [else (list q:line-spacer)])))

(define (line-wrap qs wrap-size)
  (apply append
         ;; next line removes all para-break? quads as a consequence
         (for/list ([qs (in-list (filter-split qs para-break?))])
           (wrap qs
                 (λ (q idx) (- wrap-size (quad-ref q 'inset-left 0) (quad-ref q 'inset-right 0)))
                 #:nicely (match (and (pair? qs) (quad-ref (car qs) 'line-wrap #false))
                            [(or #false "false") #false]
                            [(or "best" "kp") #true])
                 #:hard-break line-break?
                 #:soft-break soft-break-for-line?
                 #:finish-wrap finish-line-wrap))))

(define (make-nobreak! q) (quad-set! q 'no-pbr "true"))

(define (do-keep-with-next! reversed-lines)
  ;; paints nobreak onto spacers that follow keep-with-next lines
  ;; (we are iterating backward, so the geometrically previous ln follows the spacer)
  (for ([this-ln (in-list reversed-lines)]
        [prev-ln (in-list (cdr reversed-lines))]
        #:when (and (line-spacer? this-ln)
                    (quad-ref prev-ln 'keep-with-next)))
    (make-nobreak! prev-ln)
    (make-nobreak! this-ln)))

(define (apply-keeps lines)
  (define groups-of-lines (contiguous-group-by (λ (x) (quad-ref x 'display)) lines))
  (for*/fold ([reversed-lines null]
              #:result (begin
                         (do-keep-with-next! reversed-lines)
                         (reverse reversed-lines)))
             ([group (in-list groups-of-lines)]
              [group-len (in-value (length group))]
              [(ln idx0) (in-indexed group)])
    (define idx (add1 idx0))
    ;; always catch last line of block in this case
    ;; so later cases are guaranteed to have earlier lines.
    (unless (eq? idx group-len)
      (cond
        ;; if we have 'keep-all we can skip 'keep-first and 'keep-last cases
        [(quad-ref ln 'keep-all) (make-nobreak! ln)]
        ;; to keep n lines, we only paint the first n - 1
        ;; (because each nobr line sticks to the next)
        [(let ([keep-first (quad-ref ln 'keep-first)])
           (and (number? keep-first) (< idx keep-first)))
         (make-nobreak! ln)]
        [(let ([keep-last (quad-ref ln 'keep-last)])
           (and (number? keep-last) (< (- group-len keep-last) idx)))
         (make-nobreak! ln)]))
    (cons ln reversed-lines)))

(define zoom-mode? #f)
(define zoom-scale 2)
(define top-margin (/ 60 (if zoom-mode? zoom-scale 1)))
(define bottom-margin (/ 120 (if zoom-mode? zoom-scale 1)))
(define side-margin (/ 120 (if zoom-mode? zoom-scale 1)))
(define page-offset (pt (/ side-margin (if zoom-mode? 3 1))
                        (/ top-margin (if zoom-mode? 3 1))))
(require racket/date)
(define q:page (q #:offset page-offset
                  #:draw-start (λ (q doc) (add-page doc)
                                 (scale doc (if zoom-mode? zoom-scale 1) (if zoom-mode? zoom-scale 1)))
                  #:draw-end (λ (q doc)
                               (font-size doc 10)
                               (font doc default-font-face)
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
                    (match-define (list left top) (pt+ (quad-origin q) (list bil bit)))
                    (match-define (list width height) (pt- (size q) (list (+ bil bir) (+ bit bib))))
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
  ;; on timing of `insert-blocks`:
  ;; can't do it before because it depends on where pages are broken.
  ;; could do it after, but it would require going back inside each page quad
  ;; which seems overly interdependent, because `insert-blocks` is used to determine break locations.
  ;; `page-wrap` should emit quads that are complete.
  (wrap xs vertical-height
        #:soft-break (λ (q) #true)
        #:no-break (λ (q) (quad-ref q 'no-pbr))
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
               [#false line-group]
               [_ (list (block-wrap line-group))]))))

(define font-paths (make-hash))

(define (setup-font-path-table! base-path)
  ;; populate `font-paths` table with font paths
  ;; search "fonts" subdirectory in project for other subdirectories
  ;; which are presumed to contain fonts.
  ;; and link them to their family names & styles.
  ;; this allows a flexible mapping from internal to external names, like @font-face
  ;; note that all the semantics are derived from the file system
  ;; not any metadata fields within the font.
  ;; this is faster and easier, because you can just muck with the directory and filenames
  ;; to change the font mapping.
  ;; though it also creates the potential for mischief,
  ;; if a font is named something that doesn't reflect its visual reality.
  ;; but we are not the font police.
  (define-values (dir path _) (split-path base-path))
  (define fonts-dir (build-path dir "fonts"))
  (for* ([font-family-subdir (in-directory fonts-dir)]
         #:when (directory-exists? font-family-subdir)
         [font-path (in-directory font-family-subdir)]
         #:when (or (path-has-extension? font-path #"otf")
                    (path-has-extension? font-path #"ttf")))
    (match-define (list font-path-string family-name)
      (map (λ (x) (path->string (find-relative-path fonts-dir x))) (list font-path font-family-subdir)))
    (define key
      (cons family-name
            (match (string-downcase font-path-string)
              [(and (regexp "bold") (regexp "italic")) 'bi]
              [(regexp "bold") 'b]
              [(regexp "italic") 'i]
              [_ 'r])))
    ;; only set value if there's not one there already.
    ;; this means that we only use the first eligible font we find.
    (hash-ref! font-paths key font-path)))

(define (font-attrs->path font-family bold italic)
  ;; find the font-path corresponding to a certain family name and style.
  (define key (cons font-family
                    (cond
                      [(and bold italic) 'bi]
                      [bold 'b]
                      [italic 'i]
                      [else 'r])))
  (define regular-key (cons font-family 'r))
  (cond
    [(hash-ref font-paths key #false)]
    ;; if there isn't one, try the regular style.
    [(hash-ref font-paths regular-key #false)]
    ;; If there isn't one, use the default.
    [else default-font-face]))

(define (resolve-font-path attrs)
  (define this-font-family (hash-ref! attrs 'font-family default-font-family))
  (define this-bold (hash-ref! attrs 'font-bold #false))
  (define this-italic (hash-ref! attrs 'font-italic #false))
  (hash-set! attrs 'font-path (font-attrs->path this-font-family this-bold this-italic)))

(define (parse-percentage pstr)
  (/ (string->number (string-trim pstr "%")) 100.0))

(define (resolve-font-size attrs)
  (define this-font-size (hash-ref! attrs 'font-size default-font-size))
  (define this-font-size-adjust (parse-percentage (hash-ref! attrs 'font-size-adjust "100%")))
  ;; we bake the adjustment into the font size...
  (hash-set! attrs 'font-size (* this-font-size this-font-size-adjust))
  ;; and then set the adjustment back to 100% (since it's now accounted for)
  (hash-set! attrs 'font-size-adjust "100%"))

(define (handle-cascading-attrs attrs)
  (resolve-font-path attrs)
  (resolve-font-size attrs))

(define-quad first-line-indent quad ())

(define (insert-first-line-indents qs-in)
  ;; first line indents are quads inserted at the beginning of a paragraph
  ;; (that is, just after a paragraph break)
  ;; they need to be installed before line wrap
  ;; to be compatible with first-fit and best-fit.
  (for/fold ([qs-out null]
             #:result (reverse qs-out))
            ([q (in-list qs-in)]
             [next-q (in-list (cdr qs-in))])
    (match (and (para-break? q) (quad-ref next-q 'first-line-indent 0))
      [(or #false 0) (cons next-q qs-out)]
      [indent-val (list* next-q (make-quad #:type first-line-indent
                                           #:attrs (quad-attrs next-q)
                                           #:size (pt indent-val 0)) qs-out)])))
                 
(define (run xs pdf-path)
  (define pdf (time-name make-pdf (make-pdf #:compress #t
                                            #:auto-first-page #f
                                            #:output-path pdf-path
                                            #:width (if zoom-mode? 350 612)
                                            #:height (if zoom-mode? 400 792))))
  (define line-width (- (pdf-width pdf) (* 2 side-margin)))
  (define vertical-height (- (pdf-height pdf) top-margin bottom-margin))
  (setup-font-path-table! pdf-path)
  (parameterize ([current-doc pdf]
                 [verbose-quad-printing? #false])
    (let* ([x (time-name parse-qexpr (qexpr->quad xs))]
           [x (time-name atomize (atomize x #:attrs-proc handle-cascading-attrs))]
           [x (time-name hyphenate (handle-hyphenate x))]
           [x (time-name ->string-quad (map ->string-quad x))]
           [x (time-name insert-first-line-indents (insert-first-line-indents x))]  
           [x (time-name line-wrap (line-wrap x line-width))]
           [x (time-name apply-keeps (apply-keeps x))]
           [x (time-name page-wrap (page-wrap x vertical-height pdf-path))]
           [x (time-name position (position (struct-copy quad q:doc [elems x])))])
      (time-name draw (draw x pdf)))))

(define-syntax (mb stx)
  (syntax-case stx ()
    [(_ PDF-PATH . STRS)
     #'(#%module-begin
        ;; stick an nbsp in the strings so we have one printing char
        (define strs (match (list . STRS)
                       [(? null?) '(" ")]
                       [strs strs]))
        (define qx (root null (add-between strs (list pbr)
                                           #:before-first (list pbr)
                                           #:after-last (list pbr)
                                           #:splice? #true)))
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
    (define parsed-stxs (datum->syntax stx (xexpr->parse-tree (parse-markdown (apply string-append (syntax->datum stx))))))
    (strip-context
     (with-syntax ([STXS parsed-stxs]
                   [PDF-PATH (path-replace-extension path-string #".pdf")])
       #'(module _ qtest/markdown
           PDF-PATH
           . STXS)))))
