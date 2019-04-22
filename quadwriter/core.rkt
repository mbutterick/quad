#lang debug racket/base
(require (for-syntax)
         racket/promise
         racket/match
         racket/list
         sugar/list
         racket/date
         pitfall
         quad
         sugar/debug
         racket/unsafe/ops
         hyphenate
         "attrs.rkt"
         "param.rkt"
         "font.rkt")
(provide hrbr lbr pbr render-pdf)

(define-quad string-quad quad ())
 
(define (q:string-draw q doc)
  ;; draw with pdf text routine  (when (pair? (quad-elems q))
  (font doc (path->string (quad-ref q font-path-key default-font-face)))
  (font-size doc (quad-ref q 'font-size 12))
  (fill-color doc (quad-ref q 'color "black"))
  (define str (unsafe-car (quad-elems q)))
  (match-define (list x y) (quad-origin q))
  (text doc str x y
        #:tracking (quad-ref q 'character-tracking 0)
        #:bg (quad-ref q 'bg)
        #:features (list (cons #"tnum" 1))
        #:link (quad-ref q 'link)))

(define (q:string-draw-end q doc)
  (when (draw-debug-string?)
    (draw-debug q doc "#99f" "#ccf")))

(define (q:string-printable? q [sig #f])
  ;; printable unless single space, which is not printable at start or end
  (match (quad-elems q)
    [(cons elem _)
     (case elem
       [(" " #\space) (not (memq sig '(start end)))]
       [else #true])]
    [_ #true]))

(define q:string (q #:type string-quad
                    #:in 'baseline-in
                    #:out 'baseline-out
                    #:printable q:string-printable?
                    #:draw q:string-draw
                    #:draw-end q:string-draw-end))

(define (make-size-promise q [str-arg #f])
  (delay
    (define pdf (current-pdf))
    (define str
      (cond
        [str-arg]
        [(pair? (quad-elems q)) (unsafe-car (quad-elems q))]
        [else #false]))
    (define string-size
      (cond
        [str
         (font-size pdf (quad-ref q 'font-size default-font-size))
         (font pdf (path->string (quad-ref q font-path-key default-font-face)))
         (+ (string-width pdf str
                          #:tracking (quad-ref q 'character-tracking 0))
            ;; add one more dose because `string-width` only adds it intercharacter,
            ;; and this quad will be adjacent to another
            ;; (so we need to account for the "inter-quad" space
            (quad-ref q 'character-tracking 0))]
        [else 0]))
    (list string-size (quad-ref q 'line-height (current-line-height pdf)))))

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
  (when (draw-debug?)
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

(define q:line (q #:size (pt 0 default-line-height)
                  #:inner 'sw
                  #:out 'sw
                  #:printable #true
                  #:draw-start (if draw-debug-line? draw-debug void)))

(struct line-spacer quad () #:transparent)
(define q:line-spacer (q #:type line-spacer
                         #:size (pt 0 (* default-line-height 0.6))
                         #:out 'sw
                         #:printable (λ (q sig) (not (memq sig '(start end))))
                         #:draw-start (if (draw-debug-line?) draw-debug void)))

(define q:line-spacer-unbreakable
  (struct-copy line-spacer q:line-spacer
               [attrs #:parent quad
                      (make-hasheq '((keep-lines . #true)))]))

(define softies (map string '(#\space #\- #\u00AD)))

(define (soft-break-for-line? q)
  (and (pair? (quad-elems q))
       (member (unsafe-car (quad-elems q)) softies)))

(define (consolidate-runs pcs ending-q)
  (let loop ([runs empty][pcs pcs])
    (match pcs
      [(? empty?) (reverse runs)]
      [(cons (? string-quad? strq) rest)
       (define-values (run-pcs rest) (splitf-at pcs (λ (p) (same-run? strq p))))
       (define new-run (struct-copy quad q:string
                                    [attrs (quad-attrs strq)]
                                    [elems (merge-adjacent-strings (apply append (for/list ([pc (in-list run-pcs)])
                                                                                   (quad-elems pc))))]
                                    [size (delay (pt (for/sum ([pc (in-list run-pcs)])
                                                       (pt-x (size pc)))
                                                     (pt-y (size strq))))]))
       (loop (cons new-run runs) rest)]
      [(cons first rest) (loop (cons first runs) rest)])))

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
  (for* ([k (in-list block-attrs)]
         [v (in-value (hash-ref source-hash k #f))]
         #:when v)
    (hash-set! dest-hash k v))
  dest-hash)

(define (handle-hyphenate qs)
  ;; find quads that want hyphenation and split them into smaller pieces
  ;; do this before ->string-quad so that it can handle the sizing promises
  (apply append
         (for/list ([q (in-list qs)])
           (match (quad-ref q 'hyphenate)
             [(or #false "false") (list q)]
             [_ (for*/list ([str (in-list (quad-elems q))]
                            [hyphen-char (in-value #\u00AD)]
                            [hstr (in-value (hyphenate str hyphen-char
                                                       #:min-left-length 4
                                                       #:min-right-length 3))]
                            [substr (in-list (regexp-match* (regexp (string hyphen-char)) hstr #:gap-select? #t))])
                  (struct-copy quad q [elems (list substr)]))]))))

(define-quad filler quad ())
(define (fill-wrap qs ending-q line-q)
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
        (match-define (list line-width line-height) (quad-size line-q))
        ;; words may still be in hyphenated fragments
        ;; (though soft hyphens would have been removed)
        ;; so group them (but no need to consolidate — that happens elsewhere)
        (define occupied-width
          (match align-value
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

(define (hr-draw dq doc)
  (match-define (list left top) (quad-origin dq))
  (match-define (list right bottom) (size dq))
  (save doc)
  (translate doc left (+ top (/ bottom 2)))
  (move-to doc 0 0)
  (line-to doc right 0)
  (line-width doc 0.5)
  (stroke doc "black")
  (restore doc))

(define (make-hr-quad line-q)
  (struct-copy quad line-q [draw-start hr-draw]))

(define ((finish-line-wrap line-q) pcs-in opening-q ending-q idx)
  ;; we curry line-q so that the wrap size can be communicated to this operation
  ;; remove unused soft hyphens so they don't affect final shaping
  (define pcs-printing (for/list ([pc (in-list pcs-in)]
                                  #:unless (equal? (quad-elems pc) '("\u00AD")))
                         pc))
  (append
   (cond
     [(empty? pcs-printing) null]
     [(hr-break? ending-q) (list (make-hr-quad line-q))]
     [else
      ;; render hyphen first so that all printable characters are available for size-dependent ops.
      (define pcs-with-hyphen (render-hyphen pcs-printing ending-q))
      ;; fill wrap so that consolidate-runs works properly
      ;; (justified lines won't be totally consolidated)
      (define pcs (fill-wrap pcs-with-hyphen ending-q line-q))
      (match (consolidate-runs pcs ending-q)
        [(? pair? elems)
         (define elem (unsafe-car elems))
         (match-define (list line-width line-height) (quad-size line-q))
         (define new-size (let ()
                            (define line-heights
                              (filter-map (λ (q) (quad-ref q 'line-height)) pcs))
                            (pt line-width (if (empty? line-heights) line-height (apply max line-heights)))))
         (list
          (struct-copy
           quad line-q
           ;; move block attrs up, so they are visible in page wrap
           [attrs (copy-block-attrs (quad-attrs elem)
                                    (hash-copy (quad-attrs line-q)))]
           ;; line width is static
           ;; line height is the max 'line-height value or the natural height of q:line
           [size new-size]
           ;; handle list indexes. drop new quad into line to hold list index
           ;; could also use this for line numbers
           [elems
            ;; we assume here that a list item has already had extra inset-left
            ;; with room for a bullet
            ;; which we just insert at the front.
            ;; this is safe because line has already been filled.
            (append
             ;; only put bullet into line if we're at the first line of the list item
             (match (and (eq? idx 1) (quad-ref elem 'list-index))
               [#false null]
               [bullet
                (list (struct-copy
                       quad q:string ;; copy q:string to get draw routine
                       ;; borrow attrs from elem
                       [attrs (quad-attrs elem)]
                       ;; use bullet as elems
                       [elems (list (if (number? bullet) (format "~a." bullet) bullet))]
                       ;; no size because it's inside inset
                       [size (pt 0 0)]))])
             (list (make-quad
                    #:type offsetter
                    #:offset (pt (quad-ref elem 'inset-left 0) 0)
                    #:elems elems)))]))]
        [_ null])])
   (cond
     [ending-q null]
     [else (list q:line-spacer)])))

(define (line-wrap qs wrap-size)
  (define line-q (struct-copy
                  quad q:line
                  [size (pt wrap-size (pt-y (size q:line)))]))
  (apply append
         ;; next line removes all para-break? quads as a consequence
         (for/list ([qs (in-list (filter-split qs para-break?))])
           (wrap qs
                 (λ (q idx) (- wrap-size (quad-ref q 'inset-left 0) (quad-ref q 'inset-right 0)))
                 #:nicely (match (and (pair? qs) (quad-ref (car qs) 'line-wrap))
                            [(or "best" "kp") #true]
                            [_ #false])
                 #:hard-break line-break?
                 #:soft-break soft-break-for-line?
                 #:finish-wrap (finish-line-wrap line-q)))))

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

(define (page-draw-start q doc)
  (add-page doc)
  (scale doc (if zoom-mode? zoom-scale 1) (if zoom-mode? zoom-scale 1)))

(define (page-draw-end q doc)
  (font-size doc 10)
  (font doc default-font-face)
  (fill-color doc "black")
  (text doc (format "~a · ~a at ~a" (hash-ref (quad-attrs q) 'page-number)
                    (hash-ref (quad-attrs q) 'doc-title)
                    (date->string (current-date) #t))
        side-margin
        (+ (- (pdf-height doc) bottom-margin) 20)))

(define q:page (q #:offset page-offset
                  #:draw-start page-draw-start
                  #:draw-end page-draw-end))

(define q:doc (q #:draw-start (λ (q doc) (start-doc doc))
                 #:draw-end (λ (q doc) (end-doc doc))))

(define ((block-draw-start first-line) q doc)
  ;; adjust drawing coordinates for border inset
  (match-define (list bil bit bir bib)
    (for/list ([k (in-list '(border-inset-left border-inset-top border-inset-right border-inset-bottom))])
      (quad-ref first-line k 0)))
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
     #:draw-start (block-draw-start first-line)
     #:draw-end (if (draw-debug-block?)
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

(define ((page-finish-wrap path) lns q0 q idx)
  (list (struct-copy quad q:page
                     [attrs (let ([page-number idx]
                                  [h (hash-copy (quad-attrs q:page))])
                              (hash-set! h 'page-number page-number)
                              (define-values (dir name _)
                                (split-path (path-replace-extension path #"")))
                              (hash-set! h 'doc-title (string-titlecase (path->string name)))
                              h)]
                     [elems (insert-blocks lns)])))

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
        #:finish-wrap (page-finish-wrap path)))

(define (insert-blocks lines)
  (define groups-of-lines (contiguous-group-by (λ (x) (quad-ref x 'display)) lines))
  (append* (for/list ([line-group (in-list groups-of-lines)])
             (if (quad-ref (car line-group) 'display)
                 (list (block-wrap line-group))
                 line-group))))

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
                 
(define (render-pdf xs pdf-path)
  (define pdf (make-pdf #:compress #t
                                            #:auto-first-page #f
                                            #:output-path pdf-path
                                            #:width (if zoom-mode? 350 612)
                                            #:height (if zoom-mode? 400 792)))
  (define line-width (- (pdf-width pdf) (* 2 side-margin)))
  (define vertical-height (- (pdf-height pdf) top-margin bottom-margin))
  (parameterize ([current-pdf pdf]
                 [verbose-quad-printing? #false])
    (setup-font-path-table! pdf-path)
    (let* ([x (qexpr->quad  `(q ((font-family ,default-font-family)
                                            (font-size ,(number->string default-font-size))) ,xs))]
           [x (atomize x #:attrs-proc handle-cascading-attrs)]
           [x (time-name hyphenate (handle-hyphenate x))]
           [x (map ->string-quad x)]
           [x (insert-first-line-indents x)]  
           [x (time-name line-wrap (line-wrap x line-width))]
           [x (apply-keeps x)]
           [x (time-name page-wrap (page-wrap x vertical-height pdf-path))]
           [x (time-name position (position (struct-copy quad q:doc [elems x])))])
      (time-name draw (draw x pdf))
      (displayln (format "wrote PDF to ~a" pdf-path)))))
