#lang debug racket/base
(require racket/promise
         racket/match
         racket/list
         sugar/list
         txexpr/base
         racket/date
         pitfall
         quad
         racket/unsafe/ops
         "attrs.rkt"
         "param.rkt"
         "font.rkt"
         "log.rkt")
(provide (all-defined-out))

(define (sum-base xs which)
  (for/sum ([x (in-list xs)])
    (which (size x))))
(define (sum-y xs) (sum-base xs pt-y))
(define (sum-x xs) (sum-base xs pt-x))

(define-quad string-quad quad)
 
(define (q:string-draw q doc
                       #:origin [origin-in #f]
                       #:text [str-in #f])
  (match (or str-in (and (pair? (quad-elems q)) (unsafe-car (quad-elems q))))
    [#false (void)]
    [str 
     (font doc (path->string (quad-ref q font-path-key default-font-face)))
     (font-size doc (quad-ref q :font-size default-font-size))
     (fill-color doc (quad-ref q :font-color default-font-color))
     (match-define (list x y) (or origin-in (quad-origin q)))
     (text doc str x (- y  (quad-ref q :font-baseline-shift 0))
           #:tracking (quad-ref q :font-tracking 0)
           #:bg (quad-ref q :bg)
           #:features (quad-ref q :font-features default-font-features)
           #:link (quad-ref q :link))]))

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
                    #:from 'bo
                    #:to 'bi
                    #:id 'str
                    #:printable q:string-printable?
                    #:draw q:string-draw
                    #:draw-end q:string-draw-end))

(define-quad image-quad quad)

(define (q:image-draw q doc)
  (define img (quad-ref q :image-object))
  (match-define (list x y) (quad-origin q))
  (match-define (list w h) (size q))
  (image doc img x y
         #:width w
         #:height h))

(define (q:image-draw-end q doc)
  (when (draw-debug-image?)
    (draw-debug q doc "orange" "orange")))

(define q:image (q #:type image-quad
                   #:from 'bo
                   #:to 'bi
                   #:id 'image
                   #:printable #true
                   #:draw q:image-draw
                   #:draw-end q:image-draw-end))

(define (make-size-promise q [str-arg #f])
  (delay
    (define pdf (current-pdf))
    (define str (cond
                  [str-arg]
                  [(pair? (quad-elems q)) (unsafe-car (quad-elems q))]
                  [else #false]))
    (define string-size
      (cond
        [str
         (font-size pdf (quad-ref q :font-size default-font-size))
         (font pdf (path->string (quad-ref q font-path-key default-font-face)))
         (define ft-value (quad-ref q :font-tracking 0))
         (match str
           ["\u00AD" ft-value]
           [_ (+ (string-width pdf str
                               #:tracking ft-value
                               #:features (quad-ref q :font-features default-font-features)))])]
        [else 0]))
    (list string-size (quad-ref q :line-height default-line-height))))

(define (maybe-convert-draw-quad q)
  (define draw-type (quad-ref q :draw))
  (and draw-type 
       (quad-update! q
                     [draw (λ (q doc)
                             (save doc)
                             (match draw-type
                               ["line" 
                                (move-to doc (quad-ref q :x1) (quad-ref q :y1))
                                (line-to doc (quad-ref q :x2) (quad-ref q :y2))
                                (stroke doc "black")]
                               ["text" (move-to doc 0 0)
                                       (q:string-draw q doc
                                                      #:origin (pt (quad-ref q :x 0) (quad-ref q :y 0))
                                                      #:text (quad-ref q :text))]
                               [_ (void)])
                             (restore doc))]
                     [size (pt 0 0)])))

(define (maybe-convert-image-quad q)
  (define path-string (quad-ref q :image-file))
  (and path-string
       (let ()
         (unless (file-exists? path-string)
           (raise-argument-error 'create-image-quad "image path that exists" path-string))
         (define img-obj (open-image (current-pdf) path-string))
         (define img-width ($img-width img-obj))
         (define img-height ($img-height img-obj))
         (match-define (list layout-width layout-height)
           (match (list (quad-ref q :image-width) (quad-ref q :image-height)) 
             [(list (? number? w) (? number? h)) (list w h)]
             [(list #false (? number? h))
              (define ratio (/ h img-height))
              (list (* ratio img-width) h)]
             [(list (? number? w) #false)
              (define ratio (/ w img-width))
              (list w (* ratio img-height))]
             [(list #false #false) (list img-width img-height)]))
         (struct-copy
          image-quad q:image
          [attrs #:parent quad (let ([h (hash-copy (quad-attrs q))])
                                 ;; defeat 'bi 'bo positioning by removing font reference
                                 (hash-set! h font-path-key #false)
                                 ;; save the img-obj for later
                                 (hash-set! h :image-object img-obj)
                                 h)]
          [size #:parent quad (pt layout-width layout-height)]))))

(define (maybe-convert-line-break-quad q)
  (and (line-break-quad? q) q))

(define (do-string-quad q)
  (struct-copy
   string-quad q:string
   [attrs #:parent quad (let ([attrs (quad-attrs q)])
                          (hash-ref! attrs :font-size default-font-size)
                          attrs)]
   [elems #:parent quad (quad-elems q)]
   [size #:parent quad (make-size-promise q)]))

(define (generic->typed-quad q)
  (or
   (maybe-convert-line-break-quad q)
   (maybe-convert-draw-quad q)
   (maybe-convert-image-quad q)
   (do-string-quad q)))


(define (draw-debug q doc [fill-color "#f99"] [stroke-color "#fcc"] [stroke-width 0.5])
  (when (draw-debug?)
    (save doc)
    ;; draw layout box
    (line-width doc stroke-width)
    ; subtracting stroke-width keeps adjacent boxes from overlapping
    (save doc)
    (apply rect doc (append (pt+ (quad-origin q)) (map (λ (x) (- x stroke-width)) (size q))))
    (clip doc)
    (define pt (to-point q))
    (circle doc (pt-x pt) (pt-y pt) (+ 3 stroke-width))
    (fill doc fill-color)
    (restore doc)
    (apply rect doc (append (pt+ (quad-origin q)) (map (λ (x) (- x stroke-width)) (size q))))
    (stroke doc stroke-color)
    (restore doc)))


(define-quad line-break-quad quad)
(define q:line-break (make-line-break-quad #:printable #f
                                           #:id 'line-break))
(define-quad para-break-quad line-break-quad)
(define q:para-break (make-para-break-quad #:printable #f
                                           #:id 'para-break))
(define-quad hr-break-quad line-break-quad)
(define q:hr-break (make-hr-break-quad #:printable #t
                                       #:id 'hr-break))
(define-quad column-break-quad line-break-quad)
(define q:column-break (make-column-break-quad #:printable #f
                                               #:id 'column-break))
(define-quad page-break-quad column-break-quad)
(define q:page-break (make-page-break-quad #:printable #f
                                           #:id 'page-break))

(define-quad section-break-quad page-break-quad)
(define q:section-break (make-section-break-quad #:printable #f
                                                 #:id 'section-break))

(define q:line (q #:size (pt 0 default-line-height)
                  #:from 'sw
                  #:to 'nw
                  #:printable #true
                  #:id 'line
                  #:draw-start (if draw-debug-line? draw-debug void)))

(define-quad line-spacer-quad line-break-quad)

(define only-prints-in-middle (λ (q sig) (not (memq sig '(start end)))))
(define (make-paragraph-spacer maybe-first-line-q key default-val)
  (define arbitrary-width 20)
  (q #:type line-spacer-quad
     #:size (pt arbitrary-width (cond
                                  [(and maybe-first-line-q (quad-ref maybe-first-line-q key))]
                                  [else default-val]))
     #:from 'sw
     #:to 'nw
     #:printable only-prints-in-middle
     #:draw-start (if (draw-debug-line?) draw-debug void)))

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
       (define new-run (quad-copy q:string
                                  [attrs (quad-attrs strq)]
                                  [elems (merge-adjacent-strings (apply append (map quad-elems run-pcs)))]
                                  [size (delay (pt (sum-x run-pcs) (pt-y (size strq))))]))
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
     (define str (car (quad-elems last-q)))
     (define str+hyphen (string-append str "-"))
     (append head
             (list (quad-update! last-q
                                 [elems (list str+hyphen)]
                                 [size (make-size-promise last-q str+hyphen)])))]
    [_ qs]))


(module+ test
  (require rackunit)
  (check-true (line-break-quad? (second (quad-elems (q "foo" q:page-break "bar")))))
  (check-true (line-break-quad? (second (atomize (q "foo" q:page-break "bar"))))))

(define-quad filler-quad quad)

(define (space-quad? q) (equal? (quad-elems q) (list " ")))

(define (fill-line-wrap qs ending-q line-q)
  (match (and (pair? qs)
              (quad-ref (car qs) (if ending-q
                                     :line-align
                                     :line-align-last) "left"))
    [align-value
     ;; words may still be in hyphenated fragments
     ;; (though soft hyphens would have been removed)
     ;; so group them (but no need to consolidate — that happens elsewhere)
     (define-values (word-space-sublists word-sublists) (partition* space-quad? qs))
     (match (length word-sublists)
       [1 #:when (equal? align-value "justify") qs] ; can't justify single word
       [word-count
        (match-define (list line-width line-height) (quad-size line-q))
        (define hung-word-sublists
          (match word-sublists
            [(list sublists ... (list prev-qs ... last-q))
             #:when (pair? (quad-elems last-q))
             (define last-char-str (regexp-match #rx"[.,:;’-]$" (car (quad-elems last-q))))
             (match last-char-str
               [#false word-sublists]
               [_ (define hanger-q (quad-copy last-q
                                              [elems null]
                                              [size (let ([p (make-size-promise last-q (car last-char-str))])
                                                      (delay
                                                        (match-define (list x y) (force p))
                                                        (pt (- x) y)))]))
                  (define last-sublist (append prev-qs (list last-q hanger-q)))
                  (append sublists (list last-sublist))])]
            [_ word-sublists]))
        (define word-width (for/sum ([qs (in-list hung-word-sublists)])
                             (sum-x qs)))
        (define word-space-width (for/sum ([qs (in-list word-space-sublists)])
                                   (sum-x qs)))
        (define empty-hspace (- line-width
                                (quad-ref (car qs) :inset-left 0)
                                word-width
                                (quad-ref (car qs) :inset-right 0)))
        (define line-overfull? (negative? (- empty-hspace word-space-width)))

        (cond
          [(or (equal? align-value "justify")
               ;; force justification upon overfull lines
               (and line-overfull? (> word-count 1)))
           (define justified-space-width (/ empty-hspace (sub1 word-count)))
           (apply append (add-between hung-word-sublists (list (make-quad
                                                                #:from 'bo
                                                                #:to 'bi
                                                                #:draw-end q:string-draw-end
                                                                #:size (pt justified-space-width line-height)))))]
          [else
           (define space-multiplier (match align-value
                                      ["left" 0]
                                      ["center" 0.5]
                                      ;; fill inner & outer as if they were right,
                                      ;; they will be corrected later, when pagination is known.
                                      [(or "right" "inner" "outer") 1]))
           ;; subtact space-width because that appears between words
           ;; we only care about redistributing the space on the ends
           (define end-hspace (- empty-hspace word-space-width))
           ;; make filler a leading quad, not a parent / grouping quad,
           ;; so that elements can still be reached by consolidate-runs
           (define fq (make-quad #:type filler-quad
                                 #:id 'line-filler
                                 #:from-parent (quad-from-parent (car qs))
                                 #:from 'bo
                                 #:to 'bi
                                 #:size (pt (* end-hspace space-multiplier) 0)
                                 #:attrs (quad-attrs (car qs))))
           (list* fq
                  (quad-update! (car qs) [from-parent #f])
                  (cdr qs))])])]))

(define-quad offsetter-quad quad)

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
  (quad-copy line-q [draw-start hr-draw]))

(define ((finish-line-wrap line-q) pcs-in opening-q ending-q idx)
  ;; we curry line-q so that the wrap size can be communicated to this operation
  ;; remove unused soft hyphens so they don't affect final shaping
  (define pcs-printing (for/list ([pc (in-list pcs-in)]
                                  #:unless (equal? (quad-elems pc) '("\u00AD")))
                         pc))
  (define new-lines
    (cond
      [(empty? pcs-printing) null]
      [(hr-break-quad? ending-q) (list (make-hr-quad line-q))]
      [else
       ;; render hyphen first so that all printable characters are available for size-dependent ops.
       (define pcs-with-hyphen (render-hyphen pcs-printing ending-q))
       ;; fill wrap so that consolidate-runs works properly
       ;; (justified lines won't be totally consolidated)
       (define pcs (fill-line-wrap pcs-with-hyphen ending-q line-q))
       (match (consolidate-runs pcs ending-q)
         [(? pair? elems)
          (define elem (unsafe-car elems))
          (match-define (list line-width line-height) (quad-size line-q))
          (define new-size (let ()
                             (define line-heights
                               (filter-map (λ (q) (or (quad-ref q :line-height) (pt-y (size q)))) pcs))
                             (pt line-width (if (empty? line-heights) line-height (apply max line-heights)))))
          (list
           (quad-copy line-q
                      ;; move block attrs up, so they are visible in col wrap
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
                        (match (and (eq? idx 1) (quad-ref elem :list-index))
                          [#false null]
                          [bullet
                           (define bq (quad-copy q:string ;; copy q:string to get draw routine
                                                 ;; borrow attrs from elem
                                                 [attrs (quad-attrs elem)]
                                                 ;; use bullet as elems
                                                 [elems (list (if (number? bullet) (format "~a." bullet) bullet))]
                                                 ;; size doesn't matter because nothing refers to this quad
                                                 ;; just for debugging box
                                                 [size (pt 15 (pt-y (size line-q)))]))
                           (from-parent (list bq) 'sw)])
                        (from-parent
                         (match (quad-ref elem :inset-left 0)
                           [0 elems]
                           [inset-val
                            (cons (make-quad
                                   #:draw-end q:string-draw-end
                                   #:to 'sw
                                   #:size (pt inset-val 5)
                                   #:type offsetter-quad)
                                  elems)]) 'sw))]))]
         [_ null])]))
  (define maybe-first-line (and (pair? new-lines) (car new-lines)))
  (append (match opening-q
            [#false (list (make-paragraph-spacer maybe-first-line :space-before 0))] ; paragraph break
            [_ null])
          new-lines
          (match ending-q
            [(? page-break-quad? page-break) (list page-break)] ; hard page break
            [#false (list (make-paragraph-spacer maybe-first-line :space-after (* default-line-height 0.6)))] ; paragraph break
            [_ null]))) ; hard line break
                       

(define (line-wrap qs wrap-size)
  (match qs
    [(? null?) null]
    [_
     (unless (positive? wrap-size)
       (raise-argument-error 'line-wrap "positive number" wrap-size))
     (define line-q (quad-copy q:line [size (pt wrap-size (quad-ref (car qs) :line-height default-line-height))]))
     (define permitted-justify-overfill
       (match (quad-ref (car qs) :line-align)
         ;; allow justified lines to go wider,
         ;; and then fill-wrap will tighten thes word spaces
         ;; this makes justified paragraphs more even, becuase
         ;; some lines are a little tight, as opposed to all of them being loose
         ["justify" 1.04]
         [_ 1]))
     (apply append
            ;; next line removes all para-break? quads as a consequence
            (for/list ([qs (in-list (filter-split qs para-break-quad?))])
              (wrap qs
                    (λ (q idx) (* (- wrap-size
                                     (quad-ref (car qs) :inset-left 0)
                                     (quad-ref (car qs) :inset-right 0))
                                  permitted-justify-overfill))
                    #:nicely (match (or (current-line-wrap) (quad-ref (car qs) :line-wrap))
                               [(or "best" "kp") #true]
                               [_ #false])
                    #:hard-break line-break-quad?
                    #:soft-break soft-break-for-line?
                    #:finish-wrap (finish-line-wrap line-q))))]))

(define (make-nobreak! q) (quad-set! q :no-colbr #true)) ; cooperates with col-wrap

(define (do-keep-with-next! reversed-lines)
  ;; paints nobreak onto spacers that follow keep-with-next lines
  ;; (we are iterating backward, so the geometrically previous ln follows the spacer)
  (match reversed-lines
    [(? null?) null]
    [_ (for ([this-ln (in-list reversed-lines)]
             [prev-ln (in-list (cdr reversed-lines))]
             #:when (and (line-spacer-quad? this-ln)
                         (quad-ref prev-ln :keep-with-next)))
         (make-nobreak! this-ln)
         (make-nobreak! prev-ln))]))

(define (apply-keeps lines)
  (define groups-of-lines (contiguous-group-by (λ (x) (quad-ref x :display)) lines))
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
      (when (or
             ;; if we have :keep-all we can skip :keep-first and :keep-last cases
             (quad-ref ln :keep-all-lines)
             ;; to keep n lines, we only paint the first n - 1
             ;; (because each nobr line sticks to the next)
             (let ([keep-first (quad-ref ln :keep-first-lines)])
               (and (number? keep-first) (< idx keep-first)))
             (let ([keep-last (quad-ref ln :keep-last-lines)])
               (and (number? keep-last) (< (- group-len keep-last) idx))))
        (make-nobreak! ln)))
    (cons ln reversed-lines)))


(define default-page-size "letter")
(define default-page-orientation "tall")
(define (parse-page-size q)
  ;; page size can be specified by name, or measurements.
  ;; explicit measurements from page-height and page-width supersede those from page-size.
  (match-define (list page-width page-height)
    (for/list ([k (list :page-width :page-height)])
      (and (quad? q) (match (quad-ref q k)
                       [#false #false]
                       [val (inexact->exact (floor val))]))))
  (resolve-page-size
   (or (debug-page-width) page-width)
   (or (debug-page-height) page-height)
   (quad-ref q :page-size default-page-size)
   (quad-ref q :page-orientation default-page-orientation)))

(define (page-draw-start q doc)
  (match-define (list page-width page-height) (parse-page-size q))
  (add-page doc page-width page-height)
  (scale doc (zoom-factor) (zoom-factor))
  (draw-debug q doc "aliceblue" "aliceblue" 3))

(define (draw-page-footer q doc)
  (match-define (list x y) (quad-origin q))
  (font-size doc (* .8 default-font-size))
  (font doc default-font-face)
  (fill-color doc default-font-color)
  (text doc (format "~a · ~a at ~a" (quad-ref q :page-number 0)
                    (if (quadwriter-test-mode) "test" (quad-ref q :doc-title "untitled"))
                    (date->string (if (quadwriter-test-mode) (seconds->date 0) (current-date)) #t))
        x y))

(define (make-footer-quad col-q page-idx path)
  (define-values (dir name _) (split-path (path-replace-extension path #"")))
  (q #:size (pt 50 default-line-height)
     #:attrs (hasheq :page-number (+ (quad-ref col-q :page-number-start (add1 (section-pages-used)))
                                     (sub1 page-idx))
                     :doc-title (string-titlecase (path->string name)))
     #:from-parent 'sw
     #:to 'nw
     #:shift (pt 0 (* 1.5 default-line-height))
     #:printable #true
     #:draw-start (λ (q doc)
                    (when draw-debug-line?
                      (draw-debug q doc "goldenrod" "goldenrod"))
                    (draw-page-footer q doc))))

(define q:column (q
                  #:id 'col
                  #:from 'ne
                  #:to 'nw))

(define-quad column-spacer-quad quad)
(define q:column-spacer (q #:type column-spacer-quad
                           #:from 'ne
                           #:to 'nw
                           #:printable only-prints-in-middle))

(define q:page (q
                #:id 'page
                #:from-parent 'nw
                #:draw-start page-draw-start))

(define q:doc (q #:draw-start (λ (q doc) (start-doc doc))
                 #:draw-end (λ (q doc) (end-doc doc))))

(define q:section (q #:id 'section))

(define ((block-draw-start first-line) q doc)
  ;; adjust drawing coordinates for border inset
  (match-define (list bil bit bir bib)
    (for/list ([k (in-list (list :border-inset-left :border-inset-top :border-inset-right :border-inset-bottom))])
      (quad-ref first-line k 0)))
  (match-define (list left top) (pt+ (quad-origin q) (list bil bit)))
  (match-define (list width height) (pt- (size q) (list (+ bil bir) (+ bit bib))))
  ;; fill rect
  (let ([bgc (quad-ref first-line :background-color)])
    (when bgc
      (rect doc left top width height)
      (fill doc bgc)))
  ;; draw border
  (match-define (list bw-left bw-top bw-right bw-bottom)
    (map (λ (k) (max 0  (quad-ref first-line k 0)))
         (list
          :border-width-left
          :border-width-top
          :border-width-right
          :border-width-bottom)))
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
            (quad-ref first-line :border-color-top) bw-top)
  (box-side right (- top (half bw-top)) right (+ bottom (half bw-bottom))
            (quad-ref first-line :border-color-right) bw-right)
  (box-side (+ right (half bw-right)) bottom (- left (half bw-left)) bottom
            (quad-ref first-line :border-color-bottom) bw-bottom)
  (box-side left (+ bottom (half bw-bottom)) left (- top (half bw-top))
            (quad-ref first-line :border-color-left) bw-left)
  (case (quad-ref first-line :block-clip)
    [(#true)
     (when (eq? (log-clipping?) 'warn)
       (for ([line (in-list (quad-elems q))])
         (define line-width (pt-x (size line)))
         (define line-elem-width (sum-x (quad-elems line)))
         (when (< line-width line-elem-width)
           (define error-str (apply string-append (for/list ([q (in-list (quad-elems line))])
                                                    (match (quad-elems q)
                                                      [(list (? string? str)) str]
                                                      [_ ""]))))
           (log-quadwriter-warning (format "clipping overfull line: ~v" error-str)))))
     (save doc)
     (rect doc left top width height)
     (clip doc)]))

(define ((block-draw-end first-line) q doc)
  (case (quad-ref first-line :block-clip)
    [(#true) (restore doc)])
  (when (draw-debug-block?)
    (draw-debug q doc "#6c6" "#9c9")))

(define/match (lines->block lines)
  [((cons line _))
   (q #:from 'sw
      #:to 'nw
      #:elems (from-parent lines 'nw)
      #:id 'block
      #:attrs (quad-attrs line)
      #:size (delay (pt (pt-x (size line)) ; 
                        (+ (sum-y lines)
                           (quad-ref line :inset-top 0)
                           (quad-ref line :inset-bottom 0))))
      #:shift-elems (pt 0 (quad-ref line :inset-top 0))
      #:draw-start (block-draw-start line)
      #:draw-end (block-draw-end line))])

(define/match (from-parent qs [where #f])
  ;; doesn't change any positioning. doesn't depend on state. can happen anytime.
  ;; can be repeated without damage.
  [((? null?) _) null]
  [((cons q rest) where)
   (quad-update! q [from-parent (or where (quad-from q))])
   (cons q rest)])

(define ((col-finish-wrap col-quad) lns . _)
  (match lns
    [(cons line _)
     (list (quad-copy col-quad
                      ;; move block attrs up, so they are visible in page wrap
                      [attrs (copy-block-attrs (quad-attrs line)
                                               (hash-copy (quad-attrs col-quad)))]
                      [elems (from-parent (insert-blocks lns) 'nw)]))]
    [_ null]))

(define (column-wrap qs vertical-height column-gap [column-quad q:column])
  (unless (positive? vertical-height)
    (raise-argument-error 'column-wrap "positive number" vertical-height))
  
  ;; on timing of `insert-blocks`:
  ;; can't do it before because it depends on where columns are broken.
  ;; could do it after, but it would require going back inside each col quad
  ;; which seems overly interdependent, because `insert-blocks` is used to determine break locations.
  ;; `col-wrap` should emit quads that are complete.
  (define col-spacer (quad-copy q:column-spacer [size (pt column-gap 100)]))
  (add-between
   (wrap qs vertical-height
         #:soft-break #true
         #:hard-break column-break-quad?
         #:no-break (λ (q) (quad-ref q :no-colbr)) ; cooperates with make-nobreak
         #:distance (λ (q dist-so-far wrap-qs)
                      ;; do trial block insertions
                      (sum-y (insert-blocks (reverse wrap-qs))))                     
         #:finish-wrap (col-finish-wrap column-quad))
   col-spacer))

(define ((page-finish-wrap make-page-quad path) cols q0 q page-idx)
  (define page-quad (make-page-quad (+ (section-pages-used) page-idx)))
  (define elems
    (match (quad-ref (car cols) :footer-display #true)
      [(or #false "none") (from-parent cols 'nw)]
      [_ (cons (make-footer-quad (car cols) page-idx path) (from-parent cols 'nw))]))
  (list (quad-copy page-quad
                   [elems elems]
                   [attrs (copy-block-attrs (quad-attrs (car cols))
                                            (hash-copy (quad-attrs page-quad)))])))

(define (page-wrap qs width [make-page-quad (λ _ q:page)])
  (unless (positive? width)
    (raise-argument-error 'page-wrap "positive number" width))
  (wrap qs width
        #:soft-break #true
        #:hard-break page-break-quad?
        #:no-break (λ (q) (quad-ref q :no-pbr))
        #:distance (λ (q dist-so-far wrap-qs) (sum-x wrap-qs))
        #:finish-wrap (page-finish-wrap make-page-quad (pdf-output-path (current-pdf)))))

(define (insert-blocks lines)
  (define groups-of-lines (contiguous-group-by (λ (x) (quad-ref x :display)) lines))
  (append* (for/list ([line-group (in-list groups-of-lines)])
             (if (quad-ref (car line-group) :display)
                 (list (lines->block line-group))
                 line-group))))

(define-quad first-line-indent-quad quad)

(define (insert-first-line-indents qs-in)
  ;; first line indents are quads inserted at the beginning of a paragraph
  ;; (that is, just after a paragraph break)
  ;; they need to be installed before line wrap
  ;; to be compatible with first-fit and best-fit.

  ;; stick a pbr on the front if there isn't one already
  ;; because of the "lookahead" style of iteration
  (define qs (match qs-in
               [(cons (? para-break-quad?) _) qs-in]
               [_ (cons q:page-break qs-in)]))
  (apply append
         (for/list ([q (in-list qs)]
                    [next-q (in-list (cdr qs))])
           (match (and (para-break-quad? q)  (quad-ref next-q :first-line-indent 0))
             [(or #false 0) (list next-q)]
             [indent-val (list (make-quad #:from 'bo
                                          #:to 'bi
                                          #:draw-end q:string-draw-end
                                          #:type first-line-indent-quad
                                          #:attrs (quad-attrs next-q)
                                          #:size (pt indent-val 10)) next-q)]))))
