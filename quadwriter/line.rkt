#lang debug racket
(require quad/quad
         "struct.rkt"
         "param.rkt"
         "debug.rkt"
         "font.rkt"
         "string.rkt"
         "attrs.rkt"
         quad/base
         sugar/list
         pitfall
         racket/unsafe/ops)
(provide (all-defined-out))

(define (hr-draw dq doc)
  (match-define (list left top) (quad-origin dq))
  (match-define (list right bottom) (size dq))
  (save doc)
  (translate doc left (+ top (/ bottom 2.0)))
  (move-to doc 0 0)
  (line-to doc right 0)
  (line-width doc 0.5)
  (stroke doc "black")
  (restore doc))

(define (make-hr-quad line-q)
  (quad-copy line-quad line-q [draw-start hr-draw]))

(define q:line (make-quad
                #:type line-quad
                #:size (pt 0 default-line-height)
                #:from 'sw
                #:to 'nw
                #:printable #true
                #:tag 'line
                #:draw-start (if draw-debug-line? draw-debug void)))

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
                                 [size (make-size-promise-for-string last-q str+hyphen)])))]
    [_ qs]))


(define (space-quad? q) (equal? (quad-elems q) (list " ")))

(define (hang-punctuation nonspacess)
  (match nonspacess
    [(list sublists ... (list prev-qs ... last-q))
     #:when (pair? (quad-elems last-q))
     (match (regexp-match #rx"[.,:;’-]$" (car (quad-elems last-q)))
       [#false nonspacess]
       [last-char-str
        (define hanger-q (quad-copy string-quad last-q
                                    [elems null]
                                    [size (let ([p (make-size-promise-for-string last-q (car last-char-str))])
                                            (delay
                                              (match-define (list x y) (force p))
                                              (pt (- x) y)))]))
        (define last-sublist (append prev-qs (list last-q hanger-q)))
        (append sublists (list last-sublist))])]
    [_ nonspacess]))


(define (sum-sum-x qss)
  (for/sum ([qs (in-list qss)])
           (sum-x qs)))

(define (tracking-adjustment q)
  (match q
    [(? string-quad?) (/ (quad-ref q :font-tracking 0) 2.0)]
    [_ 0]))

(define (fill-line-wrap all-qs line-prototype last-line-in-paragraph?)
  ;; happens during the finish of a line wrap, before consolidation of runs
  (unless (pair? all-qs)
    (raise-argument-error 'fill-line-wrap "nonempty list of quads" all-qs))

  ;; remove anchored quads because they don't affect line layout
  (define-values (absolute-qs qs) (partition (λ (q) (quad-ref q :parent)) all-qs))

  (match qs
    [(? null?) absolute-qs]
    [(and (cons q-first other-qs) (list _ ... q-last))
     (define align-value (quad-ref q-first :line-align "left"))
     ;; words may still be in hyphenated fragments
     ;; (though soft hyphens would have been removed)
     ;; so group them (but no need to consolidate — that happens elsewhere)
     (define-values (spacess nonspacess) (partition* space-quad? qs))
     (match (length nonspacess)
       [1 #:when (equal? align-value "justify") qs] ; can't justify single word
       [nonspacess-count
        (match-define (list line-prototype-width line-prototype-height) (quad-size line-prototype))
        (define hung-nonspacess (hang-punctuation nonspacess))
        (define left-tracking-adjustment (tracking-adjustment q-first))
        (define right-tracking-adjustment (tracking-adjustment q-last))
        (define nonspace-total-width
          (- (sum-sum-x hung-nonspacess) left-tracking-adjustment right-tracking-adjustment))
        (define space-total-width (sum-sum-x spacess))
        (define empty-hspace (- line-prototype-width
                                (quad-ref q-first :inset-left 0)
                                nonspace-total-width
                                (quad-ref q-first :inset-right 0)))

        (define (make-left-edge-filler [width 0])
          (make-quad #:type filler-quad
                     #:tag 'line-filler
                     #:from-parent (quad-from-parent q-first)
                     #:from 'bo
                     #:to 'bi
                     #:shift (pt (- left-tracking-adjustment) 0)
                     #:size (pt width 0)
                     #:attrs (quad-attrs q-first)))
     
        (cond
          [(or
            (and (equal? align-value "justify") (or (not last-line-in-paragraph?)
                                                    ;; don't justify the last line in a paragraph
                                                    ;; unless empty space is less than 17% of width (an arbitrary visual threshold)
                                                    (< (/ empty-hspace line-prototype-width 1.0) .17)))
            (let ([line-overfull? (negative? (- empty-hspace space-total-width))])
              ;; force justification upon overfull lines,
              ;; which amounts to shrinking the word spaces till the line fits
              (and line-overfull? (> nonspacess-count 1))))
           (define justified-space-width (/ empty-hspace (sub1 nonspacess-count)))
           (cons (make-left-edge-filler)
                 (apply append (add-between hung-nonspacess (list (make-quad
                                                                   #:from 'bo
                                                                   #:to 'bi
                                                                   #:draw-end q:string-draw-end
                                                                   #:size (pt justified-space-width line-prototype-height))))))]
          [else
           (define space-multiplier (match align-value
                                      ["center" 0.5]
                                      ;; fill inner & outer as if they were right,
                                      ;; they will be corrected later, when pagination is known.
                                      [(or "right" "inner" "outer") 1]
                                      ;; "left" and "justify" are handled here
                                      [_ 0]))
           ;; subtact space-width because that appears between words
           ;; we only care about redistributing the space on the ends
           (define end-hspace (- empty-hspace space-total-width))
           ;; make filler a leading quad, not a parent / grouping quad,
           ;; so that elements can still be reached by consolidate-runs
           (list* (make-left-edge-filler (* end-hspace space-multiplier))
                  (quad-update! q-first [from-parent #f])
                  ;; ok to put back absolute quads at end, because it doesn't affect their layout
                  (append other-qs absolute-qs))])])]))

  (define (make-paragraph-spacer maybe-first-line-q key default-val)
    (define arbitrary-width 20)
    (make-quad #:type line-spacer-quad
               #:size (pt arbitrary-width (cond
                                            [(and maybe-first-line-q (quad-ref maybe-first-line-q key))]
                                            [else default-val]))
               #:from 'sw
               #:to 'nw
               #:printable only-prints-in-middle
               #:draw-start (if (draw-debug-line?) draw-debug void)))


  (define ((line-wrap-finish line-prototype-q default-block-id) wrap-qs q-before q-after idx)
    ;; we curry line-q so that the wrap size can be communicated to this operation
    ;; remove unused soft hyphens so they don't affect final shaping
    (define wrap-qs-printing (for/list ([wq (in-list wrap-qs)]
                                        #:unless (equal? (quad-elems wq) '("\u00AD")))
                                       wq))
    (define new-lines
      (cond
        [(empty? wrap-qs-printing) null]
        [(hr-break-quad? q-after) (list (make-hr-quad line-prototype-q))]
        [else
         ;; render hyphen first so that all printable characters are available for size-dependent ops.
         (define pcs-with-hyphen (render-hyphen wrap-qs-printing q-after))
         ;; fill wrap so that consolidate-runs works properly
         ;; (justified lines won't be totally consolidated)
         (define last-line-in-paragraph? (not q-after))
         (define pcs (fill-line-wrap pcs-with-hyphen line-prototype-q last-line-in-paragraph?))
         (match (consolidate-runs pcs)
           [(and (cons elem-first _) elems)
            (match-define (list line-width line-height) (quad-size line-prototype-q))
            (list
             (quad-copy line-quad line-prototype-q
                        ;; move block attrs up, so they are visible in col wrap
                        [attrs (let ([h (copy-block-attrs (quad-attrs elem-first) (hash-copy (quad-attrs line-prototype-q)))])
                                 ;; we want every group of lines in a paragraph to have a block id
                                 ;; so that it will be wrapped as a block later.
                                 ;; we only set this if there is no value for :display.
                                 (hash-ref! h :display default-block-id)
                                 h)]
                        ;; line width is static
                        ;; line height is the max 'line-height value or the natural height of q:line
                        [size (pt line-width (match (filter-map (λ (q) (or (quad-ref q :line-height) (pt-y (size q)))) pcs)
                                               [(? null?) line-height]
                                               [line-heights (apply max line-heights)]))]
                        ;; handle list indexes. drop new quad into line to hold list index
                        ;; could also use this for line numbers
                        [elems
                         ;; we assume here that a list item has already had extra inset-left
                         ;; with room for a bullet
                         ;; which we just insert at the front.
                         ;; this is safe because line has already been filled.
                         (append
                          ;; only put bullet into line if we're at the first line of the list item
                          (match (and (eq? idx 1) (quad-ref elem-first :list-index))
                            [#false null]
                            [bullet
                             (define bq (quad-copy string-quad q:string ;; copy q:string to get draw routine
                                                   ;; borrow attrs from elem
                                                   [attrs (quad-attrs elem-first)]
                                                   ;; use bullet as elems
                                                   [elems (list (if (number? bullet) (format "~a." bullet) bullet))]
                                                   ;; size doesn't matter because nothing refers to this quad
                                                   ;; just for debugging box
                                                   [size (pt 15 (pt-y (size line-prototype-q)))]))
                             (from-parent (list bq) 'sw)])
                          (from-parent
                           (match (quad-ref elem-first :inset-left 0)
                             [0 elems]
                             [inset-val (cons (make-quad
                                               #:draw-end q:string-draw-end
                                               #:to 'sw
                                               #:size (pt inset-val 5)
                                               #:type offsetter-quad)
                                              elems)]) 'sw))]))]
           [_ null])]))
    (define maybe-first-line (and (pair? new-lines) (car new-lines)))
    (append (match q-before
              [#false (list (make-paragraph-spacer maybe-first-line :space-before 0))] ; paragraph break
              [_ null])
            new-lines
            (match q-after
              [(? column-break-quad? column-break) (list column-break)] ; hard column (or section or page) break
              [#false (list (make-paragraph-spacer maybe-first-line :space-after (* default-line-height 0.6)))] ; paragraph break
              [_ null]))) ; hard line break
                       

  (define softies (map string '(#\space #\- #\u00AD)))

  (define (soft-break-for-line? q)
    (and (pair? (quad-elems q))
         (member (unsafe-car (quad-elems q)) softies)))


  (define (line-wrap qs wrap-size [debug #false])
    (unless (positive? wrap-size)
      (raise-argument-error 'line-wrap "positive number" wrap-size))
    (match qs
      [(cons q _)
       (define line-q (quad-copy line-quad q:line [size (pt wrap-size (quad-ref q :line-height default-line-height))]))
       (define permitted-justify-overfill
         (match (quad-ref q :line-align)
           ;; allow justified lines to go wider,
           ;; and then fill-wrap will tighten thes word spaces
           ;; this makes justified paragraphs more even, becuase
           ;; some lines are a little tight, as opposed to all of them being loose
           ["justify" 1.04]
           [_ 1]))
       ;; group lines into sublists separated by para-breaks, but then omit the para-breaks themselves
       ;; because they've served their purpose (leave the others, to be expressed later)
       ;; however, leave line-breaks in, because they will be handled by wrap.
       (define para-qss (let loop ([qs qs][acc null])
                          (match qs
                            [(? null?) (reverse acc)]
                            [(cons (? para-break-quad?) rest)
                             (loop rest acc)]
                            [(cons (? column-break-quad? bq) rest)
                             (loop rest (cons bq acc))]
                            [(list* (and (not (? para-break-quad?)) nbqs) ... rest)
                             (loop rest (cons nbqs acc))])))
       (define res
         (apply append
                (for/list ([para-qs (in-list para-qss)])
                          (define block-id (gensym))
                          (match para-qs
                            [(? break-quad? bq) (list bq)]
                            [(cons pq _)
                             (wrap para-qs
                                   (* (- wrap-size
                                         (quad-ref pq :inset-left 0)
                                         (quad-ref pq :inset-right 0))
                                      permitted-justify-overfill)
                                   debug
                                 
                                   ;; during wrap, anchored qs are treated as having distance 0
                                   ;; so they can staty in right place, so that relative queries will work.
                                   ;; but they won't affect where lines break
                                   #:distance (λ (q last-dist wrap-qs)
                                                (+ last-dist (cond
                                                               [(quad-ref q :parent) 0]
                                                               [(printable? q) (distance q)]
                                                               [else 0])))
                                   #:nicely (match (or (current-line-wrap) (quad-ref pq :line-wrap))
                                              [(or "best" "kp") #true]
                                              [_ #false])
                                   #:hard-break line-break-quad?
                                   #:soft-break soft-break-for-line?
                                   #:finish-wrap (line-wrap-finish line-q block-id))]))))
       res]
      [_ null]))

  