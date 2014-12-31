#lang racket/base
(require sugar/coerce sugar/define sugar/list sugar/debug racket/list racket/format racket/function racket/string (for-syntax racket/base racket/syntax) math/flonum racket/vector sugar/cache)
(require "ocm.rkt" "quads.rkt" "utils.rkt" "measure.rkt" "world.rkt" "logger.rkt" )

;; predicate for the soft hyphen
(define+provide/contract (soft-hyphen? x)
  (string? . -> . boolean?)
  (equal? (~a world:soft-hyphen) x))

;; visible characters that also mark possible breakpoints
(define+provide/contract (visible-breakable? x)
  (string? . -> . coerce/boolean?)
  (member x world:hyphens-and-dashes))

;; invisible characters that denote possible breakpoints
(define+provide/contract (invisible-breakable? x)
  (string? . -> . coerce/boolean?)
  (member x (cons world:empty-string world:spaces)))

;; union of visible & invisible
(define+provide/contract (breakable? x)
  (any/c . -> . boolean?)
  (cond
    [(string? x) (or (visible-breakable? x) (invisible-breakable? x))]
    [(word? x) (breakable? (word-string x))]
    [else #f]))

;; used by insert-spacers to determine which characters 
;; can be surrounded by stretchy spacers 
(define+provide/contract (takes-justification-space? x)
  (any/c . -> . boolean?)
  (whitespace/nbsp? x))

;; test if a quad can be a word break:
;; either it's an explicit word break,
;; or it's breakable (and can be converted to a word break)
(define+provide/contract (possible-word-break-quad? q)
  (quad? . -> . boolean?)
  (or (word-break? q) (breakable? q)))

;; convert a possible word break into an actual one
(define+provide/contract (convert-to-word-break q)
  (possible-word-break-quad? . -> . word-break?)
  (cond 
    [(word-break? q) q]
    [(word? q)
     (define str (word-string q)) ; str will be one character long, because we've exploded our input
     (apply word-break
            (merge-attrs q ; take q's attributes for formatting purposes
                         (cond
                           ;; a space is ordinarily visible, but disappears at the end of a line
                           [(equal? str " ") (list world:no-break-key " " world:before-break-key "")]
                           ;; soft hyphen is ordinarily invisible, but appears at the end of a line
                           [(soft-hyphen? str) (list world:no-break-key "" world:before-break-key "-")]
                           ;; a visible breakable character is always visible
                           [(visible-breakable? str) (list world:no-break-key str world:before-break-key str)]
                           [else (world:default-word-break-list)])) (quad-list q))]))

(define (make-unbreakable q)
  (quad-attr-set q world:unbreakable-key #t))

;; take list of atomic quads and gather them into pieces
;; a piece is an indivisible chunk of a line.
;; meaning, a line can wrap at a piece boundary, but not elsewhere.
;; hyphenation produces more, smaller pieces, which means more linebreak opportunities
;; but this also makes wrapping slower.
(define+provide/contract (make-pieces qs)
  (quads? . -> . pieces?)
  (define-values (breakable-items items-to-make-unbreakable) (split-at-right qs (min world:minimum-last-line-chars (length qs))))
  (define unbreak-qs (append breakable-items (map make-unbreakable items-to-make-unbreakable)))
  (define lists-of-quads (slicef-at unbreak-qs (λ(q) (or (not (possible-word-break-quad? q)) 
                                                         (quad-attr-ref q world:unbreakable-key #f)))))
  (define-values (first-lists-of-quads last-list-of-quads) (split-last lists-of-quads))
  (define (make-first-pieces qs)
    (let-values ([(first-qs last-q) (split-last qs)])
      (apply piece (list world:word-break-key (convert-to-word-break last-q)) first-qs)))
  (append (map make-first-pieces first-lists-of-quads) 
          (list (apply piece #f last-list-of-quads))))

;; extract font attributes from quad, or get default values
(provide font-attributes-with-defaults)
(define-syntax-rule (font-attributes-with-defaults q)
  (list
   (let ([size (quad-attr-ref/parameter q world:font-size-key)])
     (if (exact-integer? size) (fl size) size))
   (quad-attr-ref/parameter q world:font-name-key)
   (quad-attr-ref/parameter q world:font-weight-key)
   (quad-attr-ref/parameter q world:font-style-key)))

;; get the width of a quad.
;; Try the attr first, and if it's not available, compute the width.
;; comes in fast or slow versions.
;; not designed to update the source quad.
(define+provide/contract (quad-width q [fast? #f])
  ((quad?) (boolean?) . ->* . flonum?)
  (cond
    [(quad-has-attr? q world:width-key) (fl (quad-attr-ref q world:width-key))]
    [(ormap (λ(pred) (pred q)) (list char? run? word? word-break?)) 
     (apply measure-text (word-string q) 
            (font-attributes-with-defaults q))]
    [(line? q) (fold-fl+ (map quad-width (quad-list q)))]
    [else 0.0]))

;; shorthand for fast version of quad-width.
(define+provide (quad-width-fast q) 
  (quad-width q #t))

;; get the ascent (distance from top of text to baseline)
;; used by renderer to align text runs baseline-to-baseline.
;; consult the attrs, and if not available, compute it.
;; not designed to update the source quad.
(define+provide/contract (ascent q)
  (quad? . -> . flonum?)
  (or (quad-attr-ref q world:ascent-key #f)
      (cond 
        [(ormap (λ(pred) (pred q)) (list char? run? word? word-break?)) 
         (apply measure-ascent (word-string q) (font-attributes-with-defaults q))]
        [else 0.0])))

;; convert a piece into its final form, which depends on location.
;; if a piece appears at the end of a line, it is rendered in "before break" mode.
;; if a piece appears elsewhere in a line, it is rendered in "no break" mode.
;; this allows the appearance of a piece to change depending on whether it's at the end.
;; and thus give correct behavior to trailing word spaces, soft hyphens, etc.
(define+provide/contract (render-piece p [before-break? #f])
  ((piece?) (boolean?) . ->* . piece?)
  ;; a piece doesn't necessarily have a word-break item in it.
  ;; only needs it if the appearance of the piece changes based on location.
  ;; so words are likely to have a word-break item; boxes not.
  ;; the word break item contains the different characters needed to finish the piece.
  (define the-word-break (quad-attr-ref p world:word-break-key #f))
  (let ([p (quad-attr-remove p world:word-break-key)]) ; so it doesn't propagate into subquads
    (if the-word-break
        (quad (quad-name p) (quad-attrs p) 
              (append (quad-list p) (let ([rendered-wb ((if before-break? 
                                                          word-break->before-break 
                                                          word-break->no-break) the-word-break)])
                                      (if (> (string-length (word-string rendered-wb)) 0) ; if rendered-wb is "", don't append it
                                          (list rendered-wb)
                                          empty))))
        p)))

;; shorthand 
(define+provide (render-piece-before-break p) 
  (render-piece p #t))

;; helper macro to convert quad into word-break.
;; look up the break character and convert the quad based on what is found.
(define-syntax-rule (render-word-break wb key)
  (let ([break-char (quad-attr-ref wb key)])
    (quad (if (whitespace? break-char) 'word-break 'word)
          (hash-remove (hash-remove (quad-attrs wb) world:no-break-key) world:before-break-key) (list (quad-attr-ref wb key)))))

;; uses macro above in no-break mode.
(define (word-break->no-break wb)
  (render-word-break wb world:no-break-key))

;; uses macro above in before-break mode.
(define (word-break->before-break wb)
  (render-word-break wb world:before-break-key))

;; is this the last line? compare current line-idx to total lines 
(define+provide/contract (last-line? line)
  (line? . -> . boolean?)
  (define line-idx (quad-attr-ref line world:line-index-key #f))
  (define lines (quad-attr-ref line world:total-lines-key #f))
  (and line-idx lines (= (add1 line-idx) lines)))


;; optical kerns are automatically inserted at the beginning and end of a line
;; (by the pieces->line function)
;; but may also be found elsewhere, imperatively (e.g., before an indent)
;; they allow certain characters to hang over the line margin.
;; optical kerns aren't considered when the line is being composed,
;; rather they are an adjustment added to a composed line.
;; the optical kern doesn't have left- or right-handed versions.
;; it just looks at quads on both sides and kerns them if appropriate.
;; in practice, only one will likely be used.
(define+provide/contract (render-optical-kerns exploded-line-quads)
  (quads? . -> . quads?)
  (define (overhang-width q)
    (if (and (word? q) (member (word-string q) world:hanging-chars))
        (fl*s -1.0 (world:optical-overhang) (apply measure-text (word-string q) (font-attributes-with-defaults q)))
        0.0))
  (cond
    [(not (empty? exploded-line-quads))
     ;; after exploding, each quad will have a string with one character.
     (for/list ([(q-left q q-right) (apply in-parallel (shift exploded-line-quads '(1 0 -1)))])
       (if (optical-kern? q)
           (quad-attr-set q world:width-key (fl+ (overhang-width q-left) (overhang-width q-right)))
           q))]
    [else exploded-line-quads]))


;; ultimately every line is filled to fit the whole measure.
;; spacers are used to soak up extra space left over in a line.
;; depending on where the spacers are inserted, different formatting effects are achieved.
;; e.g., left / right / centered / justified.
(define+provide/contract (insert-spacers-in-line line [alignment-override #f])
  ((line?) ((or/c #f symbol?)) . ->* . line?)
  ;; important principle: avoid peeking into quad-list to get attributes.
  ;; because non-attributed quads may be added.
  ;; here, we know that common attributes are hoisted into the line.
  ;; so rely on line attributes to get horiz alignment.
  (define key-to-use (if (and (last-line? line) (quad-has-attr? line world:horiz-alignment-last-line-key))
                         world:horiz-alignment-last-line-key 
                         world:horiz-alignment-key))
  (define horiz-alignment (or alignment-override (quad-attr-ref line key-to-use (world:horiz-alignment-default))))
  (define default-spacer (spacer))
  (define-values (before middle after) (case horiz-alignment
                                         [(left) (values #f #f default-spacer)]
                                         [(right) (values default-spacer #f #f)]
                                         [(center) (values default-spacer #f default-spacer)]
                                         [(justified justify) (values #f default-spacer #f)]
                                         [else (values #f #f #f)]))
  (define (copy-with-attrs q attr-source) 
    (define keys-to-ignore '(width)) ; width will be determined during fill routine
    (define filtered-hash (and (quad-attrs attr-source)
                               (foldl (λ(k ht) (hash-remove ht k)) (quad-attrs attr-source) keys-to-ignore)))
    (quad (quad-name q) (merge-attrs filtered-hash q) (quad-list q)))
  (define result
    (quad (quad-name line) (quad-attrs line) (flatten (let ([qs (quad-list line)])
                                                        `(,@(when/splice before (copy-with-attrs before (first qs))) 
                                                          
                                                          ,@(map (λ(q) (if (and middle (takes-justification-space? q)) 
                                                                           (let ([interleaver (copy-with-attrs middle q)])
                                                                             (list interleaver q interleaver)) 
                                                                           q)) qs) 
                                                          ,@(when/splice after (copy-with-attrs after (last qs))))))))
  result)


;; installs the width in the quad.
;; this becomes the value reported by quad-width.
(define (embed-width q w)
  (quad-attr-set q world:width-key w))

;; installs the ascent in the quad.
(define (record-ascent q)
  (quad-attr-set q world:ascent-key (ascent q)))

;; helper function: doesn't need contract because it's already covered by the callers
(define (render-pieces ps)
  (define-values (initial-ps last-p) (split-last ps))
  (snoc (map render-piece initial-ps) (render-piece-before-break last-p)))

;; compose pieces into a finished line.
;; take the contents of the rendered pieces and merge them.
;; compute looseness for line as a whole.
;; also add ascent to each component quad, which can be different depending on font & size.
(define+provide (pieces->line ps measure-quad-proc)
  (pieces? procedure? . -> . line?)
  
  ;; handle optical kerns here to avoid resplitting and rejoining later.
  (define rendered-pieces (render-pieces ps))
  (define split-pieces (map quad-list rendered-pieces))
  (define line-quads (append* split-pieces)) 
  (define line-quads-maybe-with-opticals 
    (if world:use-optical-kerns?
        (render-optical-kerns
         (let ([my-ok (list (optical-kern (quad-attrs (car line-quads))))]) ; take attrs from line, incl measure
           (append my-ok line-quads my-ok)))
        line-quads))
  (define merged-quads (join-quads line-quads-maybe-with-opticals))
  (define merged-quad-widths (map measure-quad-proc merged-quads)) ; 10% of function time
  
  (log-quad-debug "making pieces into line = ~v" (string-append* (map quad->string merged-quads)))
  
  ;; if measure key isn't present, allow an error, because that's weird
  (when (not (quad-has-attr? (first line-quads) world:measure-key))
    (error 'pieces->line "quad has no measure key: ~a" (first line-quads)))
  
  (define measure (fl (quad-attr-ref (first merged-quads) world:measure-key)))
  (define looseness (round-float (fl/ (fl- measure (fold-fl+ merged-quad-widths)) measure)))
  
  ;; quads->line function hoists common attributes into the line
  (let* ([new-line-quads (map embed-width merged-quads merged-quad-widths)] ; 15% of time
         [new-line-quads (map record-ascent new-line-quads)] ; 35% of time
         [new-line (apply line (quad-attrs (car new-line-quads)) new-line-quads)] 
         [new-line (quad-attr-set new-line world:line-looseness-key looseness)])
    new-line))

;; a faster line-measuring function used by the wrapping function to test lines.
(define+provide (measure-potential-line ps)
  ;(pieces? . -> . flonum?)  
  (for*/sum ([rendered-piece (in-list (render-pieces ps))]
             [piece-quad (in-list (quad-list rendered-piece))])
    (quad-width-fast piece-quad)))




(define (vector-break-at vec bps)
  (define-values (vecs _) ;; loop backward
    (for/fold ([vecs empty][end (vector-length vec)])([start (in-list (reverse (cons 0 bps)))])
      (if (= start end)
          (values vecs start)
          (values (cons (vector-copy vec start end) vecs) start))))
  vecs)

(define-syntax-rule (report-time0 name expr)
  (let ([op (open-output-string)])
    (parameterize ([current-output-port op])
      (define result (time expr))
      (report (string-trim (get-output-string op)) name)
      (values result))))

(define-syntax-rule (report-time name expr)
  expr)

;; makes a wrap function by combining component functions.
(define+provide (make-wrap-proc 
                 #:make-pieces-proc make-pieces-proc 
                 #:measure-quad-proc measure-quad-proc
                 #:compose-line-proc compose-line-proc
                 #:find-breakpoints-proc find-breakpoints-proc)
  (λ(qs [measure #f])
    (let* ([measure (fl+ (fl (or measure (quad-attr-ref/parameter (car qs) world:measure-key))) 0.0)]
           [qs (if (quad-has-attr? (car qs) world:measure-key)
                   qs
                   (map (curryr quad-attr-set world:measure-key measure) qs))])
      (log-quad-debug "wrapping on measure = ~a" measure)
      (define pieces (make-pieces-proc qs)) ; 5%
      (define bps (report-time 'find-bps (find-breakpoints-proc (list->vector pieces) measure))) ; 50%
      (define broken-pieces (break-at pieces bps)) ; 5%
      ; (report (add1 (length bps)) 'lines-in-paragraph)
      (report-time 'compose-lines (map (λ(bp) (compose-line-proc bp measure-quad-proc)) broken-pieces))))) ; 50%

(define width? flonum?)
(define measure? flonum?)
(define (breakpoints? x) (and (list? x) (andmap integer? x)))


(define (install-measurement-keys p)
  (define basic-width (round-float (apply + (map quad-width-fast (quad-list p)))))
  (define p-word-break (quad-attr-ref p world:word-break-key #f))
  (define before-break-width (fl+ basic-width (if p-word-break
                                                  (quad-width-fast (word (quad-attrs p-word-break) (quad-attr-ref p-word-break world:before-break-key)))
                                                  0.0)))
  (define no-break-width (fl+ basic-width (if p-word-break
                                              (quad-width-fast (word (quad-attrs p-word-break) (quad-attr-ref p-word-break world:no-break-key)))
                                              0.0)))
  (quad-attr-set* p 'bb-width before-break-width 'nb-width no-break-width))


(define (make-piece-vectors pieces)
  (define pieces-measured 
    (report-time 'make-wrap-vector (for/list ([p (in-vector pieces)])
                                     (define wb (quad-attr-ref p world:word-break-key #f))
                                     (vector
                                      (fold-fl+ (for/list ([q (in-list (quad-list p))])
                                                  (define str (quad->string q))
                                                  (if (equal? str "")
                                                      (fl (quad-attr-ref q world:width-key 0.0))
                                                      (apply measure-text (quad->string q) (font-attributes-with-defaults q)))))
                                      (if wb (apply measure-text (quad-attr-ref wb world:no-break-key) (font-attributes-with-defaults wb)) 0.0)
                                      (if wb (apply measure-text (quad-attr-ref wb world:before-break-key) (font-attributes-with-defaults wb)) 0.0)))))
  (values
   (for/flvector ([p (in-list pieces-measured)]) (fl+ (vector-ref p 0) (vector-ref p 1))) ; first = word length, second = nb length
   (for/flvector ([p (in-list pieces-measured)]) (fl+ (vector-ref p 0) (vector-ref p 2))))) ; first = word length, third = bb length


(define (trial-line pieces-rendered-widths pieces-rendered-before-break-widths i j)
  (define flvec (flvector-copy pieces-rendered-widths i j))
  (flvector-set! flvec (sub1 (flvector-length flvec)) (flvector-ref pieces-rendered-before-break-widths (sub1 j)))
  flvec)


(define (get-line-width line) (round-float (fold-fl+ (flvector->list line))))


;; optimal linefitting: minimize the penalty function across all linebreaks in the paragraph
;; slower but nicer. TeX algorithm + SMAWK speed.
(define+provide (best-fit-proc pieces measure)
  ;((pieces? . -> . width?) . -> . (pieces? measure? . -> . breakpoints?))
  
  ;; don't use struct for penalty, because of read/write overhead
  (define $penalty vector)
  (define ($penalty-width x) (vector-ref x 1))
  (define ($penalty-hyphens x) (vector-ref x 0))
  ;; Reduce the vector to an integer by treating it as magnitude from origin.
  ;(define ($penalty->integer v) (sqrt (apply + (map (compose1 (curryr expt 2)) (list ($penalty-width v))))))
  (define ($penalty->value v) ($penalty-width v))
  (define initial-value ($penalty 0 0.0))
  
  ;(define initial-value 0)
  (define matrix-value->number identity)
  
  (define checked-ijs (make-hash))
  ;; this is the winning performance strategy: extract the numbers first, then just wrap on those.
  ;; todo: how to avoid re-measuring pieces later?
  ;; todo: how to retain information about words per line and hyphen at end?
  (define-values (pieces-rendered-widths pieces-rendered-before-break-widths)
    (make-piece-vectors pieces))
  (define pieces-with-word-space (vector-map (λ(piece) (and (quad-has-attr? piece world:word-break-key) (equal? (quad-attr-ref (quad-attr-ref piece world:word-break-key) 'nb) " "))) pieces))
  
  (log-quad-debug "~a pieces to wrap = ~v" (vector-length pieces) (vector-map quad->string pieces))
  (define (penalty i j)
    (hash-set! checked-ijs (cons i j) #t)
    (define out-of-bounds-signal ($penalty 0 (fl* -1.0 (fl i)))) ; for ocm
    (define last-line? (= j (vector-length pieces)))
    (cond
      [(or (>= i j) ; implies negative or zero length line 
           (> j (vector-length pieces))) ; exceeds available pieces
       out-of-bounds-signal] 
      [else
       (define penalty-up-to-i (ocm-min-value ocm i))
       (define words (fl (vector-count identity (vector-copy pieces-with-word-space i (sub1 j)))))     
       (define last-piece-to-test (vector-ref pieces (sub1 j)))
       (define new-hyphen?
         (and (quad-has-attr? last-piece-to-test world:word-break-key)
              (equal? (quad-attr-ref (quad-attr-ref last-piece-to-test world:word-break-key) world:before-break-key) "-")))
       (define cumulative-hyphens (if (not new-hyphen?) 
                                      0 
                                      (add1 ($penalty-hyphens penalty-up-to-i))))
       
       
       (define line-width (get-line-width (trial-line pieces-rendered-widths pieces-rendered-before-break-widths i j)))
       
       ($penalty 
        cumulative-hyphens
        (round-float
         (fl+s 
          (if (> cumulative-hyphens world:hyphen-limit)
              (fl world:hyphen-penalty)
              0.0)
          (fl world:new-line-penalty) 
          ($penalty->value penalty-up-to-i)
          (cond
            ;; overfull line: huge penalty prevents break; multiplier is essential for monotonicity.
            ;; multiply by -1 because line-width is longer than measure, thus diff is negative
            [(fl> line-width (fl* world:allowed-overfull-ratio measure))
             (fl* (fl- line-width measure) (flexpt 10.0 7.0))] 
            ;; standard penalty, optionally also applied to last line (by changing operator)
            [((if world:last-line-can-be-short < <=) j (vector-length pieces)) (fl/ (flexpt (fl- measure line-width) 2.0) (flmax 1.0 words))]
            ;; only option left is (= j (length pieces)), meaning we're on the last line.
            ;; 0 penalty means any length is ok.
            ;[(< (length pieces-to-test) (world:minimum-last-line-pieces)) 50000]
            [else 0.0]))))]))
  
  (define ocm (make-ocm penalty initial-value $penalty->value))
  
  ;; starting from last position, ask ocm for position of row minimum (= new-pos)
  ;; collect this value, and use it as the input next time
  ;; until you reach first position.    
  (define first-position 0)
  (define last-position (vector-length pieces))
  (define result (let loop ([pos last-position][acc null])
                   (let ([next-pos (ocm-min-index ocm pos)]) ; first look ahead ...
                     (if (= next-pos first-position) ; therefore we're done
                         acc
                         (loop next-pos (cons next-pos acc))))))
  (log-quad-debug "penalty pieces vs. pairs checked = ~a ~a" (vector-length pieces) (exact->inexact (/ (length (hash-keys checked-ijs)) (vector-length pieces))))
  (log-quad-debug "best-fit breakpoints = ~a" result)
  result)



;; greedy linefitting: find the biggest line that will fit, then set the next
;; faster but coarser. Web browsers & most word processors use this approach.
(define+provide (first-fit-proc pieces measure)
  ;((pieces? . -> . width?) . -> . (pieces? measure? . -> . breakpoints?))
  (define-values (pieces-rendered-widths pieces-rendered-before-break-widths)
    (make-piece-vectors pieces))
  
  
  (define bps 
    (for/fold ([bps '(0)])([j-1 (in-range (vector-length pieces))])
      (if (fl> (get-line-width (trial-line pieces-rendered-widths
                                           pieces-rendered-before-break-widths
                                           (car bps) (add1 j-1))) 
               (fl* world:allowed-overfull-ratio measure))
          (cons j-1 bps)
          bps)))
  (log-quad-debug "first-fit breakpoints = ~a" (cdr (reverse bps)))
  (cdr (reverse bps)))


;; wrap proc based on greedy proc
(define+provide wrap-first (make-wrap-proc 
                            #:make-pieces-proc make-pieces
                            #:measure-quad-proc quad-width 
                            #:compose-line-proc pieces->line
                            #:find-breakpoints-proc first-fit-proc))

;; wrap proc based on penalty function
(define+provide wrap-best (make-wrap-proc 
                           #:make-pieces-proc make-pieces
                           #:measure-quad-proc quad-width 
                           #:compose-line-proc pieces->line
                           #:find-breakpoints-proc best-fit-proc))


(define (fixed-width? q) (quad-has-attr? q world:width-key))


;; build quad out to a given width by distributing excess into spacers
;; todo: adjust this to work recursively, so that fill operation cascades down
(define+provide/contract (fill starting-quad [target-width? #f])
  ((quad?) ((or/c #f flonum?)) . ->* . quad?)
  (define target-width (fl (or target-width? (quad-attr-ref starting-quad world:measure-key)))) 
  (define subquads (quad-list starting-quad))
  (define-values (flexible-subquads fixed-subquads) (partition spacer? subquads)) ; only puts fill into spacers.
  (define width-used (fold-fl+ (map quad-width fixed-subquads)))
  (define width-remaining (round-float (fl- target-width width-used)))
  (cond
    ;; check for zero condition because we want to divide by this number
    ;; if there's no spacers, put one in
    ;; todo: go in two rounds, once for word spacers, and once for line spacers?
    ;; or separate the line alignment & word-spacing properties?
    [(fl= 0.0 (fl (length flexible-subquads))) (fill (insert-spacers-in-line starting-quad (world:horiz-alignment-default)) target-width)]
    [else (define width-per-flexible-quad (round-float (fl/ width-remaining (fl (length flexible-subquads)))))
          (define new-quad-list (map (λ(q) (if (spacer? q)
                                               (quad-attr-set q world:width-key width-per-flexible-quad)
                                               q)) subquads))
          
          (quad (quad-name starting-quad) (quad-attrs (quad-attr-set starting-quad world:width-key target-width)) new-quad-list)]))


;; add x positions to a list of fixed-width quads
;; todo: adjust this to work recursively, so that positioning operation cascades down
(define+provide/contract (add-horiz-positions starting-quad)
  ((and/c quad? fixed-width?) . -> . quad?)
  (define-values (new-quads final-width)
    (for/fold ([new-quads empty][width-so-far 0.0])([q (in-list (quad-list starting-quad))])
      (values (cons (quad-attr-set q world:x-position-key width-so-far) new-quads) (round-float (fl+ (quad-width q) width-so-far)))))
  (quad (quad-name starting-quad) (quad-attrs starting-quad) (reverse new-quads)))



(module+ main
  (define eqs (split-quad (block '(x-align center font "Equity Text B" size 10) "Foo-d" (word '(size 13) "og ") "and " (box) " Zu" (word-break '(nb "c" bb "k-")) "kerman's. Instead of a circle, the result is a picture of the code that, if it were used as an expression, would produce a circle. In other words, code is not a function, but instead a new syntactic form for creating pictures; the bit between the opening parenthesis with code is not an expression, but instead manipulated by the code syntactic form. This helps explain what we meant in the previous section when we said that racket provides require and the function-calling syntax. Libraries are not restricted to exporting values, such as functions; they can also define new syntactic forms. In this sense, Racket isn’t exactly a language at all; it’s more of an idea for how to structure a language so that you can extend it or create entirely " (word '(font "Courier" size 5) "lang."))))
  
  (define megs (split-quad (block '(size 15) "Meg is an ally.")))
  
  (define trials 1)
  (time-repeat trials (let () (wrap-first megs 36) (void)))
  (time-repeat trials (let ([measure 36]) (wrap-best megs measure) (void)))
  
  (time-repeat trials (let () (wrap-first eqs 54) (void)))
  (time-repeat trials (let ([measure 54]) (wrap-best eqs measure) (void)))
  )

