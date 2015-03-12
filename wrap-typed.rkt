#lang typed/racket/base
(require (for-syntax racket/base racket/syntax))
(require/typed sugar/list [slicef-after ((Listof Quad) (Quad . -> . Boolean) . -> . (Listof (Listof Quad)))]
               [shift ((Listof Any) (Listof Integer) . -> . (Listof Any))]
               [break-at ((Listof Quad) (Listof Nonnegative-Integer) . -> . (Listof (Listof Quad)))])
(require math/flonum (except-in racket/list flatten) racket/vector math/statistics)
(require/typed racket/list [flatten (All (A) (Rec as (U Any (Listof as))) -> (Listof Any))])
(require "ocm-typed.rkt" "quads-typed.rkt" "utils-typed.rkt" "measure-typed.rkt" "world-typed.rkt" "logger-typed.rkt")

;; predicate for the soft hyphen
(define/typed (soft-hyphen? x)
  (String . -> . Boolean)
  (equal? (format "~a" world:soft-hyphen) x))

;; visible characters that also mark possible breakpoints
(define/typed (visible-breakable? x)
  (String . -> . Boolean)
  (and (member x world:hyphens-and-dashes) #t))

;; invisible characters that denote possible breakpoints
(define/typed (invisible-breakable? x)
  (String . -> . Boolean)
  (and (member x (cons world:empty-string world:spaces)) #t))

;; union of visible & invisible
(define/typed (breakable? x)
  (Any . -> . Boolean)
  (cond
    [(string? x) (or (visible-breakable? x) (invisible-breakable? x))]
    [(word? x) (breakable? (word-string (cast x Quad)))]
    [else #f]))

;; used by insert-spacers to determine which characters 
;; can be surrounded by stretchy spacers 
(define/typed (takes-justification-space? x)
  (Any . -> . Boolean)
  (whitespace/nbsp? x))

;; test if a quad can be a word break:
;; either it's an explicit word break,
;; or it's breakable (and can be converted to a word break)
(define/typed (possible-word-break-quad? q)
  (Quad . -> . Boolean)
  (or (word-break? q) (breakable? q)))


;; convert a possible word break into an actual one
(define/typed (convert-to-word-break q)
  (Quad . -> . Quad)
  (when (not (possible-word-break-quad? q))
    (error 'convert-to-word-break "input is not a possible word break:" q))
  (define result (cond 
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
                                          [else (cast (world:default-word-break-list) HashableList)])) (quad-list q))]
                   [else #f]))
  (or result (error 'convert-to-word-break "result was a not word break for input:" q)))

(define/typed (make-unbreakable q)
  (Quad . -> . Quad)
  (quad-attr-set q world:unbreakable-key #t))


;; take list of atomic quads and gather them into pieces
;; a piece is an indivisible chunk of a line.
;; meaning, a line can wrap at a piece boundary, but not elsewhere.
;; hyphenation produces more, smaller pieces, which means more linebreak opportunities
;; but this also makes wrapping slower.
(define-type Make-Pieces-Type ((Listof Quad) . -> . (Listof Quad)))
(define/typed (make-pieces qs)
  Make-Pieces-Type
  (define-values (breakable-items items-to-make-unbreakable) (split-at-right qs (min world:minimum-last-line-chars (length qs))))
  (define unbreak-qs (append breakable-items (map make-unbreakable items-to-make-unbreakable)))
  (define lists-of-quads (slicef-after unbreak-qs (λ(q) (and (possible-word-break-quad? (cast q Quad)) (not (quad-attr-ref (cast q Quad) world:unbreakable-key #f))))))
  (define-values (first-lists-of-quads last-list-of-quads) ((inst split-last (Listof Quad)) lists-of-quads))
  (define/typed (make-first-pieces qs)
    ((Listof Quad) . -> . Quad) 
    (let-values ([(first-qs last-q) ((inst split-last Quad) qs)])
      (apply piece (list world:word-break-key (convert-to-word-break (cast last-q Quad))) (cast first-qs QuadList))))
  (append (map make-first-pieces first-lists-of-quads) 
          (list (apply piece #f (cast last-list-of-quads QuadList)))))


;; extract font attributes from quad, or get default values
(define/typed (font-attributes-with-defaults q)
  (Quad . -> . (List Nonnegative-Flonum String Symbol Symbol))
  (list
   (cast (let ([size (quad-attr-ref/parameter q world:font-size-key)])
           (if (exact-integer? size) (fl size) size)) Nonnegative-Flonum)
   (cast (quad-attr-ref/parameter q world:font-name-key) String)
   (cast (quad-attr-ref/parameter q world:font-weight-key) Symbol)
   (cast (quad-attr-ref/parameter q world:font-style-key) Symbol)))


;; get the width of a quad.
;; Try the attr first, and if it's not available, compute the width.
;; comes in fast or slow versions.
;; not designed to update the source quad.
(define-type Measure-Quad-Type (Quad . -> . Flonum))
(define/typed (quad-width q)
  Measure-Quad-Type
  (cond
    [(quad-has-attr? q world:width-key) (fl (cast (quad-attr-ref q world:width-key) Real))]
    [(ormap (λ([pred : (Any . -> . Boolean)]) (pred q)) (list char? run? word? word-break?)) 
     (apply measure-text (word-string q) 
            (font-attributes-with-defaults q))]
    [(line? q) (fl (apply + ((inst map Flonum Quad) quad-width (cast (quad-list q) (Listof Quad)))))]
    [else 0.0]))

;; get the ascent (distance from top of text to baseline)
;; used by renderer to align text runs baseline-to-baseline.
;; consult the attrs, and if not available, compute it.
;; not designed to update the source quad.
(define/typed (ascent q)
  (Quad . -> . Flonum)
  (define ascent-value-or-false (quad-attr-ref q world:ascent-key #f))
  (if ascent-value-or-false
      (cast ascent-value-or-false Flonum)
      (cond 
        [(ormap (λ([pred : (Any . -> . Boolean)]) (pred q)) (list char? run? word? word-break?)) 
         (apply measure-ascent (word-string q) (font-attributes-with-defaults q))]
        [else 0.0])))


;; convert a piece into its final form, which depends on location.
;; if a piece appears at the end of a line, it is rendered in "before break" mode.
;; if a piece appears elsewhere in a line, it is rendered in "no break" mode.
;; this allows the appearance of a piece to change depending on whether it's at the end.
;; and thus give correct behavior to trailing word spaces, soft hyphens, etc.

(define/typed (render-piece p [before-break? #f])
  ((Quad) (Boolean) . ->* . Quad)
  ;; a piece doesn't necessarily have a word-break item in it.
  ;; only needs it if the appearance of the piece changes based on location.
  ;; so words are likely to have a word-break item; boxes not.
  ;; the word break item contains the different characters needed to finish the piece.
  (define the-word-break (cast (quad-attr-ref p world:word-break-key #f) (Option Quad)))
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
(define/typed (render-piece-before-break p)
  (Quad . -> . Quad)
  (render-piece p #t))


;; helper macro to convert quad into word-break.
;; look up the break character and convert the quad based on what is found.
(define/typed (render-word-break wb key)
  (Quad Symbol . -> . Quad)
  (let ([break-char (quad-attr-ref wb key)])
    (quad (if (whitespace? break-char) 'word-break 'word)
          (hash-remove (hash-remove (quad-attrs wb) world:no-break-key) world:before-break-key) (list (cast (quad-attr-ref wb key) String)))))

;; uses macro above in no-break mode.
(define/typed (word-break->no-break wb)
  (Quad . -> . Quad)
  (render-word-break wb world:no-break-key))

;; uses macro above in before-break mode.
(define/typed (word-break->before-break wb)
  (Quad . -> . Quad)
  (render-word-break wb world:before-break-key))

;; is this the last line? compare current line-idx to total lines 
(define/typed (last-line? line)
  (Quad . -> . Boolean)
  (define line-idx (cast (quad-attr-ref line world:line-index-key #f) Number))
  (define lines (cast (quad-attr-ref line world:total-lines-key #f) Number))
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
(define/typed (render-optical-kerns exploded-line-quads)
  ((Listof Quad) . -> . (Listof Quad))
  (define/typed (overhang-width q)
    ((U Quad False) . -> . Flonum)
    (if (and (word? q) (member (word-string (cast q Quad)) world:hanging-chars))
        (* -1.0 (world:optical-overhang) (apply measure-text (word-string (cast q Quad)) (font-attributes-with-defaults (cast q Quad))))
        0.0))
  (cond
    [(not (empty? exploded-line-quads))
     ;; after exploding, each quad will have a string with one character.
     (define shifted-lists (shift exploded-line-quads '(1 0 -1)))
     (define lefts (cast (first shifted-lists) (Listof (U Quad False)))) ;; need False in type because shift fills with #f
     (define centers (cast (second shifted-lists) (Listof Quad))) ;; don't need False because shift is 0 (no fill)
     (define rights (cast (third shifted-lists) (Listof (U Quad False)))) ;; need False in type because shift fills with #f
     (for/list : (Listof Quad) ([(q-left q q-right) (in-parallel lefts centers rights)])
       (if (optical-kern? q)
           (quad-attr-set q world:width-key (fl+ (overhang-width q-left) (overhang-width q-right)))
           q))]
    [else exploded-line-quads]))


;; ultimately every line is filled to fit the whole measure.
;; spacers are used to soak up extra space left over in a line.
;; depending on where the spacers are inserted, different formatting effects are achieved.
;; e.g., left / right / centered / justified.
(define/typed+provide (insert-spacers-in-line line [alignment-override #f])
  ((Quad) ((Option Symbol)) . ->* . Quad)
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
  
  (define/typed (copy-with-attrs q attr-source)
    (Quad Quad . -> . Quad)
    (define keys-to-ignore '(width)) ; width will be determined during fill routine
    (define filtered-hash (cast (and (quad-attrs attr-source)
                                     (foldl (λ(k [ht : HashTableTop]) (hash-remove ht k)) (quad-attrs attr-source) keys-to-ignore)) QuadAttrs))
    (quad (quad-name q) (merge-attrs filtered-hash q) (quad-list q)))
  
  (quad (quad-name line) 
        (quad-attrs line) 
        (cast (flatten (let ([qs (cast (quad-list line) (Listof Quad))])
                         ;; (first qs) is a single quad, but wrap it in a list to make it spliceable
                         `(,@(cast (if before (list (copy-with-attrs before (first qs))) null) (Listof Quad))
                           ,@(map (λ([q : Quad]) (if (and middle (takes-justification-space? q)) 
                                                     (let ([interleaver (copy-with-attrs middle q)])
                                                       (list interleaver q interleaver)) 
                                                     q)) qs) 
                         ;; (last qs) is a single quad, but wrap it in a list to make it spliceable
                           ,@(cast (if after (list (copy-with-attrs after (last qs))) null) (Listof Quad))
                           ))) QuadList)))


;; installs the width in the quad.
;; this becomes the value reported by quad-width.
(define/typed (embed-width q w)
  (Quad Flonum . -> . Quad)
  (quad-attr-set q world:width-key w))

;; installs the ascent in the quad.
(define/typed (record-ascent q)
  (Quad . -> . Quad)
  (quad-attr-set q world:ascent-key (ascent q)))

;; helper function: doesn't need contract because it's already covered by the callers
(define/typed (render-pieces ps)
  ((Listof Quad) . -> . (Listof Quad))
  (define-values (initial-ps last-p) (split-last ps))
  (snoc ((inst map Quad Quad) render-piece (cast initial-ps (Listof Quad))) (render-piece-before-break (cast last-p Quad))))


(define/typed (calc-looseness total-width measure)
  (Flonum Flonum . -> . Flonum)
  (round-float (fl/ (fl- measure total-width) measure)))


;; compose pieces into a finished line.
;; take the contents of the rendered pieces and merge them.
;; compute looseness for line as a whole.
;; also add ascent to each component quad, which can be different depending on font & size.
(define-type Compose-Line-Type ((Listof Quad) (Quad . -> . Flonum) . -> . Quad))
(define/typed (pieces->line ps measure-quad-proc)
  Compose-Line-Type
  
  ;; handle optical kerns here to avoid resplitting and rejoining later.
  (define rendered-pieces (render-pieces ps))
  (define split-pieces (map quad-list rendered-pieces))
  (define line-quads (cast (append* split-pieces) (Listof Quad))) 
  (define line-quads-maybe-with-opticals 
    (if world:use-optical-kerns?
        (render-optical-kerns
         (let ([my-ok (list (optical-kern (quad-attrs (car line-quads))))]) ; take attrs from line, incl measure
           (append my-ok line-quads my-ok)))
        line-quads))
  (define merged-quads (join-quads line-quads-maybe-with-opticals))
  (define merged-quad-widths (map measure-quad-proc merged-quads)) ; 10% of function time
  
  (log-quad-debug "making pieces into line = ~v" (apply string-append (map quad->string merged-quads)))
  
  ;; if measure key isn't present, allow an error, because that's weird
  (when (not (quad-has-attr? (first line-quads) world:measure-key))
    (error 'pieces->line "quad has no measure key: ~a" (first line-quads)))
  
  (define measure (cast (quad-attr-ref (first merged-quads) world:measure-key) Flonum))
  (define looseness (calc-looseness (fl (apply + merged-quad-widths)) measure))
  
  ;; quads->line function hoists common attributes into the line
  (let* ([new-line-quads (map embed-width merged-quads merged-quad-widths)] ; 15% of time
         [new-line-quads (map record-ascent new-line-quads)] ; 35% of time
         [new-line (quads->line new-line-quads)] 
         [new-line (quad-attr-set new-line world:line-looseness-key looseness)])
    new-line))


;; a faster line-measuring function used by the wrapping function to test lines.
(define/typed (measure-potential-line ps)
  ((Listof Quad) . -> . Flonum)
  (cast (for*/sum : (U Flonum Zero) 
          ([rendered-piece (in-list (render-pieces ps))]
           [piece-quad (in-list (quad-list rendered-piece))])
          (quad-width (cast piece-quad Quad))) Flonum))


(define/typed (vector-break-at vec bps)
  ((Vectorof Any) (Listof Nonnegative-Integer) . -> . (Listof (Vectorof Any)))
  (define-values (vecs _) ;; loop backward
    (for/fold ([vecs : (Listof (Vectorof Any)) empty][end : Nonnegative-Integer (vector-length vec)])([start (in-list (reverse (cons 0 bps)))])
      (if (= start end)
          (values vecs start)
          (values (cons ((inst vector-copy Any) vec start end) vecs) start))))
  vecs)


;; makes a wrap function by combining component functions.
(define-type Wrap-Proc-Type (((Listof Quad)) (Flonum) . ->* . (Listof Quad)))
(define/typed (make-wrap-proc 
               make-pieces-proc 
               measure-quad-proc
               compose-line-proc
               find-breakpoints-proc)
  ((Make-Pieces-Type Measure-Quad-Type Compose-Line-Type Find-Breakpoints-Type) () . ->* . Wrap-Proc-Type)
  (λ(qs [measure #f])
    (let* ([measure : Flonum (fl+ (cast (or measure (quad-attr-ref/parameter (car qs) world:measure-key)) Flonum) 0.0)]
           [qs : (Listof Quad) (if (quad-has-attr? (car qs) world:measure-key)
                                   qs
                                   ((inst map Quad Quad) (λ(q) (quad-attr-set q world:measure-key measure)) qs))])
      (log-quad-debug "wrapping on measure = ~a" measure)
      (define pieces : (Listof Quad) (make-pieces-proc qs)) ; 5%
      (define bps : (Listof Nonnegative-Integer) (find-breakpoints-proc (list->vector pieces) measure)) ; 50%
      (define broken-pieces : (Listof (Listof Quad)) (break-at pieces bps)) ; 5%
      #; (define-type Compose-Line-Type ((Listof Quad) (Quad . -> . Flonum) . -> . Quad))
      ((inst map Quad (Listof Quad)) (λ(broken-piece) (compose-line-proc broken-piece measure-quad-proc)) broken-pieces)))) ; 50%

(define width? flonum?)
(define measure? flonum?)
(define (breakpoints? x) (and (list? x) (andmap integer? x)))

(define/typed (install-measurement-keys p)
  (Quad . -> . Quad)
  (define basic-width (round-float (apply + ((inst map Flonum Quad) quad-width (cast (quad-list p) (Listof Quad))))))
  (define p-word-break (cast (quad-attr-ref p world:word-break-key #f) Quad))
  (define before-break-width (fl+ basic-width (if p-word-break
                                                  (quad-width (word (quad-attrs p-word-break) (cast (quad-attr-ref p-word-break world:before-break-key) QuadListItem)))
                                                  0.0)))
  (define no-break-width (fl+ basic-width (if p-word-break
                                              (quad-width (word (quad-attrs p-word-break) (cast (quad-attr-ref p-word-break world:no-break-key) QuadListItem)))
                                              0.0)))
  (quad-attr-set* p 'bb-width before-break-width 'nb-width no-break-width))

(require sugar/debug)
(define/typed (make-piece-vectors pieces)
  ((Vectorof Quad) . -> . (values (Vectorof Flonum) (Vectorof Flonum))) 
  (define pieces-measured 
    (for/list : (Listof (Vector Flonum Flonum Flonum)) ([p (in-vector pieces)])
      (define wb (cast (quad-attr-ref p world:word-break-key #f) (U Quad False)))
      (vector
       (cast (apply + (for/list : (Listof Flonum) ([qli (in-list (quad-list p))])
                        (define q (cast qli Quad))
                        (define str (quad->string q))
                        (if (equal? str "")
                            (cast (quad-attr-ref q world:width-key 0.0) Flonum)
                            (apply measure-text (quad->string q) (font-attributes-with-defaults q))))) Flonum)
       (if wb (cast (apply measure-text (cast (quad-attr-ref wb world:no-break-key) String) (font-attributes-with-defaults wb)) Flonum) 0.0)
       (if wb (cast (apply measure-text (cast (quad-attr-ref wb world:before-break-key) String) (font-attributes-with-defaults wb)) Flonum) 0.0))))
  (values
   (for/vector : (Vectorof Flonum) ([p (in-list pieces-measured)])
     (fl+ (vector-ref p 0) (vector-ref p 1))) ; first = word length, second = nb length
   (for/vector : (Vectorof Flonum) ([p (in-list pieces-measured)]) 
     (fl+ (vector-ref p 0) (vector-ref p 2))))) ; first = word length, third = bb length


(define/typed (make-trial-line pieces-rendered-widths pieces-rendered-before-break-widths i j)
  ((Vectorof Flonum) (Vectorof Flonum) Nonnegative-Integer Nonnegative-Integer . -> . (Vectorof Flonum))
  (let ([vec (vector-copy pieces-rendered-widths i j)])
    (vector-set! vec (sub1 (vector-length vec)) (vector-ref pieces-rendered-before-break-widths (sub1 j)))
    vec))

(define/typed (get-line-width line)
  ((Vectorof Flonum) . -> . Flonum)
  (round-float (apply + (vector->list line))))

(struct $penalty ([hyphens : Nonnegative-Integer][width : Value-Type]) #:transparent #:mutable)

;; top-level adaptive wrap proc.
;; first-fit and best-fit are variants.
(define-type Find-Breakpoints-Type ((Vectorof Quad) Flonum . -> . (Listof Nonnegative-Integer)))
(define/typed (adaptive-fit-proc pieces measure [use-first? #t] [use-best? #t])
  (((Vectorof Quad) Flonum) (Boolean Boolean) . ->* . (Listof Nonnegative-Integer))
  
  ;; this is the winning performance strategy: extract the numbers first, then just wrap on those.
  ;; todo: how to avoid re-measuring pieces later?
  ;; todo: how to retain information about words per line and hyphen at end?
  (define-values (pieces-rendered-widths pieces-rendered-before-break-widths)
    (make-piece-vectors pieces))
  (define pieces-with-word-space ((inst vector-map Boolean Quad) (λ(piece) (and (quad-has-attr? piece world:word-break-key) (equal? (quad-attr-ref (cast (quad-attr-ref piece world:word-break-key) Quad) 'nb) " "))) pieces))
  
  (define (make-first-fit-bps-and-widths)
    (define-values (folded-bps folded-widths) 
      (for/fold ([bps : (Listof Nonnegative-Integer) '(0)][line-widths : (Listof Flonum) empty])([j-1 (in-range (vector-length pieces))])
        (define line-width (get-line-width (make-trial-line pieces-rendered-widths
                                                            pieces-rendered-before-break-widths
                                                            (car bps) (cast (add1 j-1) Nonnegative-Integer))))
        (if (fl> line-width (fl* world:allowed-overfull-ratio measure))
            (values (cons (cast j-1 Nonnegative-Integer) bps) (cons line-width line-widths))
            (values bps line-widths))))
    (values (cdr (reverse folded-bps)) (reverse folded-widths)))
  
  (define (fu-formula)
    (define line-count (length trial-line-widths))
    (cond
      [(<= line-count 2) 1.0] ; signals that first-fit is always OK with 1 or 2 lines
      [else ; only measure middle lines. we know bps has at least 2 bps
       (define looseness-stddev (stddev ((inst map Flonum Flonum) (λ(x) (calc-looseness x measure)) (drop-right (drop trial-line-widths 1) 1))))         
       (define piece-count (vector-length pieces-rendered-widths)) 
       (define pieces-per-line (fl/ (fl piece-count) (sub1 (fl line-count)))) ; todo: more accurate to count only pieces in middle
       (apply + (list 2.2 (fllog (flabs (cast looseness-stddev Flonum))) (* 0.09 pieces-per-line)))])) ; the FU FORMULA
  
  ;; only buy first-fit-bps if use-first? is true.
  ;; use (values '(0) '(0.0)) as void-ish values that will typecheck properly.
  (define-values (first-fit-bps trial-line-widths) (if use-first? (make-first-fit-bps-and-widths) (values '(0) '(0.0))))
  
  (cond
    ;; possible outcomes at this branch:
    ;; adaptive wrap: use-first and use-best are true, so first-fit-bps will exist, and fu-formula will be used.
    ;; first-fit wrap: use-first is true but not use-best. So first-fit-bps will be returned regardless.
    ;; best-fit wrap: use-first is false but use-best is true. So first-fit-bps will be skipped, and move on to best-fit.
    [(and use-first? (if use-best? (fl> (fu-formula) 0.0) #t))  
     (log-quad-debug "first-fit breakpoints = ~a" first-fit-bps)
     first-fit-bps]
    [else
     
     (define/typed ($penalty->value x)
       ($penalty . -> . Value-Type)
       ($penalty-width x))
     (define initial-value ($penalty 0 0.0))
     
     (log-quad-debug "~a pieces to wrap = ~v" (vector-length pieces) (vector-map quad->string pieces))
     
     (define/typed (penalty i j)
       Matrix-Proc-Type
       (cond
         [(or (>= i j) ; implies negative or zero length line 
              (> j (vector-length pieces))) ; exceeds available pieces
          ($penalty 0 (fl* -1.0 (fl i)))] ; ocm out of bounds signal
         [else
          (define penalty-up-to-i (cast (ocm-min-entry ocm i) $penalty))
          (define last-piece-to-test (vector-ref pieces (sub1 j)))
          (define new-hyphen?
            (and (quad-has-attr? last-piece-to-test world:word-break-key)
                 (equal? (cast (quad-attr-ref (cast (quad-attr-ref last-piece-to-test world:word-break-key) Quad) world:before-break-key) String) "-")))
          (define cumulative-hyphens (if (not new-hyphen?) 
                                         0 
                                         (add1 ($penalty-hyphens penalty-up-to-i))))
          
          ($penalty 
           cumulative-hyphens
           (round-float
            (apply + (list
                      (if (> cumulative-hyphens world:hyphen-limit)
                          (fl world:hyphen-penalty)
                          0.0)
                      (fl world:new-line-penalty) 
                      ($penalty->value penalty-up-to-i)
                      (let ([line-width (get-line-width (make-trial-line pieces-rendered-widths pieces-rendered-before-break-widths i j))])
                        (cond
                          ;; overfull line: huge penalty prevents break; multiplier is essential for monotonicity.
                          ;; multiply by -1 because line-width is longer than measure, thus diff is negative
                          [(fl> line-width (fl* world:allowed-overfull-ratio measure))
                           (fl* (fl- line-width measure) (flexpt 10.0 7.0))] 
                          ;; standard penalty, optionally also applied to last line (by changing operator)
                          [((if world:last-line-can-be-short < <=) j (vector-length pieces)) 
                           (define words (fl (vector-count (λ(x) x) (vector-copy pieces-with-word-space i (sub1 j)))))     
                           (fl/ (flexpt (fl- measure line-width) 2.0) (flmax 1.0 words))]
                          ;; only option left is (= j (vector-length pieces)), meaning we're on the last line.
                          ;; 0 penalty means any length is ok.
                          ;[(< (length pieces-to-test) (world:minimum-last-line-pieces)) 50000]
                          [else 0.0]))))))]))
     
     (define ocm : OCM-Type (make-ocm penalty (cast $penalty->value Entry->Value-Type) initial-value))
     
     ;; starting from last position, ask ocm for position of row minimum (= new-pos)
     ;; collect this value, and use it as the input next time
     ;; until you reach first position.    
     (define first-position 0)
     (define last-position (vector-length pieces))
     (define result (let loop : (Listof Nonnegative-Integer) ([pos : Nonnegative-Integer last-position][acc : (Listof Nonnegative-Integer) null])
                      (let ([next-pos (cast (ocm-min-index ocm pos) Nonnegative-Integer)]) ; first look ahead ...
                        (if (= next-pos first-position) ; therefore we're done
                            acc
                            (loop next-pos (cons next-pos acc))))))
     
     (log-quad-debug "best-fit breakpoints = ~a" result)
     result]))


;; wrap proc based on greedy proc
(define-syntax-rule (define+provide name expr ...)
  (begin 
    (provide name)
    (define name expr ...)))

(define+provide wrap-first (make-wrap-proc  
                    make-pieces 
                    quad-width 
                    pieces->line 
                    (λ(x y) (adaptive-fit-proc (cast x (Vectorof Quad)) (cast y Flonum) #t #f))))

;; wrap proc based on penalty function
(define+provide wrap-best (make-wrap-proc
                   make-pieces
                   quad-width
                   pieces->line
                   (λ(x y) (adaptive-fit-proc (cast x (Vectorof Quad)) (cast y Flonum) #f #t)))) ; note difference in boolean args

(define+provide wrap-adaptive (make-wrap-proc 
                       make-pieces
                       quad-width
                       pieces->line
                       adaptive-fit-proc))


(define/typed (fixed-width? q) 
  (Quad . -> . Boolean)
  (quad-has-attr? q world:width-key))


;; build quad out to a given width by distributing excess into spacers
;; todo: adjust this to work recursively, so that fill operation cascades down
(define/typed+provide (fill starting-quad [target-width? #f])
  ((Quad) ((Option Flonum)) . ->* . Quad)
  (define target-width (fl (or target-width? (cast (quad-attr-ref starting-quad world:measure-key) Flonum)))) 
  (define subquads (cast (quad-list starting-quad) (Listof Quad)))
  (define-values (flexible-subquads fixed-subquads) (partition spacer? subquads)) ; only puts fill into spacers.
  (define width-used (apply + ((inst map Flonum Quad) quad-width fixed-subquads)))
  (define width-remaining (round-float (- target-width width-used)))
  (cond
    ;; check for zero condition because we want to divide by this number
    ;; if there's no spacers, put one in
    ;; todo: go in two rounds, once for word spacers, and once for line spacers?
    ;; or separate the line alignment & word-spacing properties?
    [(fl= 0.0 (fl (length flexible-subquads))) (fill (insert-spacers-in-line starting-quad (world:horiz-alignment-default)) target-width)]
    [else (define width-per-flexible-quad (round-float (fl/ width-remaining (fl (length flexible-subquads)))))
          (define new-quad-list ((inst map Quad Quad) (λ(q) (if (spacer? q)
                                                                (quad-attr-set q world:width-key width-per-flexible-quad)
                                                                q)) subquads))
          
          (quad (quad-name starting-quad) (quad-attrs (quad-attr-set starting-quad world:width-key target-width)) new-quad-list)]))


;; add x positions to a list of fixed-width quads
;; todo: adjust this to work recursively, so that positioning operation cascades down
(define/typed+provide (add-horiz-positions starting-quad)
  (Quad . -> . Quad)
  (define-values (new-quads final-width)
    (for/fold ([new-quads : (Listof Quad) empty][width-so-far : Flonum 0.0])([qi (in-list (quad-list starting-quad))])
      (define q (cast qi Quad))
      (values (cons (quad-attr-set q world:x-position-key width-so-far) new-quads) (round-float (fl+ (quad-width q) width-so-far)))))
  (quad (quad-name starting-quad) (quad-attrs starting-quad) (reverse new-quads)))

