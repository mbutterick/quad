#lang typed/racket/base
(require (for-syntax racket/base racket/syntax))
(require/typed sugar/list [slicef-after ((Listof Quad) (Quad . -> . Boolean) . -> . (Listof (Listof Quad)))]
               [shift ((Listof Any) (Listof Integer) . -> . (Listof Any))])
(require math/flonum (except-in racket/list flatten))
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
(define/typed (make-pieces qs)
  ((Listof Quad) . -> . (Listof Quad))
  (define-values (breakable-items items-to-make-unbreakable) (split-at-right qs (min world:minimum-last-line-chars (length qs))))
  (define unbreak-qs (append breakable-items (map make-unbreakable items-to-make-unbreakable)))
  (define lists-of-quads (slicef-after unbreak-qs (λ(q) (and (possible-word-break-quad? (cast q Quad)) (not (quad-attr-ref (cast q Quad) world:unbreakable-key #f))))))
  (define-values (first-lists-of-quads last-list-of-quads) (split-last lists-of-quads))
  (define (make-first-pieces qs)
    (let-values ([(first-qs last-q) (split-last qs)])
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
(define/typed (quad-width q)
  (Quad . -> . Flonum)
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
  (or (cast (quad-attr-ref q world:ascent-key #f) Flonum)
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
  (define the-word-break (cast (quad-attr-ref p world:word-break-key #f) Quad))
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
          (hash-remove (hash-remove (quad-attrs wb) world:no-break-key) world:before-break-key) (list (cast (quad-attr-ref wb key) Quad)))))

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
    (Quad . -> . Flonum)
    (if (and (word? q) (member (word-string q) world:hanging-chars))
        (* -1.0 (world:optical-overhang) (apply measure-text (word-string q) (font-attributes-with-defaults q)))
        0.0))
  (cond
    [(not (empty? exploded-line-quads))
     ;; after exploding, each quad will have a string with one character.
     (define shifted-lists (shift exploded-line-quads '(1 0 -1)))
     (define lefts (cast (first shifted-lists) (Listof Quad)))
     (define centers (cast (second shifted-lists) (Listof Quad)))
     (define rights (cast (third shifted-lists) (Listof Quad)))
     (for/list : (Listof Quad) ([(q-left q q-right) (in-parallel lefts centers rights)])
       (if (optical-kern? q)
           (quad-attr-set q world:width-key (fl+ (overhang-width q-left) (overhang-width q-right)))
           q))]
    [else exploded-line-quads]))


;; ultimately every line is filled to fit the whole measure.
;; spacers are used to soak up extra space left over in a line.
;; depending on where the spacers are inserted, different formatting effects are achieved.
;; e.g., left / right / centered / justified.
(define/typed (insert-spacers-in-line line [alignment-override #f])
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
                `(,@(cast (if before (copy-with-attrs before (first qs)) null) (Listof Quad))
                  ,@(map (λ([q : Quad]) (if (and middle (takes-justification-space? q)) 
                                          (let ([interleaver (copy-with-attrs middle q)])
                                            (list interleaver q interleaver)) 
                                          q)) qs) 
                  ,@(cast (if after (copy-with-attrs after (last qs)) null) (Listof Quad))
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
(define/typed (pieces->line ps measure-quad-proc)
  ((Listof Quad) (Quad . -> . Flonum) . -> . Quad)
  
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
