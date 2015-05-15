#lang typed/racket/base
(require racket/list math/flonum typed/racket/class)
(require typed/sugar/define typed/sugar/list)
(require/typed csp
               [problem%  (Class (init-field [solver Any])
                                 (field [_solver Any])
                                 (field [_variable-domains Any])
                                 (field [_constraints Any])
                                 [reset (-> Void)]
                                 [custom-print (Output-Port Integer -> Void)]
                                 [custom-display (Output-Port -> Void)]
                                 [custom-write (Output-Port -> Void)]
                                 [add-variable (Any (Listof Any) . -> . Void)]
                                 [add-variables ((Listof Any) Any . -> . Void)]
                                 [add-constraint ((Index . -> . Boolean) (Listof Any) . -> . Void)][get-solution (-> HashTableTop)]
                                 [get-solutions (-> (Listof (HashTable String Integer)))]
                                 [get-solution-iter (-> HashTableTop)]
                                 [set-solver (Any . -> . Void)]
                                 [get-solver (-> Any)])])
(require "quads-typed.rkt" "utils-typed.rkt" "wrap-typed.rkt" "measure-typed.rkt" "world-typed.rkt" "logger-typed.rkt" "core-types.rkt")

(define-type Block-Type (Listof Quad))
(define-type Multicolumn-Type (Listof Block-Type))
(define-type Multipage-Type (Listof Multicolumn-Type))

(define/typed (cons-reverse xs ys)
  (All (A B) ((Listof A) (Listof B) -> (Pairof (Listof A) (Listof B))))
  ((inst cons (Listof A) (Listof B)) ((inst reverse A) xs) ys))

(define/typed+provide (input->nested-blocks i)
  (Quad . -> . (Listof Multipage-Type))
  (define-values (mps mcs bs b)
    (for/fold ([multipages : (Listof Multipage-Type) empty]
               [multicolumns : (Listof Multicolumn-Type) empty]
               [blocks : (Listof Block-Type) empty]
               [block-acc : Block-Type empty])
              ([q (in-list (split-quad i))])
      (cond
        [(page-break? q) (values (cons-reverse (cons-reverse (cons-reverse block-acc blocks) multicolumns) multipages) empty empty empty)]
        [(column-break? q) (values multipages (cons-reverse (cons-reverse block-acc blocks) multicolumns) empty empty)]
        [(block-break? q) (values multipages multicolumns (cons-reverse block-acc blocks) empty)]
        [else (values multipages multicolumns blocks (cons q block-acc))])))
  (reverse (cons-reverse (cons-reverse ((inst cons-reverse Quad Block-Type) b bs) mcs) mps)))

(define/typed+provide (merge-adjacent-within q)
  (Quad . -> . Quad)
  (quad (quad-name q) (quad-attrs q) (join-quads (cast (quad-list q) (Listof Quad)))))

(define/typed+provide (hyphenate-quad-except-last-word q)
  (Quad . -> . Quad)
  (log-quad-debug "last word will not be hyphenated")
  (define-values (first-quads last-quad) ((inst split-last QuadListItem) (quad-list q)))
  (quad (quad-name q) (quad-attrs q) (snoc ((inst map QuadListItem QuadListItem) hyphenate-quad first-quads) last-quad)))

(define/typed+provide (average-looseness lines)
  ((Listof Quad) . -> . Float)
  (if (<= (length lines) 1)
      0.0
      (let ([lines-to-measure (drop-right lines 1)]) ; exclude last line from looseness calculation
        (round-float (/ (foldl fl+ 0.0 ((inst map Float Quad) (λ(line) (cast (quad-attr-ref line world:line-looseness-key 0.0) Float)) lines-to-measure)) (- (fl (length lines)) 1.0))))))


(define/typed+provide (log-debug-lines lines)
  ((Listof Quad) . -> . (Listof String)) 
  (log-quad-debug "line report:")
  (for/list : (Listof String) ([(line idx) (in-indexed lines)])
    (format "~a/~a: ~v ~a" idx
            (length lines) 
            (quad->string line)
            (quad-attr-ref line world:line-looseness-key))))


(define/typed+provide (block->lines b)
  (BlockQuad . -> . (Listof LineQuad)) ;; todo: introduce a Quad subtype where quad-list is guaranteed to be all Quads (no strings)
  (define quality (assert (quad-attr-ref/parameter b world:quality-key) Index?))
  (define/typed (wrap-quads qs)
    ((Listof Quad) . -> . (Listof LineQuad))
    (define wrap-proc (cond
                        [(>= quality world:max-quality) wrap-best] 
                        [(<= quality world:draft-quality) wrap-first]
                        [else wrap-adaptive])) 
    (wrap-proc qs))
  (log-quad-debug "wrapping lines")
  (log-quad-debug "quality = ~a" quality)
  (log-quad-debug "looseness tolerance = ~a" world:line-looseness-tolerance)
  (define wrapped-lines-without-hyphens (wrap-quads (cast (quad-list b) (Listof Quad)))) ; 100/150
  (log-quad-debug* (log-debug-lines wrapped-lines-without-hyphens))
  (define avg-looseness (average-looseness wrapped-lines-without-hyphens))
  (define gets-hyphenation? (and world:use-hyphenation?
                                 (fl> avg-looseness world:line-looseness-tolerance)))
  (log-quad-debug "average looseness = ~a" avg-looseness)
  (log-quad-debug (if gets-hyphenation? "hyphenating" "no hyphenation needed"))
  
  (define wrapped-lines (if gets-hyphenation?
                            (wrap-quads (split-quad (cast ((if world:allow-hyphenated-last-word-in-paragraph
                                                               hyphenate-quad
                                                               hyphenate-quad-except-last-word) (merge-adjacent-within b)) Quad)))
                            wrapped-lines-without-hyphens))
  (when gets-hyphenation? (log-quad-debug* (log-debug-lines wrapped-lines)))
  
  
  (log-quad-debug "final looseness = ~a" (average-looseness wrapped-lines))
  (map insert-spacers-in-line
       (for/list : (Listof LineQuad) ([line-idx (in-naturals)][the-line (in-list wrapped-lines)])
         (apply line (attr-change (quad-attrs the-line) (list 'line-idx line-idx 'lines (length wrapped-lines))) (quad-list the-line)))))


(define/typed+provide (number-pages ps)
  ((Listof PageQuad) . -> . (Listof PageQuad))
  (for/list ([i (in-naturals)][p (in-list ps)])
    (apply page (merge-attrs (quad-attrs p) `(page ,i)) (quad-list p))))

(define/typed+provide (pages->doc ps)
  ((Listof PageQuad) . -> . DocQuad)  
  ;; todo: resolve xrefs and other last-minute tasks
  ;; todo: generalize computation of widths and heights, recursively
  (define/typed (columns-mapper page-in)
    (PageQuad . -> . PageQuad)
    (apply page (quad-attrs page-in)
           (map add-vert-positions (for/list : (Listof ColumnQuad) ([col (in-list (quad-list page-in))])
             (assert col ColumnQuad?)
             (apply column (quad-attrs col) (map (λ([ln : Quad]) (assert ln LineQuad?) (compute-line-height (add-horiz-positions (fill ln)))) (quad-list col))))))) 
  (define mapped-pages (map columns-mapper (number-pages ps)))
  (define doc (quads->doc mapped-pages))
  doc)


(define/typed+provide (lines->columns lines)
  ((Listof LineQuad) . -> . (Listof ColumnQuad))
  (define prob (new problem% [solver #f]))
  (define max-column-lines world:default-lines-per-column)
  (define-values (columns ignored-return-value)
    (for/fold ([columns : (Listof ColumnQuad) empty][lines-remaining : (Listof LineQuad) lines])
              ([col-idx : Nonnegative-Integer (stop-before (in-naturals) (λ(x) (empty? lines-remaining)))])
      (log-quad-info "making column ~a" (add1 col-idx))
      ;; domain constraint is best way to simplify csp, because it limits the search space.
      ;; search from largest possible value to smallest.
      ;; largest possible is the minimum of the max column lines, or 
      ;; the number of lines left (modulo minimum page lines) ...
      (define viable-column-range
        (range (min max-column-lines (max 
                                      (length lines-remaining)
                                      (- (length lines-remaining) world:minimum-lines-per-column)))
               ;; ... and the smallest possible is 1, or the current minimum lines.
               ;; (sub1 insures that range is inclusive of last value.)
               (sub1 (min 1 world:minimum-lines-per-column)) -1))
      
      (log-quad-debug "viable number of lines for this column to start =\n~a" viable-column-range)
      (send prob add-variable "column-lines" viable-column-range)
      
      
      ;; greediness constraint: leave enough lines for next page, or take all
      (define/typed (greediness-constraint pl)
        (Index . -> . Boolean)
        (define leftover (- (length lines-remaining) pl))
        (or (= leftover 0) (>= leftover world:minimum-lines-per-column)))
      (send prob add-constraint greediness-constraint '("column-lines"))
      
      (log-quad-debug "viable number of lines after greediness constraint =\n~a" ((inst map Integer (HashTable String Integer)) (λ(x) (hash-ref x "column-lines")) (send prob get-solutions)))
      
      ;; last lines constraint: don't take page that will end with too few lines of last paragraph.
      (define/typed (last-lines-constraint pl)
        (Index . -> . Boolean)
        (define last-line-of-page ((inst list-ref Quad) lines-remaining (sub1 pl)))
        (define lines-in-this-paragraph (assert (quad-attr-ref last-line-of-page world:total-lines-key) Index?))
        (define line-index-of-last-line (assert (quad-attr-ref last-line-of-page world:line-index-key) Index?))
        (define (paragraph-too-short-to-meet-constraint?)
          (< lines-in-this-paragraph world:min-last-lines))
        (or (paragraph-too-short-to-meet-constraint?)
            (>= (add1 line-index-of-last-line) world:min-last-lines)))
      (send prob add-constraint last-lines-constraint '("column-lines"))
      
      (log-quad-debug "viable number of lines after last-lines constraint =\n~a" ((inst map Integer (HashTable String Integer)) (λ(x) (hash-ref x "column-lines")) (send prob get-solutions)))
      
      ;; first lines constraint: don't take page that will leave too few lines at top of next page
      (define/typed (first-lines-constraint pl lines-remaining)
        (Index (Listof Quad) . -> . Boolean)
        (define last-line-of-page (list-ref lines-remaining (sub1 pl)))
        (define lines-in-this-paragraph (assert (quad-attr-ref last-line-of-page world:total-lines-key) integer?))
        (define line-index-of-last-line (assert (quad-attr-ref last-line-of-page world:line-index-key) integer?))
        (define lines-that-will-remain (- lines-in-this-paragraph (add1 line-index-of-last-line)))
        (define (paragraph-too-short-to-meet-constraint?)
          (< lines-in-this-paragraph world:min-first-lines))
        (or (paragraph-too-short-to-meet-constraint?)
            (= 0 lines-that-will-remain) ; ok to use all lines ...
            (>= lines-that-will-remain world:min-first-lines))) ; but if any remain, must be minimum number.
      (send prob add-constraint (λ(x) (first-lines-constraint (assert x Index?) lines-remaining)) '("column-lines"))
      
      (log-quad-debug "viable number of lines after first-lines constraint =\n~a" ((inst map Integer (HashTable String Integer)) (λ(x) (hash-ref x "column-lines")) (send prob get-solutions)))
      
      (define s (send prob get-solution))
      (define how-many-lines-to-take (assert (hash-ref s "column-lines") index?))
      (define-values (lines-to-take lines-to-leave) (split-at lines-remaining how-many-lines-to-take))
      (log-quad-debug "taking ~a lines for column ~a:" how-many-lines-to-take (add1 col-idx))
      (map (λ([idx : Index] [line : LineQuad]) (log-quad-debug "~a:~a ~v" (add1 col-idx) (add1 idx) (quad->string line))) (range how-many-lines-to-take) lines-to-take)
      (send prob reset)
      (define new-column (quads->column lines-to-take))
      (values (cons (apply column (attr-change (quad-attrs new-column) (list world:column-index-key col-idx)) (quad-list new-column)) columns) lines-to-leave)))
  (reverse columns))


(define/typed+provide (columns->pages cols)
  ((Listof ColumnQuad) . -> . (Listof PageQuad))
  (define columns-per-page (cast (quad-attr-ref/parameter (car cols) world:column-count-key) Positive-Integer))
  (define column-gutter (assert (quad-attr-ref/parameter (car cols) world:column-gutter-key) flonum?))
  ;; don't use default value here. If the col doesn't have a measure key, 
  ;; it deserves to be an error, because that means the line was composed incorrectly.
  (when (not (quad-has-attr? (car cols) world:measure-key))
    (error 'columns->pages "column attrs contain no measure key: ~a ~a" (quad-attrs (car cols)) (quad-car (car cols))))
  (define column-width (assert (quad-attr-ref (car cols) world:measure-key) flonum?))
  (define width-of-printed-area (+ (* columns-per-page column-width) (* (sub1 columns-per-page) column-gutter)))
  (define result-pages
    ((inst map PageQuad (Listof Quad)) (λ(cols) (quads->page cols)) 
                                       (for/list : (Listof (Listof Quad)) ([page-cols (in-list (slice-at cols columns-per-page))])
                                         (define-values (last-x cols)
                                           (for/fold ([current-x : Float (/ (- (world:paper-width-default) width-of-printed-area) 2.0)]
                                                      [cols : (Listof Quad) empty]) 
                                                     ([col (in-list page-cols)][idx (in-naturals)])
                                             (values (foldl fl+ 0.0 (list current-x column-width column-gutter)) (cons (quad-attr-set* col (list 'x current-x 'y 40.0 world:column-index-key idx)) cols))))
                                         (reverse cols))))
  result-pages)

(define current-eof (make-parameter (gensym)))
(define (eof? x) (equal? x (current-eof)))

(define/typed (block-quads->lines qs)
  ((Listof Quad) . -> . (Listof LineQuad))
  (block->lines (quads->block qs)))

(require typed/sugar/debug)
(define/typed+provide (typeset x)
  (Quad . -> . DocQuad)
  (load-text-cache-file)
  
  (define pages (append*
                 (for/list : (Listof (Listof PageQuad))
                   ([multipage (in-list (input->nested-blocks x))])
                   (columns->pages (append*
                                    (for/list : (Listof (Listof ColumnQuad))
                                      ([multicolumn (in-list multipage)])
                                      (lines->columns (append*
                                                       (for/list : (Listof (Listof LineQuad))
                                                         ([block-quads (in-list multicolumn)])
                                                         (block-quads->lines block-quads))))))))))
  (define doc (pages->doc pages))
  (update-text-cache-file)
  doc)




