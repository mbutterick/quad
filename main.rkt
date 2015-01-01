#lang racket/base
(require racket/list sugar racket/contract racket/function math/flonum)
(require "quads.rkt" "utils.rkt" "wrap.rkt" "measure.rkt" "world.rkt" "logger.rkt")
(provide typeset)

(define+provide/contract (input->multipages i)
  (input? . -> . multipages?)
  (define exploded-input (split-quad i))
  (map quads->multipage (split-on-page-breaks exploded-input)))


(define/contract (multipage->multicolumns mp)
  (multipage? . -> . multicolumns?)
  (map quads->multicolumn (split-on-column-breaks (quad-list mp))))


(define+provide/contract (multicolumn->blocks mc)
  (multicolumn? . -> . blocks?)
  ;; segfault happens in next line
  (map quads->block (split-on-block-breaks (quad-list mc))))


(define+provide/contract (merge-adjacent-within q)
  (quad? . -> . quad?)
  (quad (quad-name q) (quad-attrs q) (join-quads (quad-list q))))

(define (hyphenate-quad-except-last-word q)
  (log-quad-debug "last word will not be hyphenated")
  (define-values (first-quads last-quad) (split-last (quad-list q)))
  (quad (quad-name q) (quad-attrs q) (snoc (map hyphenate-quad first-quads) last-quad)))

(define+provide/contract (average-looseness lines)
  (lines? . -> . flonum?)
  (if (<= (length lines) 1)
      0.0
      (let ([lines-to-measure (drop-right lines 1)]) ; exclude last line from looseness calculation
        (round-float (fl/ (fold-fl+ (map (λ(line) (quad-attr-ref line world:line-looseness-key 0.0)) lines-to-measure)) (fl- (fl (length lines)) 1.0))))))


(define (log-debug-lines lines)
  (log-quad-debug "line report:")
  (for/list ([(line idx) (in-indexed lines)])
    (format "~a/~a: ~v ~a" idx
            (length lines) 
            (quad->string line)
            (quad-attr-ref line world:line-looseness-key))))
(require racket/trace)
(define+provide/contract (block->lines b-in)
  (block? . -> . lines?)
  (define b (if (ormap string? (quad-list b-in))
                (quads->block (split-quad b-in))
                b-in))
  (define quality (quad-attr-ref/parameter b world:quality-key))
  (define (wrap-quads qs)
    (define wrap-proc (cond
                        [(>= quality world:max-quality) wrap-best] 
                        [(<= quality world:draft-quality) wrap-first]
                        [else wrap-adaptive])) 
    (wrap-proc qs))
  (log-quad-debug "wrapping lines")
  (log-quad-debug "quality = ~a" quality)
  (log-quad-debug "looseness tolerance = ~a" world:line-looseness-tolerance)
  (define wrapped-lines-without-hyphens (wrap-quads (quad-list b))) ; 100/150
  (log-quad-debug* (log-debug-lines wrapped-lines-without-hyphens))
  (define avg-looseness (average-looseness wrapped-lines-without-hyphens))
  (define gets-hyphenation? (and world:use-hyphenation?
                                 (fl> avg-looseness world:line-looseness-tolerance)))
  (log-quad-debug "average looseness = ~a" avg-looseness)
  (log-quad-debug (if gets-hyphenation? "hyphenating" "no hyphenation needed"))
  
  (define wrapped-lines (if gets-hyphenation?
                            (wrap-quads (split-quad ((if world:allow-hyphenated-last-word-in-paragraph
                                                         hyphenate-quad
                                                         hyphenate-quad-except-last-word) (merge-adjacent-within b))))
                            wrapped-lines-without-hyphens))
  
  (when gets-hyphenation? (log-quad-debug* (log-debug-lines wrapped-lines)))
  
  
  (log-quad-debug "final looseness = ~a" (average-looseness wrapped-lines))
  (map insert-spacers-in-line
       (for/list ([line-idx (in-naturals)][line (in-list wrapped-lines)])
         (quad-attr-set* line 'line-idx line-idx 'lines (length wrapped-lines)))))


(define+provide/contract (number-pages ps)
  (pages? . -> . pages?)
  (for/list ([i (in-naturals)][p (in-list ps)])
    (quad (quad-name p) (merge-attrs (quad-attrs p) `(page ,i)) (quad-list p))))

(define+provide/contract (pages->doc ps)
  (pages? . -> . doc?)
  (map quad-attrs (quad-list (first ps)))
  
  ;; todo: resolve xrefs and other last-minute tasks
  ;; todo: generalize computation of widths and heights, recursively
  (define (columns-mapper page)
    (quad-map (compose1 add-vert-positions (curry quad-map (compose1 compute-line-height add-horiz-positions fill))) page))
  (define mapped-pages (map columns-mapper (number-pages ps)))
  (define doc (quads->doc mapped-pages))
  doc)

(require racket/class csp)
(define+provide/contract (lines->columns lines)
  (lines? . -> . columns?)
  (define prob (new problem%))
  (define max-column-lines world:default-lines-per-column)
  (define-values (columns ignored-return-value)
    (for/fold ([columns null][lines-remaining lines])([col-idx (in-naturals)] #:break (empty? lines-remaining))
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
      (define (greediness-constraint pl)
        (define leftover (- (length lines-remaining) pl))
        (or (= leftover 0) (>= leftover world:minimum-lines-per-column)))
      (send prob add-constraint greediness-constraint '("column-lines"))
      
      (log-quad-debug "viable number of lines after greediness constraint =\n~a" (map (curryr hash-ref "column-lines") (send prob get-solutions)))
      
      ;; last lines constraint: don't take page that will end with too few lines of last paragraph.
      (define (last-lines-constraint pl) 
        (define last-line-of-page (list-ref lines-remaining (sub1 pl)))
        (define lines-in-this-paragraph (quad-attr-ref last-line-of-page world:total-lines-key))
        (define line-index-of-last-line (quad-attr-ref last-line-of-page world:line-index-key))
        (define (paragraph-too-short-to-meet-constraint?)
          (< lines-in-this-paragraph world:min-last-lines))
        (or (paragraph-too-short-to-meet-constraint?)
            (>= (add1 line-index-of-last-line) world:min-last-lines)))
      (send prob add-constraint last-lines-constraint '("column-lines"))
      
      (log-quad-debug "viable number of lines after last-lines constraint =\n~a" (map (curryr hash-ref "column-lines") (send prob get-solutions)))
      
      ;; first lines constraint: don't take page that will leave too few lines at top of next page
      (define (first-lines-constraint pl lines-remaining)
        (define last-line-of-page (list-ref lines-remaining (sub1 pl)))
        (define lines-in-this-paragraph (quad-attr-ref last-line-of-page world:total-lines-key))
        (define line-index-of-last-line (quad-attr-ref last-line-of-page world:line-index-key))
        (define lines-that-will-remain (- lines-in-this-paragraph (add1 line-index-of-last-line)))
        (define (paragraph-too-short-to-meet-constraint?)
          (< lines-in-this-paragraph world:min-first-lines))
        (or (paragraph-too-short-to-meet-constraint?)
            (= 0 lines-that-will-remain) ; ok to use all lines ...
            (>= lines-that-will-remain world:min-first-lines))) ; but if any remain, must be minimum number.
      (send prob add-constraint (curryr first-lines-constraint lines-remaining) '("column-lines"))
      
      (log-quad-debug "viable number of lines after first-lines constraint =\n~a" (map (curryr hash-ref "column-lines") (send prob get-solutions)))
      
      
      (define s (send prob get-solution))
      (define how-many-lines-to-take (hash-ref s "column-lines"))
      (define-values (lines-to-take lines-to-leave) (split-at lines-remaining how-many-lines-to-take))
      (log-quad-debug "taking ~a lines for column ~a:" how-many-lines-to-take (add1 col-idx))
      (map (λ(idx line) (log-quad-debug "~a:~a ~v" (add1 col-idx) (add1 idx) (quad->string line))) (range how-many-lines-to-take) lines-to-take)
      (send prob reset)
      (values (cons (quad-attr-set (quads->column lines-to-take) world:column-index-key col-idx) columns) lines-to-leave)))
  (reverse columns))

(define/contract (columns->pages cols)
  (columns? . -> . pages?)
  (define columns-per-page (quad-attr-ref/parameter (car cols) world:column-count-key))
  (define column-gutter (quad-attr-ref/parameter (car cols) world:column-gutter-key))
  ;; don't use default value here. If the col doesn't have a measure key, 
  ;; it deserves to be an error, because that means the line was composed incorrectly.
  (when (not (quad-has-attr? (car cols) world:measure-key))
    (error 'columns->pages "column attrs contain no measure key: ~a ~a" (quad-attrs (car cols)) (quad-car (car cols))))
  (define column-width (quad-attr-ref (car cols) world:measure-key))
  (define width-of-printed-area (+ (* columns-per-page column-width) (* (sub1 columns-per-page) column-gutter)))
  (define result-pages
    (map (λ(cols) (quads->page cols)) 
         (for/list ([page-cols (in-list (slice-at cols columns-per-page))])
           (define-values (last-x cols)
             (for/fold ([current-x (/ (- (world:paper-width-default) width-of-printed-area) 2)][cols empty]) ([col (in-list page-cols)][idx (in-naturals)])
               (values (+ current-x column-width column-gutter) (cons (quad-attr-set* col 'x current-x 'y 40 world:column-index-key idx) cols))))
           (reverse cols))))
  result-pages)


(define/contract (typeset x)
  (coerce/input? . -> . doc?)  
  (cond
    [(input? x) (load-text-cache-file)
                (define multipages (input->multipages x)) ; 125 = timings for jude0
                (define pages (append-map typeset multipages)) ; 1446
                (define doc (typeset pages)) ; 250
                (update-text-cache-file)
                doc]
    [(multipage? x) (define multicolumns (multipage->multicolumns x)) ; 81
                    (define columns (append-map typeset multicolumns)) ; 1460
                    (define pages (typeset columns)) ; 0
                    pages]
    [(multicolumn? x) (define blocks (multicolumn->blocks x)) ; 69
                      (define lines (append-map typeset blocks)) ; 1363
                      (define columns (typeset lines)) ; 4
                      columns]
    [(lines? x) (map typeset (lines->columns x))] ; 10
    [(pages? x) (typeset (pages->doc x))] ; 249
    [(columns? x) (map typeset (columns->pages x))] ; 1
    [(block? x) (map typeset (block->lines x))] ; about 2/3 of running time
    [else x]))

(define (para ht . xs)
  (apply box ht `(,(block-break) ,@xs ,(block-break)))) 


(module+ main
  (require "render.rkt" racket/class profile)
  (require "samples.rkt")
  (activate-logger quad-logger)
  (parameterize ([world:quality-default world:adaptive-quality]
                 [world:paper-width-default 412]
                 [world:paper-height-default 600])
    (define to (begin (time (typeset (jude0)))))
    (time (send (new pdf-renderer%) render-to-file to "foo.pdf"))))
