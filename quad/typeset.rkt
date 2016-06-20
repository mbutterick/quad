#lang quad/dev
(provide (all-defined-out))
(require "measure.rkt")

(define last-breakpoint-k #f)
(define (set-breakpoint-k-here!)
  (let/cc k (set! last-breakpoint-k k) #f))

(define char-width 6)
(define line-width (* 60 char-width)) ; 50 chars, each 6 pts wide
(define line-height 12)
(define col-height (* 6 line-height)) ; 3 rows, each 12 pts high
(define page-width (* 3 line-width)) ; meaning, two columns

(struct tp (page-horiz vert horiz))

(define page-start-position (tp 0 0 0))

(define (handle-break val [tpos #f])
  (caseq val ; test in order of frequency
    [(line-break) (tp (tp-page-horiz tpos) (+ (tp-vert tpos) line-height) 0)]
    [(column-break) (tp (+ (tp-page-horiz tpos) line-width) 0 0)]
    [(page-break) page-start-position]
    [else tpos]))

(define (typeset-fit qs)
  (for/fold ([tpos (handle-break 'page-break)])
            ([q (in-vector qs)])
    (unless (quad-dim q) (measure! q))
    (cond
      ;; hard may contain an imperative break. Test for this first because it makes the rest irrelevant.
      ;; todo: how to suppress spaces adjacent to imperative breaks?
      [($hard? q) (handle-break (quad-dim q) tpos)]
      
      ;; test for overset (before a new bp-k gets set).
      ;; order is precedence: test bigger breaks first
      ;; test page-horiz with >= because one column impliedly exists at the start
      ;; (we could also make this explicit with page-start-position but it seems clearer to use zeroes there)
      [(>= (tp-page-horiz tpos) page-width) (last-breakpoint-k 'page-break)]
      
      ;; test tp-vert with >= because one column impliedly exists at the start
      [(>= (tp-vert tpos) col-height) (last-breakpoint-k 'column-break)]
      
      ;; but test tp-horiz with > because no characters exist in the line at the start
      [(> (tp-horiz tpos) line-width) (last-breakpoint-k 'line-break)]
      
      ;; set a new bp-k, or resume after invoking a bp-k
      [(and ($soft? q) (set-breakpoint-k-here!))
       => ; grabs the end value of the conditional, which is the arg passed when breakpoint-k was invoked
       (λ (breakpoint-k-result)
         ;; convert the white, thereby consuming it. todo: don't consume hyphens
         (quad-dim-set! q breakpoint-k-result) 
         (handle-break breakpoint-k-result tpos))]
      [else (tp (tp-page-horiz tpos) (tp-vert tpos) (+ (tp-horiz tpos) (quad-dim q)))]))
  qs)

(module+ test
  (require "atomize.rkt" "render.rkt")
  (define q (quad #f "One morning, when Gregor" (line-break) " and his old hizn himself"))
  (time (debug-render (typeset-fit (atomize q)))))