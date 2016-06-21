#lang quad/dev
(provide (all-defined-out))
(require "measure.rkt")

(define last-breakpoint-k (λ _ (error 'typeset-fit "No breakpoint available. Increase line width")))
(define (set-breakpoint-k-here!)
  (let/cc k (set! last-breakpoint-k k) #f))

(define char-width 6)
(define line-width (* 60 char-width)) ; 50 chars, each 6 pts wide
(define line-height 12)
(define col-height (* 6 line-height)) ; 3 rows, each 12 pts high
(define page-width (* 3 line-width)) ; meaning, two columns

;; posn-page : horiz position of column within page
;; posn-col : vert position of line within column
;; posn-line : horiz position of char within line 
(struct posn (page col line))
(define (make-posn [page 0] [col 0] [line 0]) (posn page col line))

(define page-start-position (make-posn))

(define (typeset-fit qs [line-width line-width] [col-height col-height])
  
  (define (handle-break val [current-posn #f])
    (caseq val ; test in order of frequency
           [(line-break) (make-posn (posn-page current-posn) (+ (posn-col current-posn) line-height))]
           [(column-break) (make-posn (+ (posn-page current-posn) line-width))]
           [(page-break) page-start-position]
           [else current-posn]))
  
  (for/fold ([current-posn (handle-break 'page-break)])
            ([q (in-vector qs)])
    (unless (quad-dim q) (measure! q))
    (cond
      ;; hard may contain an imperative break. Test for this first because it makes the rest irrelevant.
      ;; todo: how to suppress spaces adjacent to imperative breaks?
      [($hard? q) (handle-break (quad-dim q) current-posn)]
      
      ;; test for overset (before a new bp-k gets set).
      ;; order is precedence: test bigger breaks first
      ;; send break type back through continuation
      ;; test page-horiz with >= because one column impliedly exists at the start
      ;; (we could also make this explicit with page-start-position but it seems clearer to use zeroes there)
      [(>= (posn-page current-posn) page-width) (last-breakpoint-k 'page-break)]
      
      ;; test tp-vert with >= because one column impliedly exists at the start
      [(>= (posn-col current-posn) col-height) (last-breakpoint-k 'column-break)]
      
      ;; but test tp-horiz with > because no characters exist in the line at the start
      [(> (posn-line current-posn) line-width) (last-breakpoint-k 'line-break)]
      
      ;; set a new bp-k, or resume after invoking a bp-k
      [(and ($soft? q) (set-breakpoint-k-here!))
       => ; grabs the end value of the conditional, which is the arg passed when breakpoint-k was invoked
       (λ (breakpoint-k-result)
         ;; convert the white, thereby consuming it. todo: don't consume hyphens
         (quad-dim-set! q breakpoint-k-result) 
         (handle-break breakpoint-k-result current-posn))]
      [else (posn (posn-page current-posn) (posn-col current-posn) (+ (posn-line current-posn) (quad-dim q)))]))
  qs)

(module+ test
  (require "atomize.rkt" "render.rkt")
  (define q (quad #f "One morning, when Gregor " (line-break) "  and his old hizn himself"))
  (time (debug-render (typeset-fit (atomize q)))))