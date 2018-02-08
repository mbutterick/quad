#lang quad/dev
(provide (all-defined-out))
(require "measure.rkt")

;; track this k outside of for/fold loop to keep it independent.
;; otherwise, every time k is invoked, the loop k will also change.
;; (mutated data is not reset by a continuation, but loop vars are)
(define last-breakpoint-k raise-overflow-error)

(define (set-breakpoint-k-here!)
  (let/cc k (set! last-breakpoint-k k) #f))

(define (already-breakpoint-type? q type)
  (eq? (quad-dim q) type))

(define char-width 6)
(define line-width (* 60 char-width)) ; 50 chars, each 6 pts wide
(define line-height 12)
(define col-height (* 6 line-height)) ; 3 rows, each 12 pts high
(define page-width (* 3 line-width)) ; meaning, two columns

;; posn-page : horiz position of column within page
;; posn-col : vert position of line within column
;; posn-line : horiz position of char within line 
(struct posn (page col line) #:transparent)
(define (make-posn [page 0] [col 0] [line 0]) (posn page col line))

(define page-start-position (make-posn))

(define (fit qs [line-width line-width] [col-height col-height])
  
  (define (handle-break val [current-posn #f])
    (caseq val ; test in order of frequency
           [(line-break) (make-posn (posn-page current-posn) (+ (posn-col current-posn) line-height))]
           [(column-break) (make-posn (+ (posn-page current-posn) line-width))]
           [(page-break) page-start-position]
           [else current-posn]))
  
  (for/fold ([current-posn page-start-position]) 
            ([q (in-vector qs)])
    (unless (quad-dim q) (measure! q))
    (cond
      ;; shim may contain an imperative break. 
      [($shim? q) (handle-break (quad-dim q) current-posn)]
      
      ;; test for overset (before a new bp-k gets set).
      ;; send break type back through continuation
      ;; we do a combined test to find out the "biggest" break that is needed
      ;; order connotes precedence
      [(or
        ;; test page-horiz with >= because one column impliedly exists at the start
        (and (>= (posn-page current-posn) page-width) 'page-break)
        ;; test tp-vert with >= because one column impliedly exists at the start
        (and (>= (posn-col current-posn) col-height) 'column-break)
        ;; test tp-horiz with > because no characters exist in the line at the start
        (and (> (posn-line current-posn) line-width) 'line-break)) => last-breakpoint-k]
      
      ;; set a new bp-k, or resume after invoking a bp-k
      ;; bp-k has to be in conditional so it triggers side effect but also forces next branch
      [(and ($space? q) (set-breakpoint-k-here!))
       => ; grabs the value of the condition: the arg passed to breakpoint-k
       (λ (breakpoint-k-result)
         (when (already-breakpoint-type? q breakpoint-k-result)
           ;; it means we're caught in an overflow loop, so
           (raise-overflow-error))
         ;; convert the white, thereby consuming it. todo: don't consume hyphens
         (quad-dim-set! q breakpoint-k-result)
         (handle-break breakpoint-k-result current-posn))]
      
      [else (posn (posn-page current-posn) (posn-col current-posn) (+ (posn-line current-posn) (quad-dim q)))]))
  qs)

(module+ test
  (require "atomize.rkt" "render.rkt")
  ;; todo: preserve space between black quads
  (define q (quad #f "One morning " (quad #f "and himself")))
  (time (debug-render (fit (atomize q)))))