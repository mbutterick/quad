#lang debug racket
(require "struct.rkt"
         "font.rkt"
         "attrs.rkt"
         "param.rkt"
         "debug.rkt"
         quad/quad
         quad/atomize
         pitfall
         quad/position
         racket/unsafe/ops)
(provide (all-defined-out))

(define (convert-string-quad q)
  ;; need to handle casing here so that it's reflected in subsequent sizing ops
  (define cased-str (match (quad-elems q)
                      [(cons str _)
                       (define proc (match (quad-ref q :font-case)
                                      [(or "upper" "uppercase") string-upcase]
                                      [(or "lower" "lowercase" "down" "downcase") string-downcase]
                                      [(or "title" "titlecase") string-titlecase]
                                      [_ values]))
                       (proc str)]
                      [_ ""])) ; a string quad should always contain a string
  (quad-copy string-quad q:string
             [attrs (let ([attrs (quad-attrs q)])
                      (hash-ref! attrs :font-size default-font-size)
                      attrs)]
             [elems (list cased-str)]
             [size (make-size-promise-for-string q cased-str)]))


(define soft-hyphen-string "\u00AD")

(define (make-size-promise-for-string q [str-arg #f])
  ;; we know sensible defaults for all text properties have been set up during atomization.
  (delay
    (define q-string-width
      (let ([str (cond
                   [str-arg]
                   [else (match (quad-elems q)
                           [(cons q _) q]
                           [_ #false])])])
        (cond
          [(positive? (string-length str))
           (define pdf (current-pdf))
           (font-size pdf (quad-ref q :font-size))
           (font pdf (path->string (quad-ref q font-path-key)))
           (define tracking-val (quad-ref q :font-tracking 0))
           (cond
             [(equal? str soft-hyphen-string) tracking-val]
             [else ;; `string-width` only applies tracking between glyphs.
              ;; we add an extra tracking-val because we want to count tracking on every glyph.
              ;; because at this stage, we don't know whether the quad will be freestanding or adjacent to another
              ;; probably adjacent. And if so, it should have half tracking on the ends, full tracking in between
              (+ (string-width pdf str
                               #:tracking tracking-val
                               #:features (quad-ref q :font-features))
                 tracking-val)])]
          [else 0])))
    (list q-string-width (quad-ref q :line-height))))


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
     (define tracking (quad-ref q :font-tracking 0))
     ;; we adjust x by half tracking because by convention, string quads have half tracking at beginning & end
     ;; whereas PDF drawing only puts tracking between the glyphs.
     (text doc str (+ x (/ tracking 2.0)) (- y (quad-ref q :font-baseline-shift 0))
           #:tracking tracking
           #:underline (quad-ref q :font-underline)
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
                    #:tag 'str
                    #:printable q:string-printable?
                    #:draw q:string-draw
                    #:draw-end q:string-draw-end))

(define (consolidate-runs pcs)
  (let loop ([runs empty][pcs pcs])
    (match pcs
      [(cons (? string-quad? strq) rest)
       (define-values (run-pcs rest) (splitf-at pcs (λ (p) (same-run? strq p))))
       ;; run-pcs has at least one element (strq)
       ;; and the other members are part of the same run.
       ;; meaning, they share the same formatting, including character tracking.
       
       ;; we add a tracking adjustment because it only "appears"
       ;; once characters are consolidated
       (define tracking-adjustment
         (* (sub1 (length run-pcs)) (quad-ref (car run-pcs) :font-tracking 0)))
       (define new-run
         (quad-copy string-quad q:string
                    [attrs (quad-attrs strq)]
                    [elems (merge-adjacent-strings (apply append (map quad-elems run-pcs)))]
                    [size (delay (pt (sum-x run-pcs) (pt-y (size strq))))]))
       (loop (cons new-run runs) rest)]
      [(cons first rest) (loop (cons first runs) rest)]
      [_ (reverse runs)])))