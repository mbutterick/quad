#lang racket
(require math/statistics sugar racket/serialize plot)
(require (except-in "quads.rkt" line) "utils.rkt" "wrap.rkt" "world.rkt" "measure.rkt" "logger.rkt" "main.rkt")

(define+provide (make-wrap-proc-bps
                 #:make-pieces-proc make-pieces-proc 
                 #:measure-quad-proc measure-quad-proc
                 #:compose-line-proc compose-line-proc
                 #:fast-measure-pieces-proc [fast-measure-pieces-proc (compose1 measure-quad-proc compose-line-proc)]
                 #:find-breakpoints-proc find-breakpoints-proc)
  (λ(qs [measure #f])
    (let* ([measure (or measure (quad-attr-ref/parameter (car qs) world:measure-key))]
           [qs (if (quad-has-attr? (car qs) world:measure-key)
                   qs
                   (map (curryr quad-attr-set world:measure-key measure) qs))])
      (log-quad-debug "wrapping on measure = ~a" measure)
      (define pieces (make-pieces-proc qs))
      
      (log-quad-debug "avg piece length for breakpoints = ~a" 
                      (/ (for/sum ([p (in-list pieces)])
                           (for/sum ([q (in-list (quad-list p))])
                             (define str (quad->string q))
                             (if (equal? str "")
                                 (quad-attr-ref q world:width-key 0)
                                 (apply measure-text (quad->string q) (font-attributes-with-defaults q)))))
                         (length pieces)))
      
      (define bps (find-breakpoints-proc (list->vector pieces) (+ 0.0 measure)))
      (values pieces bps (map (curryr compose-line-proc measure-quad-proc) (break-at pieces bps))))))

;; wrap proc based on greedy proc
(define wrap-first-bps (make-wrap-proc-bps 
                        #:make-pieces-proc (make-caching-proc make-pieces)
                        #:measure-quad-proc quad-width 
                        #:compose-line-proc pieces->line
                        #:fast-measure-pieces-proc measure-potential-line
                        #:find-breakpoints-proc wrap-first))

;; wrap proc based on penalty function
(define wrap-best-bps (make-wrap-proc-bps 
                       #:make-pieces-proc (make-caching-proc make-pieces)
                       #:measure-quad-proc quad-width 
                       #:compose-line-proc pieces->line
                       #:fast-measure-pieces-proc measure-potential-line
                       #:find-breakpoints-proc wrap-best))

(define ti (block '(measure 54 leading 18) "Meg is an ally."))


(define (looseness-spread lines)
  (if (<= (length lines) 1)
      0
      (let ([lines-to-measure (drop-right lines 1)]) ; exclude last line from looseness calculation
        (define measures (map (λ(line) (quad-attr-ref line world:line-looseness-key 0)) lines-to-measure))
        (round-float (- (apply max measures) (apply min measures))))))

(define (geometric-mean lines)
  (if (<= (length lines) 1)
      0
      (let ([lines-to-measure (drop-right lines 1)]) ; exclude last line from looseness calculation
        (define measures (map (λ(line) (quad-attr-ref line world:line-looseness-key 0)) lines-to-measure))
        (expt (apply * measures) (/ 1 (length measures))))))

(define (looseness-stddev lines)
  (if (<= (length lines) 1)
      0
      (let ([lines-to-measure (drop-right lines 1)]) ; exclude last line from looseness calculation
        (define measures (map (λ(line) (quad-attr-ref line world:line-looseness-key 0)) lines-to-measure))
        (stddev measures))))

(define (looseness-var lines)
  (if (<= (length lines) 1)
      0
      (let ([lines-to-measure (drop-right lines 1)]) ; exclude last line from looseness calculation
        (define measures (map (λ(line) (quad-attr-ref line world:line-looseness-key 0)) lines-to-measure))
        (variance measures))))



(define (list->hash0 . xs)
  (define mh (make-hash))
  (for ([(k v) (in-hash (apply hash xs))])
    (hash-set! mh k v))
  mh)

(define (piece-length p)
  (apply + (map quad-width (quad-list p))))

(define (trial wrap-proc ti measure)
  (match-define-values (result time _ _) (time-apply wrap-proc (append (list ti) (list measure))))
  (match-define (list pieces bps lines) result)
  (define line-count (length lines))
  (define piece-count (length pieces))
  (define avg-piece-length (/ (apply + (map piece-length pieces)) (length pieces)))
  (define avg-looseness (average-looseness lines))
  (define looseness-sd (looseness-stddev lines))
  (define looseness-variance (looseness-var lines))
  (define spread (looseness-spread lines))
  (define looses (map (λ(line) (quad-attr-ref line world:line-looseness-key 0)) lines))
  (hash 'bps bps 'time time 'line-count line-count 'looses looses 
        'piece-count piece-count 'avg-piece-length avg-piece-length
        'avg-looseness avg-looseness 'looseness-spread spread
        'looseness-sd looseness-sd
        'looseness-variance looseness-variance))


(define (improved? h1 h2)
  (define h1-bps (if (hash? h1) (hash-ref h1 'bps) h1))
  (define h2-bps (if (hash? h2) (hash-ref h2 'bps) h2))
  (define min-length (min (length h1-bps) (length h2-bps)))
  (apply + (map abs (map - (take h1-bps min-length) (take h2-bps min-length)))))


(define (trial-set measure ti)
  (define text (quad->string ti))
  
  (define ti-unhyphenated (split-quad ti))
  (define fu (trial wrap-first-bps ti-unhyphenated measure))
  (define bu (trial wrap-best-bps ti-unhyphenated measure))
  (define ti-hyphenated (split-quad (hyphenate-quad ti)))
  (define fh (trial wrap-first-bps ti-hyphenated measure))
  (define bh (trial wrap-best-bps ti-hyphenated measure))
  
  (hash 'fu fu 'bu bu 'fh fh 'bh bh
        'text text
        'measure measure
        'fh-improved (improved? fu fh)
        'bu-improved (improved? fu bu)
        'bh-improved (improved? fh bh)))

(define (make-blocks textfile)
  (define strings (filter (λ(s) (> (string-length s) 10)) (map (λ(s) (regexp-replace* #rx"\n" s " ")) (map string-trim (string-split (file->string textfile) "\n\n")))))
  (map (λ(t) (block '(font "Equity Text B" leading 14 column-count 1 column-gutter 10 size 11.5 x-align justify x-align-last-line left) t)) strings))

(define (refresh [filename "jude0.txt"] [measures '(150 180 210 240 270 300 330 360 390)])
  (define paragraphs (make-blocks filename))
  ;; only use paragraphs > 2 lines because last line is disregarded for looseness spread calculation
  (define results (filter
                   (λ(rec) (> (hash-ref (hash-ref rec 'fu) 'line-count) 2)) (append-map (compose1 (λ(m) (map (λ(p) (trial-set m p)) paragraphs)) (λ(m) (report m))) measures)))
  (write-to-file (serialize results) "stats-data.txt" #:exists 'replace))

(define (load-data-records)
  (deserialize (file->value "stats-data.txt")))

(define recs (load-data-records))

(define (plot-lists xs ys zs kx ky kz)
  (parameterize ([plot-width    700]
                 [plot-height   700]
                 [plot-x-label  kx]
                 [plot-y-label  ky]
                 [plot-z-label  kz])
    (plot3d  
     ;#:x-max 2 #:x-min -2
     #:y-min 140 #:y-max 400
     #:z-max 1.5
     #:angle 0
     #:altitude 32
     (points3d (map vector xs ys zs)
               #:sym 'fullcircle7
               #:alpha 0.1
               #:color 42))))

(define (fu-formula rec)
  (define pieces-per-line (/ (hash-ref (hash-ref rec 'fu) 'piece-count)
                             (hash-ref (hash-ref rec 'fu) 'line-count) 1.0))
  (+ 2.2 (log (abs (hash-ref (hash-ref rec 'fu) 'looseness-sd)))
     (* 0.09 pieces-per-line)))

(define (geo-mean rec)
  (define looses (hash-ref (hash-ref rec 'fu) 'looses))
  (expt (abs (apply * looses)) (/ 1 (length looses))))

(define (magic2 rec)
  (define looses (map abs (hash-ref (hash-ref rec 'fu) 'looses)))
  (expt (log (+ 1 (/ (variance looses) (expt (mean looses) 2)))) 0.5))


(define (plot-it)
  (define-values (helped or-not) (partition (λ(rec) (< 0 (hash-ref rec 'bu-improved))) recs))
  (define-values (fu-pos fu-neg) (partition (λ(rec) (> (fu-formula rec) 0)) recs))
  (report* (length helped) (length or-not) (length fu-pos) (length fu-neg))
  (let ([recs recs])
    (plot-lists
   (map fu-formula recs)
   (map (λ(rec) (hash-ref rec 'measure)) recs)
   (map (λ(rec) (if (= 0 (hash-ref rec 'bu-improved)) 0 1))  recs) "fu-formula" "measure" "improve?")))

(plot-it)

(define (looseness-improvement rec)
  (/ (abs (- (hash-ref (hash-ref rec 'bu) 'avg-looseness) (hash-ref (hash-ref rec 'fu) 'avg-looseness)))
     (hash-ref (hash-ref rec 'bu) 'avg-looseness)))
(define zs (filter positive? (sort (map looseness-improvement recs) <)))