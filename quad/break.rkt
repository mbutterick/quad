#lang debug racket/base
(require racket/contract racket/list txexpr sugar/debug sugar/list racket/promise
         "param.rkt" "qexpr.rkt" "atomize.rkt" "quad.rkt")

(define debug #f)
(define/contract (breaks xs-in
                         [target-size (current-line-width)]
                         #:break-val [break-val 'break]
                         ;; todo: generalize these procs so they're not particular to quads
                         #:mandatory-break-proc [mandatory-break? (λ (q) (memv (car (qe q)) '(#\newline)))]
                         #:optional-break-proc [optional-break? (λ (q) (memv (car (qe q)) '(#\space)))]
                         #:size-proc [size-proc (λ (q) (let ([val (hash-ref (qa q) 'size (λ ()
                                                                                           (if (memv (car (qe q)) '(#\space))
                                                                                               (delay (values 0 1 0))
                                                                                               (delay (values 1 1 1)))))])
                                                         (if (promise? val) (force val) (val))))])
  ((any/c) (integer? #:break-val any/c
                      #:mandatory-break-proc procedure?
                      #:optional-break-proc procedure?
                      #:size-proc procedure?) . ->* . (listof any/c))
  (define last-breakpoint-k #f)
  (define (capture-k!) (let/cc k (set! last-breakpoint-k k) #f))
  (for/fold ([xss null]
             [size-so-far 0]
             #:result (reverse xss))
            ([x (in-list xs-in)])
    (define-values (size-start size-mid size-end) (size-proc x))
    (cond
      [(<= (+ size-so-far size-end) target-size) ;; check overflow condition based on size-end (as if x were breakpoint)
       (cond
         [(mandatory-break? x) (when debug (report x 'got-mandatory-break))
                               (values (cons break-val xss) 0)]
         [(and (optional-break? x) (capture-k!)) (when debug (report x 'resuming-breakpoint)) ;; return point for k
          (set! last-breakpoint-k #f) ;; prevents continuation loop
          (values (cons break-val xss) 0)] ;; when break is found, q is omitted from accumulation
         [else (when debug (report x 'add-to-line))
               (values (cons x xss) (if (zero? size-so-far) ;; we're still at start
                                               size-start
                                               (+ size-so-far size-mid)))])] ;; otherwise recur based on size-mid
      ;; overflow handlers
      [last-breakpoint-k (when debug (report x 'invoking-last-breakpoint))
                         (last-breakpoint-k #t)]
      [else (when debug (report x 'falling-back))
            (values (list* x break-val xss) size-start)]))) ;; fallback if no last-breakpoint-k exists


(module+ test
  (require rackunit)
  (define ch (q (hasheq 'size (delay (values 1 1 1))) #\x))
  (define a (q (hasheq 'size (delay (values 1 1 1))) #\a))
  (define b (q (hasheq 'size (delay (values 1 1 1))) #\b))
  (define c (q (hasheq 'size (delay (values 1 1 1))) #\c))
  (define d (q (hasheq 'size (delay (values 1 1 1))) #\d))
  (define sp (q (hasheq 'size (delay (values 0 1 0))) #\space))
  (define br (q (hasheq 'size (delay (values 0 0 0))) #\newline))

  (define (visual-breaks str int)
    (apply string (for/list ([b (in-list (breaks (atomize str) int))])
                    (cond
                      [(quad? b) (car (qe b))]
                      [else #\|]))))
  
  (check-equal? (breaks (list) 1) null)  
  (check-equal? (breaks (list ch) 1) (list ch))
  (check-equal? (breaks (list ch ch) 1) (list ch 'break ch))
  (check-equal? (breaks (list ch ch ch) 1) (list ch 'break ch 'break ch))
  (check-equal? (breaks (list ch ch ch) 2) (list ch ch 'break ch))
  (check-equal? (breaks (list ch ch ch ch) 2) (list ch ch 'break ch ch))
  (check-equal? (breaks (list ch ch ch ch ch) 3) (list ch ch ch 'break ch ch))
  (check-equal? (breaks (list ch ch ch ch ch) 1) (list ch 'break ch 'break ch 'break ch 'break ch))
  (check-equal? (breaks (list ch ch ch ch ch) 10) (list ch ch ch ch ch))
  
  (check-equal? (breaks (list ch sp ch) 1) (list ch 'break ch))
  (check-equal? (breaks (list ch ch sp ch) 2) (list ch ch 'break ch))
  (check-equal? (breaks (list a sp b) 3) (list a sp b))
  (check-equal? (breaks (list ch sp ch ch) 3) (list ch 'break ch ch))
  
  (check-equal? (breaks (list a br b) 2) (list a 'break b))
  (check-equal? (breaks (list ch br ch ch) 3) (list ch 'break ch ch))
  (check-equal? (breaks (list ch ch br ch) 3) (list ch ch 'break ch))
  (check-equal? (breaks (list ch ch ch ch) 3) (list ch ch ch 'break ch))
  (check-equal? (breaks (list ch ch ch sp ch ch) 2) (list ch ch 'break ch 'break ch ch))
  (check-equal? (breaks (list ch ch ch sp ch ch) 3) (list ch ch ch 'break ch ch))

  (check-equal? (visual-breaks "My dog has fleas" 1) "M|y|d|o|g|h|a|s|f|l|e|a|s")
  (check-equal? (visual-breaks "My dog has fleas" 2) "My|do|g|ha|s|fl|ea|s")
  (check-equal? (visual-breaks "My dog has fleas" 3) "My|dog|has|fle|as")
  (check-equal? (visual-breaks "My dog has fleas" 4) "My|dog|has|flea|s")
  (check-equal? (visual-breaks "My dog has fleas" 5) "My|dog|has|fleas")
  (check-equal? (visual-breaks "My dog has fleas" 6) "My dog|has|fleas")
  (check-equal? (visual-breaks "My dog has fleas" 7) "My dog|has|fleas")
  (check-equal? (visual-breaks "My dog has fleas" 8) "My dog|has|fleas")
  (check-equal? (visual-breaks "My dog has fleas" 9) "My dog|has fleas")
  (check-equal? (visual-breaks "My dog has fleas" 10) "My dog has|fleas")
  (check-equal? (visual-breaks "My dog has fleas" 11) "My dog has|fleas")
  (check-equal? (visual-breaks "My dog has fleas" 12) "My dog has|fleas")
  (check-equal? (visual-breaks "My dog has fleas" 13) "My dog has|fleas")
  (check-equal? (visual-breaks "My dog has fleas" 14) "My dog has|fleas")
  (check-equal? (visual-breaks "My dog has fleas" 15) "My dog has|fleas")
  (check-equal? (visual-breaks "My dog has fleas" 16) "My dog has fleas"))