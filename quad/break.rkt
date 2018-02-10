#lang debug racket/base
(require racket/contract racket/list txexpr sugar/debug sugar/list racket/promise racket/function
         "param.rkt" "qexpr.rkt" "atomize.rkt" "quad.rkt")

(define/contract (insert-breaks xs
                                [target-size (current-line-width)]
                                [debug #f]
                                #:break-val [break-val 'break]
                                ;; todo: generalize these procs so they're not particular to quads
                                #:mandatory-break-proc [mandatory-break? (const #f)]
                                #:optional-break-proc [optional-break? (const #f)]
                                #:size-proc [size-proc (const 1)])
  ((any/c) (integer? any/c
                     #:break-val any/c
                     #:mandatory-break-proc procedure?
                     #:optional-break-proc procedure?
                     #:size-proc procedure?) . ->* . (listof any/c))
  (define start-signal (gensym))
  (define last-breakpoint-k #f)
  (define (capture-k!) (let/cc k (set! last-breakpoint-k k) #f))
  (for/fold ([vals null]
             [size-so-far start-signal]
             #:result (reverse (dropf vals optional-break?)))
            ([x (in-list xs)])
    (define-values (size-start size-mid size-end) (size-proc x))
    (define at-start? (eq? size-so-far start-signal))
    (define underflow? (and (not at-start?) (<= (+ size-so-far size-end) target-size)))
    (define (add-to-segment) (values (cons x vals) (if at-start?
                                                       size-start
                                                       (+ size-so-far size-mid))))
    (define (insert-break)
      ;; when break is found, q is omitted from accumulation
      ;; and any preceding optional breaks are dropped (that would be trailing before the break)
      (values (cons break-val (dropf vals optional-break?)) start-signal))
    (cond
      [(mandatory-break? x) (when debug (report x 'got-mandatory-break))
                            (insert-break)]
      [(optional-break? x)
       (cond
         [at-start? (when debug (report x 'skipping-opt-break-at-beginning)) (values vals size-so-far)]
         [(and underflow? (capture-k!)) (when debug (report x 'resuming-breakpoint))
                                        (set! last-breakpoint-k #f) ;; prevents continuation loop
                                        (insert-break)]
         [else (when debug (report x 'add-optional-break))
               (add-to-segment)])]
      [(or at-start? underflow?) (when debug (report x 'add-ordinary-char))
                                 (add-to-segment)]
      [last-breakpoint-k (when debug (report x 'invoking-last-breakpoint))
                         (last-breakpoint-k #t)]
      [else (when debug (report x 'falling-back))
            (values (list* x break-val vals) size-start)]))) ;; fallback if no last-breakpoint-k exists


(define x (q (hasheq 'size (delay (values 1 1 1))) #\x))
(define zwx (q (hasheq 'size (delay (values 0 0 0))) #\z))
(define a (q (hasheq 'size (delay (values 1 1 1))) #\a))
(define b (q (hasheq 'size (delay (values 1 1 1))) #\b))
(define c (q (hasheq 'size (delay (values 1 1 1))) #\c))
(define d (q (hasheq 'size (delay (values 1 1 1))) #\d))
(define sp (q (hasheq 'size (delay (values 0 1 0))) #\space))
(define br (q (hasheq 'size (delay (values 0 0 0))) #\newline))


(module+ test
  (require rackunit)

  (define (lbs xs size [debug #f])
    (insert-breaks xs size debug
                   #:break-val 'lb
                   #:mandatory-break-proc (λ (q) (and (quad? q) (memv (car (qe q)) '(#\newline))))
                   #:optional-break-proc (λ (q) (and (quad? q) (memv (car (qe q)) '(#\space))))
                   #:size-proc (λ (q) (let ([val (hash-ref (qa q) 'size (λ ()
                                                                          (if (memv (car (qe q)) '(#\space))
                                                                              (delay (values 0 1 0))
                                                                              (delay (values 1 1 1)))))])
                                        (if (promise? val) (force val) (val))))))

  (test-case
   "chars"
   (check-equal? (lbs (list) 1) null)  
   (check-equal? (lbs (list x) 1) (list x))
   (check-equal? (lbs (list x x) 1) (list x 'lb x))
   (check-equal? (lbs (list x x x) 1) (list x 'lb x 'lb x))
   (check-equal? (lbs (list x x x) 2) (list x x 'lb x))
   (check-equal? (lbs (list x x x x) 2) (list x x 'lb x x))
   (check-equal? (lbs (list x x x x x) 3) (list x x x 'lb x x))
   (check-equal? (lbs (list x x x x x) 1) (list x 'lb x 'lb x 'lb x 'lb x))
   (check-equal? (lbs (list x x x x x) 10) (list x x x x x)))

  (test-case
   "chars and spaces"
   (check-equal? (lbs (list x sp x) 1) (list x 'lb x))
   (check-equal? (lbs (list x x sp x) 2) (list x x 'lb x))
   (check-equal? (lbs (list a sp b) 3) (list a sp b))
   (check-equal? (lbs (list x sp x x) 3) (list x 'lb x x)))

  (test-case
   "leading & trailing spaces"
   (check-equal? (lbs (list sp x) 2) (list x))
   (check-equal? (lbs (list x sp) 2) (list x))
   (check-equal? (lbs (list sp x sp) 2) (list x))
   (check-equal? (lbs (list sp sp x sp sp) 2) (list x))
   (check-equal? (lbs (list sp sp x sp sp x sp) 1) (list x 'lb x)))
  
  (test-case
   "zero width nonbreakers"
   (check-equal? (lbs (list sp zwx) 2) (list zwx))
   (check-equal? (lbs (list zwx sp) 2) (list zwx))
   (check-equal? (lbs (list sp zwx sp) 2) (list zwx))
   (check-equal? (lbs (list sp sp zwx sp sp) 2) (list zwx))
   (check-equal? (lbs (list sp sp zwx sp sp zwx sp) 2) (list zwx sp sp zwx)))

  (test-case
   "mandatory breaks"
   (check-equal? (lbs (list br) 2) (list 'lb))
   (check-equal? (lbs (list a br b) 2) (list a 'lb b))
   (check-equal? (lbs (list a b br) 2) (list a b 'lb))
   (check-equal? (lbs (list a b br br) 2) (list a b 'lb 'lb))
   (check-equal? (lbs (list x br x x) 3) (list x 'lb x x))
   (check-equal? (lbs (list x x br x) 3) (list x x 'lb x))
   (check-equal? (lbs (list x x x x) 3) (list x x x 'lb x))
   (check-equal? (lbs (list x x x sp x x) 2) (list x x 'lb x 'lb x x))
   (check-equal? (lbs (list x x x sp x x) 3) (list x x x 'lb x x)))

  (test-case
   "mandatory breaks and spurious spaces"
   (check-equal? (lbs (list a sp sp sp br b) 2) (list a 'lb b))
   (check-equal? (lbs (list x sp br sp sp x x sp) 3) (list x 'lb x x))
   (check-equal? (lbs (list sp sp x x sp sp br sp sp sp x) 3) (list x x 'lb x))
   (check-equal? (lbs (list a sp b sp sp br sp c) 3) (list a sp b 'lb c))
   (check-equal? (lbs (list x x x x) 3) (list x x x 'lb x))
   (check-equal? (lbs (list x x x sp x x) 2) (list x x 'lb x 'lb x x))
   (check-equal? (lbs (list x x x sp x x) 3) (list x x x 'lb x x)))

  (define (visual-breaks str int)
    (apply string (for/list ([b (in-list (lbs (atomize str) int))])
                    (cond
                      [(quad? b) (car (qe b))]
                      [else #\|]))))

  (test-case
   "visual breaks"
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

  
  (define (pbs xs size [debug #f])
    (insert-breaks xs size debug
                   #:break-val 'pb
                   #:mandatory-break-proc (λ (x) (and (quad? x) (memv (car (qe x)) '(#\page))))
                   #:optional-break-proc (λ (x) (eq? x 'lb))
                   #:size-proc (λ (q) (case q
                                        [(lb) (values 0 0 0)]
                                        [else (values 1 1 1)]))))
  (define pbr (q (hasheq 'size (delay (values 0 0 0))) #\page))

  (test-case
   "soft page breaks"
   (check-equal? (pbs null 2) null)
   (check-equal? (pbs (list x) 2) (list x))
   (check-equal? (pbs (list x x) 2) (list x x))
   (check-equal? (pbs (list x x x) 1) (list x 'pb x 'pb x))
   (check-equal? (pbs (list x x x) 2) (list x x 'pb x))
   (check-equal? (pbs (list x x x) 3) (list x x x))
   (check-equal? (pbs (list x x x) 4) (list x x x))
   (check-equal? (pbs (list x 'lb x x) 2) (list x 'pb x x)))

  (test-case
   "hard page breaks"
   (check-equal? (pbs (list x pbr x x) 2) (list x 'pb x x))
   (check-equal? (pbs (list x pbr x x) 1) (list x 'pb x 'pb x))
   (check-equal? (pbs (list x pbr pbr x x) 1) (list x 'pb 'pb x 'pb x))
   (check-equal? (pbs (list x pbr pbr x x) 2) (list x 'pb 'pb x x))
   (check-equal? (pbs (list 'lb x 'lb 'lb pbr 'lb x x 'lb) 2) (list x 'pb x x)))

  (test-case
   "composed line breaks and page breaks"
   (check-equal? (pbs (lbs null 1) 2) null)
   (check-equal? (pbs (lbs (list x) 1) 2) (list x))
   (check-equal? (pbs (lbs (list x x x) 1) 2) (list x 'lb x 'pb x))
   (check-equal? (pbs (lbs (list x x x) 2) 2) (list x x 'pb x))))