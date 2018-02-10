#lang debug racket/base
(require racket/contract racket/list txexpr sugar/debug sugar/list racket/promise racket/function
         "param.rkt" "qexpr.rkt" "atomize.rkt" "quad.rkt")

(define/contract (breaks xs
                         [target-size (current-line-width)]
                         [debug #f]
                         #:break-val [break-val 'break]
                         ;; todo: generalize these procs so they're not particular to quads
                         #:mandatory-break-proc [mandatory-break? (λ (q) (and (quad? q) (memv (car (qe q)) '(#\newline))))]
                         #:optional-break-proc [optional-break? (λ (q) (and (quad? q) (memv (car (qe q)) '(#\space))))]
                         #:size-proc [size-proc (λ (q) (let ([val (hash-ref (qa q) 'size (λ ()
                                                                                           (if (memv (car (qe q)) '(#\space))
                                                                                               (delay (values 0 1 0))
                                                                                               (delay (values 1 1 1)))))])
                                                         (if (promise? val) (force val) (val))))])
  ((any/c) (integer? any/c
                     #:break-val any/c
                     #:mandatory-break-proc procedure?
                     #:optional-break-proc procedure?
                     #:size-proc procedure?) . ->* . (listof any/c))
  #;(and (pair? xs)
         (let ([can-be-break? (disjoin mandatory-break? optional-break?)])
           (for/first ([x (in-list xs)]
                       [next-x (in-list (cdr xs))]
                       #:when (and (can-be-break? x) (can-be-break? next-x)))
             (raise-argument-error 'breaks "no adjacent break possibilities allowed in input" (list x next-x)))))
  (define start-signal (gensym))
  (define last-breakpoint-k #f)
  (define (capture-k!) (let/cc k (set! last-breakpoint-k k) #f))
  (for/fold ([vals null]
             [size-so-far start-signal]
             #:result (reverse (dropf vals optional-break?)))
            ([x (in-list xs)])
    (define (at-start?) (eq? size-so-far start-signal))
    (define (underflow?) (<= (+ size-so-far size-end) target-size))
    (define (add-to-segment)
      (values (cons x vals) (if (at-start?)
                                size-start
                                (+ size-so-far size-mid))))
    (define-values (size-start size-mid size-end) (size-proc x))
    (define (insert-break)
      ;; when break is found, q is omitted from accumulation
      ;; and any preceding optional breaks are dropped (that would be trailing before the break)
      (values (cons break-val (dropf vals optional-break?)) start-signal))
    (cond
      [(mandatory-break? x) (when debug (report x 'got-mandatory-break))
                            (insert-break)]
      [(optional-break? x)
       (cond
         [(at-start?) (when debug (report x 'skipping-opt-break-at-beginning)) (values vals size-so-far)]
         [(and (underflow?) (capture-k!)) (when debug (report x 'resuming-breakpoint))
                                          (set! last-breakpoint-k #f) ;; prevents continuation loop
                                          (insert-break)]
         [else (when debug (report x 'add-optional-break))
               (add-to-segment)])]
      [(or (at-start?) (underflow?)) (when debug (report x 'add-ordinary-char))
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

  (test-case
   "chars"
   (check-equal? (breaks (list) 1) null)  
   (check-equal? (breaks (list x) 1) (list x))
   (check-equal? (breaks (list x x) 1) (list x 'break x))
   (check-equal? (breaks (list x x x) 1) (list x 'break x 'break x))
   (check-equal? (breaks (list x x x) 2) (list x x 'break x))
   (check-equal? (breaks (list x x x x) 2) (list x x 'break x x))
   (check-equal? (breaks (list x x x x x) 3) (list x x x 'break x x))
   (check-equal? (breaks (list x x x x x) 1) (list x 'break x 'break x 'break x 'break x))
   (check-equal? (breaks (list x x x x x) 10) (list x x x x x)))

  (test-case
   "chars and spaces"
   (check-equal? (breaks (list x sp x) 1) (list x 'break x))
   (check-equal? (breaks (list x x sp x) 2) (list x x 'break x))
   (check-equal? (breaks (list a sp b) 3) (list a sp b))
   (check-equal? (breaks (list x sp x x) 3) (list x 'break x x)))

  (test-case
   "leading & trailing spaces"
   (check-equal? (breaks (list sp x) 2) (list x))
   (check-equal? (breaks (list x sp) 2) (list x))
   (check-equal? (breaks (list sp x sp) 2) (list x))
   (check-equal? (breaks (list sp sp x sp sp) 2) (list x))
   (check-equal? (breaks (list sp sp x sp sp x sp) 1) (list x 'break x)))
  
  (test-case
   "zero width nonbreakers"
   (check-equal? (breaks (list sp zwx) 2) (list zwx))
   (check-equal? (breaks (list zwx sp) 2) (list zwx))
   (check-equal? (breaks (list sp zwx sp) 2) (list zwx))
   (check-equal? (breaks (list sp sp zwx sp sp) 2) (list zwx))
   (check-equal? (breaks (list sp sp zwx sp sp zwx sp) 2) (list zwx sp sp zwx)))

  (test-case
   "mandatory breaks"
   (check-equal? (breaks (list br) 2) (list 'break))
   (check-equal? (breaks (list a br b) 2) (list a 'break b))
   (check-equal? (breaks (list x br x x) 3) (list x 'break x x))
   (check-equal? (breaks (list x x br x) 3) (list x x 'break x))
   (check-equal? (breaks (list x x x x) 3) (list x x x 'break x))
   (check-equal? (breaks (list x x x sp x x) 2) (list x x 'break x 'break x x))
   (check-equal? (breaks (list x x x sp x x) 3) (list x x x 'break x x)))

  (test-case
   "mandatory breaks and spurious spaces"
   (check-equal? (breaks (list a sp sp sp br b) 2) (list a 'break b))
   (check-equal? (breaks (list x sp br sp sp x x sp) 3) (list x 'break x x))
   (check-equal? (breaks (list sp sp x x sp sp br sp sp sp x) 3) (list x x 'break x))
   (check-equal? (breaks (list a sp b sp sp br sp c) 3) (list a sp b 'break c))
   (check-equal? (breaks (list x x x x) 3) (list x x x 'break x))
   (check-equal? (breaks (list x x x sp x x) 2) (list x x 'break x 'break x x))
   (check-equal? (breaks (list x x x sp x x) 3) (list x x x 'break x x)))

  (define (visual-breaks str int)
    (apply string (for/list ([b (in-list (breaks (atomize str) int))])
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
   (check-equal? (visual-breaks "My dog has fleas" 16) "My dog has fleas")))