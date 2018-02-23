#lang debug racket/base
(require racket/contract racket/list racket/match txexpr sugar/debug sugar/define sugar/list racket/promise racket/function (only-in racket/control call/prompt)
         "param.rkt" "qexpr.rkt" "atomize.rkt" "quad.rkt" "generic.rkt" "position.rkt")

(define/contract (distance q [signal #f])
  ((quad?) (any/c) . ->* . real?)
  (match-define (list x0 y0) (start-point q))
  (match-define (list x1 y1) (end-point q))
  ;; pythagorically
  (sqrt (+ (expt (- x1 x0) 2) (expt (- y1 y0) 2))))


(define+provide/contract (wrap xs
                               [target-size (current-line-width)]
                               [debug #f]
                               #:break-val [break-val 'break]
                               #:mandatory-break-proc [mandatory-break? (const #f)]
                               #:optional-break-proc [optional-break? (const #f)]
                               #:finish-segment-proc [finish-segment-proc values])
  ((any/c) (real? any/c
                  #:break-val any/c
                  #:mandatory-break-proc procedure?
                  #:optional-break-proc procedure?
                  #:finish-segment-proc procedure?) . ->* . (listof any/c))
  (define start-signal (gensym))
  (define (finish-segment pieces) (finish-segment-proc (reverse (dropf pieces optional-break?))))
  (define last-optional-break-k #f)
  (call/prompt ;; continuation boundary for last-optional-break-k
   (thunk
    (define (capture-optional-break-k!) (let/cc k (set! last-optional-break-k k) #f))
    (for/fold ([segments null]
               [pieces null]
               [size-so-far start-signal]
               #:result (append* (reverse (cons (finish-segment pieces) segments))))
              ([x (in-list xs)])
      (define at-start? (eq? size-so-far start-signal))
      (define underflow? (and (not at-start?) (<= (+ size-so-far (size x 'end)) target-size)))
      (define (add-to-segment) (values segments (cons x pieces) (if at-start?
                                                                    (size x 'start)
                                                                    (+ size-so-far (size x)))))
      (define (insert-break)
        ;; when break is found, q is omitted from accumulation
        ;; and any preceding optional breaks are dropped (that would be trailing before the break)
        (values (list* (list break-val) (finish-segment pieces) segments) null start-signal))
      (cond
        [(mandatory-break? x) (when debug (report x 'got-mandatory-break))
                              (insert-break)]
        [(optional-break? x)
         (cond
           [at-start? (when debug (report x 'skipping-opt-break-at-beginning)) (values segments null size-so-far)]
           [(and underflow? (capture-optional-break-k!)) (when debug (report x 'resuming-breakpoint))
                                                         (set! last-optional-break-k #f) ;; prevents continuation loop
                                                         (insert-break)]
           [else (when debug (report x 'add-optional-break))
                 (add-to-segment)])]
        [(or at-start? underflow?) (when debug (report x 'add-ordinary-char))
                                   (add-to-segment)]
        [last-optional-break-k (when debug (report x 'invoking-last-breakpoint))
                               (last-optional-break-k #t)]
        [else (when debug (report x 'falling-back))
              (match-define-values (vals _ _) (insert-break))
              (values vals (list x) (size x 'start))]))))) ;; fallback if no last-breakpoint-k exists



(define x (q (hasheq) #\x))
(define zwx (q (hasheq 'size (delay (values 0 0 0))) #\z))
(define a (q (hasheq 'size (delay (values 1 1 1))) #\a))
(define b (q (hasheq 'size (delay (values 1 1 1))) #\b))
(define c (q (hasheq 'size (delay (values 1 1 1))) #\c))
(define d (q (hasheq 'size (delay (values 1 1 1))) #\d))
(define sp (q (hasheq 'size (delay (values 0 1 0))) #\space))
(define br (q (hasheq 'size (delay (values 0 0 0))) #\newline))
(define optional-break? (λ (q) (and (quad? q) (memv (car (elems q)) '(#\space)))))

(define (linewrap xs size [debug #f])
  (wrap xs size debug
        #:break-val 'lb
        #:mandatory-break-proc (λ (q) (and (quad? q) (memv (car (elems q)) '(#\newline))))
        #:optional-break-proc optional-break?
        #|
        #:size-proc (λ (q) (let ([val (hash-ref (attrs q) 'size (λ ()
                                                                  (if (memv (car (elems q)) '(#\space))
                                                                      (delay (values 0 1 0))
                                                                      (delay (values 1 1 1)))))])
                             (if (promise? val) (force val) (val))))

        |#
        ))

(module+ test
  (require rackunit)

  (test-case
   "chars"
   (check-equal? (linewrap (list) 1) null)
   (check-equal? (linewrap (list x) 1) (list x))
   #|
   (check-equal? (linewrap (list x x) 1) (list x 'lb x))
   (check-equal? (linewrap (list x x x) 1) (list x 'lb x 'lb x))
   (check-equal? (linewrap (list x x x) 2) (list x x 'lb x))
   (check-equal? (linewrap (list x x x x) 2) (list x x 'lb x x))
   (check-equal? (linewrap (list x x x x x) 3) (list x x x 'lb x x))
   (check-equal? (linewrap (list x x x x x) 1) (list x 'lb x 'lb x 'lb x 'lb x))
   (check-equal? (linewrap (list x x x x x) 10) (list x x x x x))
|#

   ))