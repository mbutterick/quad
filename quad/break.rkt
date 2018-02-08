#lang sugar/debug racket/base
(require racket/contract racket/list txexpr sugar/debug racket/promise
         "param.rkt" "qexpr.rkt" "atomize.rkt")
(module+ test (require rackunit))

(define/contract (break xs-in [target-size (current-line-width)])
  (((listof (or/c promise? procedure?))) (integer?) . ->* . (listof any/c))
  (define last-breakpoint-k #f)
  (define (capture-k!) (let/cc k (set! last-breakpoint-k k) #f))
  (for/fold ([xs null]
             [break-open? #t]
             [size-so-far 0]
             #:result (reverse xs))
            ([(x idx) (in-indexed xs-in)])
    (define-values (size-start size-mid size-end breakability) (if (promise? x) (force x) (x)))
    (cond
      [(not break-open?) (values (cons #f xs) #t (+ size-so-far size-start))]
      [(<= (+ size-so-far size-end) target-size) ;; check condition based on size-end (as if x were breakpoint) ...
       (cond
         [(or (eq? breakability 'must) (and (eq? breakability 'can) (capture-k!))) ;; return point for `last-breakpoint-k`
          (set! last-breakpoint-k #f)
          (values (cons #f xs) #f 0)] ;; closes the break at this quad
         [else
          (values (cons #f xs) #t (if (zero? size-so-far) ;; we're still at start
                                      size-start
                                      (+ size-so-far size-mid)))])] ;; otherwise recur based on size-mid
      ;; todo bug: but this doesn't work right with a trailing sequence of word spaces
      
      ;; overflow handlers
      [last-breakpoint-k (last-breakpoint-k #t)]
      [else (values (cons #t xs) #t size-start)]))) ;; fallback if no last-breakpoint-k exists

;; todo bug: constrain breaking to certain junctures
(define ch (delay (values 1 1 1 #f)))
(define sp (delay (values 0 1 0 'can)))
(define br (delay (values 0 0 0 'must)))

(module+ test
  (check-equal? (break (list) 1) null)
  (check-equal? (break (list ch) 1) '(#f))
  (check-equal? (break (list ch ch) 1) '(#f #t))
  (check-equal? (break (list ch ch ch) 1) '(#f #t #t))
  (check-equal? (break (list ch ch ch) 2) '(#f #f #t))
  (check-equal? (break (list ch ch ch ch) 2) '(#f #f #t #f))
  (check-equal? (break (list ch ch ch ch ch) 3) '(#f #f #f #t #f))
  (check-equal? (break (list ch ch ch ch ch) 1) '(#f #t #t #t #t))
  (check-equal? (break (list ch ch ch ch ch) 10) '(#f #f #f #f #f))

  
  (check-equal? (break (list sp) 1) '(#f))
  (check-equal? (break (list sp sp) 1) '(#f #f))
  (check-equal? (break (list sp sp) 2) '(#f #f))
  (check-equal? (break (list sp sp) 3) '(#f #f))
  (check-equal? (break (list sp sp sp) 1) '(#f #f #f))
  (check-equal? (break (list sp sp sp) 2) '(#f #f #f))
  (check-equal? (break (list sp sp sp) 3) '(#f #f #f))

  ;; now it gets weird
  (check-equal? (break (list ch sp) 1) '(#f #f))
  (check-equal? (break (list sp ch) 1) '(#f #f))
  (check-equal? (break (list sp ch ch) 1) '(#f #f #t))
  (check-equal? (break (list ch sp ch) 1) '(#f #f #t))
  #|
  (check-equal? (break (list ch sp sp ch) 1) '(#f #f #t #f))
  (check-equal? (break (list ch sp ch sp) 1) '(#f #f #t #f))
  (check-equal? (break (list ch ch sp ch) 2) '(#f #f #f #t))
  (check-equal? (break (list ch sp ch) 3) '(#f #f #f))
  (check-equal? (break (list ch sp ch ch) 3) '(#f #f #t #f))

  ;; trailing spaces
  (check-equal? (break (list ch sp) 3) '(#t #f))
  (check-equal? (break (list ch sp sp) 3) '(#t #f #f))
  (check-equal? (break (list ch sp sp) 2) '(#t #f #f))
  (check-equal? (break (list ch sp sp) 1) '(#t #f #f)) ; fails
  
  (check-equal? (break (list ch br ch) 2) '(#t #f #t))
  (check-equal? (break (list ch br ch ch) 3) '(#t #f #t #f))
  (check-equal? (break (list ch ch br ch) 3) '(#t #f #f #t))
  (check-equal? (break (list ch ch ch ch) 3) '(#t #f #f #t))

  (check-equal? (break (list ch ch ch sp sp ch ch) 2) '(#t #f #t #f #f #t #f))
  (check-equal? (break (list ch ch ch sp ch ch) 3) '(#t #f #f #f #t #f))
  |#

  )


