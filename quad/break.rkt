#lang debug racket/base
(require racket/contract racket/list txexpr sugar/debug sugar/list racket/promise
         "param.rkt" "qexpr.rkt" "atomize.rkt" "quad.rkt")
(module+ test (require rackunit))

(define (alternating-atomic-quads? xs)
  (or (null? xs)
      (and (atomic-quads? xs)
           (not ($break? (first xs)))
           (not ($break? (last xs)))
           (let ([sublists (filter-split xs (compose1 not $break?))])
             (or (null? sublists) (= 1 (apply max (map length sublists))))))))

(define debug #f)
(define/contract (breaks qs-in [target-size (current-line-width)])
  ((alternating-atomic-quads?) (integer?) . ->* . (listof any/c))
  (define last-breakpoint-k #f)
  (define (capture-k!) (let/cc k (set! last-breakpoint-k k) #f))
  (define break-here #t)
  (define mandatory-breaks '(#\newline))
  (for/fold ([bs null]
             [break-open? #t]
             [size-so-far 0]
             #:result (reverse bs))
            ([(q qidx) (in-indexed qs-in)])
    (define-values (size-start size-mid size-end) (let ([val (hash-ref (qa q) 'size (λ ()
                                                                                      (if ($break? q)
                                                                                          (delay (values 0 1 0))
                                                                                          (delay (values 1 1 1)))))])
                                                    (if (promise? val) (force val) (val))))
    (cond
      [(not break-open?) (when debug (report q 'open-break))
                         (values (cons (not break-here) bs) (not break-open?) (+ size-so-far size-start))]
      [(<= (+ size-so-far size-end) target-size) ;; check condition based on size-end (as if x were breakpoint) ...
       (cond
         [(or (memv (car (qe q)) mandatory-breaks)
              (and ($break? q) (capture-k!))) ;; return point for `last-breakpoint-k`
          (when debug (report q 'resuming-breakpoint))
          (set! last-breakpoint-k #f) ;; prevents continuation loop
          (values (cons break-here bs) (not break-open?) 0)] ;; closes the break at this quad
         [else (when debug (report q 'add-to-line))
               (values (cons (not break-here) bs) break-open? (if (zero? size-so-far) ;; we're still at start
                                                                  size-start
                                                                  (+ size-so-far size-mid)))])] ;; otherwise recur based on size-mid
      ;; overflow handlers
      [last-breakpoint-k (when debug (report q 'invoking-last-breakpoint))
                         (last-breakpoint-k #t)]
      [else (when debug (report q 'falling-back))
            (values (cons break-here bs) break-open? size-start)]))) ;; fallback if no last-breakpoint-k exists

;; todo bug: constrain breaking to certain junctures
(define ch (q (hasheq 'size (delay (values 1 1 1))) #\x))
(define a (q (hasheq 'size (delay (values 1 1 1))) #\a))
(define b (q (hasheq 'size (delay (values 1 1 1))) #\b))
(define c (q (hasheq 'size (delay (values 1 1 1))) #\c))
(define d (q (hasheq 'size (delay (values 1 1 1))) #\d))
(define sp (break (hasheq 'size (delay (values 0 1 0))) #\space))
(define br (break (hasheq 'size (delay (values 0 0 0))) #\newline))

(define (visual-breaks str int)
  (apply string (for/list ([c (in-string str)]
                           [b (in-list (breaks (atomize str) int))])
                          (cond
                            [(not b) c]
                            [(eqv? c #\space) #\|]
                            [else #\*]))))

(module+ test
  (check-equal? (breaks (list) 1) null)
  (check-equal? (breaks (list ch) 1) '(#f))
  (check-equal? (breaks (list ch ch) 1) '(#f #t))
  (check-equal? (breaks (list ch ch ch) 1) '(#f #t #t))
  (check-equal? (breaks (list ch ch ch) 2) '(#f #f #t))
  (check-equal? (breaks (list ch ch ch ch) 2) '(#f #f #t #f))
  (check-equal? (breaks (list ch ch ch ch ch) 3) '(#f #f #f #t #f))
  (check-equal? (breaks (list ch ch ch ch ch) 1) '(#f #t #t #t #t))
  (check-equal? (breaks (list ch ch ch ch ch) 10) '(#f #f #f #f #f))
  
  (check-equal? (breaks (list ch sp ch) 1) '(#f #t #f))
  (check-equal? (breaks (list ch ch sp ch) 2) '(#f #f #t #f))
  (check-equal? (breaks (list a sp b) 3) '(#f #f #f))
  (check-equal? (breaks (list ch sp ch ch) 3) '(#f #t #f #f))
  
  (check-equal? (breaks (list a br b) 2) '(#f #t #f))
  (check-equal? (breaks (list ch br ch ch) 3) '(#f #t #f #f))
  (check-equal? (breaks (list ch ch br ch) 3) '(#f #f #t #f))
  (check-equal? (breaks (list ch ch ch ch) 3) '(#f #f #f #t))
  (check-equal? (breaks (list ch ch ch sp ch ch) 2) '(#f #f #t #t #f #f))
  (check-equal? (breaks (list ch ch ch sp ch ch) 3) '(#f #f #f #t #f #f))

  (check-equal? (visual-breaks "My dog has fleas" 1) "M*|d**|h**|f****")
  (check-equal? (visual-breaks "My dog has fleas" 2) "My|do*|ha*|fl*a*")
  (check-equal? (visual-breaks "My dog has fleas" 3) "My|dog|has|fle*s")
  (check-equal? (visual-breaks "My dog has fleas" 4) "My|dog|has|flea*")
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