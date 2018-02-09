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
(define/contract (breaks qs-in
                         [target-size (current-line-width)]
                         #:break-val [break-val 'break]
                         #:mandatory-break-proc [mandatory-break? (λ (q) (memv (car (qe q)) '(#\newline)))]
                         #:optional-break-proc [optional-break? (λ (q) (memv (car (qe q)) '(#\space)))]
                         #:size-proc [size-proc (λ (q) (let ([val (hash-ref (qa q) 'size (λ ()
                                                                                           (if (memv (car (qe q)) '(#\space))
                                                                                               (delay (values 0 1 0))
                                                                                               (delay (values 1 1 1)))))])
                                                         (if (promise? val) (force val) (val))))])
  ((quads?) (integer? #:break-val any/c
                      #:mandatory-break-proc procedure?
                      #:optional-break-proc procedure?
                      #:size-proc procedure?) . ->* . (listof any/c))
  (define last-breakpoint-k #f)
  (define (capture-k!) (let/cc k (set! last-breakpoint-k k) #f))
  (define break-here #t)
  (for/fold ([qss null]
             [break-open? #t]
             [size-so-far 0]
             #:result (reverse qss))
            ([(q qidx) (in-indexed qs-in)])
    (define-values (size-start size-mid size-end) (size-proc q))
    (cond
      [(not break-open?) (when debug (report q 'open-break))
                         (values (append (list q) qss) (not break-open?) (+ size-so-far size-start))]
      [(<= (+ size-so-far size-end) target-size) ;; check condition based on size-end (as if x were breakpoint) ...
       (cond
         [(or (mandatory-break? q)
              (and (optional-break? q) (capture-k!))) ;; return point for `last-breakpoint-k`
          (when debug (report q 'resuming-breakpoint))
          (set! last-breakpoint-k #f) ;; prevents continuation loop
          ;; when break is found, q is omitted from accumulation
          (values (append (list break-val) qss) (not break-open?) 0)] ;; closes the break at this quad
         [else (when debug (report q 'add-to-line))
               (values (append (list q) qss) break-open? (if (zero? size-so-far) ;; we're still at start
                                                             size-start
                                                             (+ size-so-far size-mid)))])] ;; otherwise recur based on size-mid
      ;; overflow handlers
      [last-breakpoint-k (when debug (report q 'invoking-last-breakpoint))
                         (last-breakpoint-k #t)]
      [else (when debug (report q 'falling-back))
            (values (append (list q break-val) qss) break-open? size-start)])))
;; fallback if no last-breakpoint-k exists

;; todo bug: constrain breaking to certain junctures
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

(module+ test
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