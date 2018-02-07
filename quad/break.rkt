#lang sugar/debug racket/base
(require racket/contract racket/list txexpr sugar/debug racket/promise
         "param.rkt" "qexpr.rkt" "atomize.rkt")
(module+ test (require rackunit))

(define/contract (break xs-in [target-size (current-line-width)])
  (((listof (or/c promise? procedure?))) (integer?) . ->* . (listof any/c))
  (define last-breakpoint-k (λ (x) (error 'no-breakpoint-found)))
  (define (capture-k!) (let/cc k (set! last-breakpoint-k k) #f))
  (define start-key 'start)
  (define end-key 'end)
  (for/fold ([xs null]
             [size-so-far 0]
             #:result (reverse xs))
            ([(x idx) (in-indexed xs-in)])
    (define-values (size-start size-mid size-end) (if (promise? x) (force x) (x)))
    (cond
      [(zero? size-so-far) ;; looking for break-start
       (values (cons (and (positive? size-start) start-key) xs) size-start)] ;; size-start is either zero or positive
      ;; looking for break-end
      [(or (= idx (sub1 (length xs-in))) ; x is last element, thus must be break-end
           (and (positive? size-end) (capture-k!))) ; store possible break-end candidate
       (values (cons end-key xs) 0)]
      [(< (+ size-so-far size-end) target-size) ;; check condition based on size-end (as if x were breakpoint) ...
       (values (cons #f xs) (+ size-so-far size-mid))] ;; but recur based on size-mid
      ;; todo bug: but this doesn't work right with a trailing sequence of word spaces
      [else (last-breakpoint-k #t)])))

;; todo bug: break lists have to be >= 2 elements
;; todo bug: constrain breaking to certain junctures
(define char (delay (values 1 1 1)))
(define space (delay (values 0 1 0)))
(break (list char char char space char char space char char space char char char char char) 2)

#;(module+ test
    (check-equal? (break (atomize "aaa bb cc ddddd") 5
                         #:size-proc (λ (q) 1)
                         #:break-start-key 'line-start
                         #:break-end-key 'line-end
                         #:breakable-proc (λ (q) (equal? (second q) " ")))
                  '((q ((line-start "true")) "a")
                    (q "a")
                    (q ((line-end "true")) "a")
                    (q " ")
                    (q ((line-start "true")) "b")
                    (q "b")
                    (q " ")
                    (q "c")
                    (q ((line-end "true")) "c")
                    (q " ")
                    (q ((line-start "true")) "d")
                    (q "d")
                    (q "d")
                    (q "d")
                    (q ((line-end "true")) "d")))
    (check-equal?
     (break (atomize "aaa bb cc ddddd") 6
            #:size-proc (λ (q) 1)
            #:break-start-key 'line-start
            #:break-end-key 'line-end
            #:breakable-proc (λ (q) (equal? (second q) " ")))
     '((q ((line-start "true")) "a")
       (q "a")
       (q "a")
       (q " ")
       (q "b")
       (q ((line-end "true")) "b")
       (q " ")
       (q ((line-start "true")) "c")
       (q ((line-end "true")) "c")
       (q " ")
       (q ((line-start "true")) "d")
       (q "d")
       (q "d")
       (q "d")
       (q ((line-end "true")) "d")))

    (check-equal? (break (atomize "aaa bb cc ddddd") 8
                         #:size-proc (λ (q) 1)
                         #:break-start-key 'line-start
                         #:break-end-key 'line-end
                         #:breakable-proc (λ (q) (equal? (second q) " ")))
                  '((q ((line-start "true")) "a")
                    (q "a")
                    (q "a")
                    (q " ")
                    (q "b")
                    (q ((line-end "true")) "b")
                    (q " ")
                    (q ((line-start "true")) "c")
                    (q "c")
                    (q " ")
                    (q "d")
                    (q "d")
                    (q "d")
                    (q "d")
                    (q ((line-end "true")) "d"))))