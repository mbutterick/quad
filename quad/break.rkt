#lang racket/base
(require racket/contract racket/list txexpr sugar/debug
         "param.rkt" "qexpr.rkt" "atomize.rkt")
(module+ test (require rackunit))

(define/contract (break qs-in [target-size (current-line-width)]
                        #:size-proc [size-proc (λ () 1)]
                        #:break-start-key [break-start-key 'break-start]
                        #:break-end-key [break-end-key 'break-end]
                        #:breakable-proc [breakable? (λ (q) #t)])
  (((listof qexpr?)) (integer? #:size-proc procedure?
                               #:break-start-key symbol?
                               #:break-end-key symbol?
                               #:breakable-proc procedure?) . ->* . (listof qexpr?))
  (define last-breakpoint-k (λ (x) (error 'no-breakpoint-found)))
  (define (capture-k!) (let/cc k (set! last-breakpoint-k k) #f))
  (for/fold ([qs null]
             [acc-size 0]
             #:result (reverse qs))
            ([q (in-list qs-in)]
             [next-q (in-list (append (cdr qs-in) (list #f)))])
    (define next-acc-size (and next-q (+ acc-size (size-proc next-q))))
    (cond
      [(or (not next-acc-size) (and (breakable? next-q) (capture-k!)))
       (values (cons (attr-set q break-end-key "true") qs) 0)]
      [(zero? acc-size) (if (breakable? q)
                            (values (cons q qs) 0)
                            (values (cons (attr-set q break-start-key "true") qs) next-acc-size))]
      [(< next-acc-size target-size) (values (cons q qs) next-acc-size)]
      [else (last-breakpoint-k #t)])))

(module+ test
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