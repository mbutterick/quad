#lang debug racket
(require quad/base "struct.rkt" "param.rkt")
(provide (all-defined-out))

(define (make-query-index q)
  (define qs (let loop ([q q])
               (cons q (append* (for/list ([elem (in-list (quad-elems q))]
                                           #:when (quad? elem))
                                  (loop elem))))))
  (list->vector qs))

(define (string->key str)
  (match str
    ["doc" 'doc]
    [(or "section" "sec" "s") 'section]
    [(or "page" "pg" "p") 'page]
    [(or "column" "col" "c") 'column]
    [(or "block" "b") 'block]
    [(or "line" "ln" "l") 'line]))

(define preds (hasheq 'doc doc-quad?
                      'section section-quad?
                      'page page-quad?
                      'column column-quad?
                      'block block-quad?
                      'line line-quad?))

(define (parse-query str)
  (for/list ([piece (in-list (string-split str ":"))])
    (match (regexp-match #px"^(.*)\\[(.*?)\\]$" piece)
      [#false (cons (string->key piece) #false)]
      [(list _ name arg) (cons (hash-ref preds (string->key name))
                               (or (string->number arg) (string->symbol arg)))])))

(define (query quad-or-index query-str [starting-q #false])
  (define vec (match quad-or-index
                [(? quad? q) (make-query-index q)]
                [idx idx]))
  (define (next-occurrence-between pred lidx ridx)
    (cond
      [(or (not lidx) (not ridx)) ridx]
      ;; we are searching between, so add 1 to left idx
      [(for/first ([idx (in-range (add1 lidx) ridx)]
                   #:when (pred (vector-ref vec idx)))
         idx)]
      [else (vector-length vec)]))
  (for/fold ([vidx 0]
             [maxidx (vector-length vec)]
             #:result (and vidx (vector-ref vec vidx)))
            ([query-piece (in-list (parse-query query-str))]
             #:break (not vidx))
    (match-define (cons pred count) query-piece)
    (define res-idx
      (match count
        [(or (== 'this eq?) (== 'last eq?))
         ;; find a starting point, then search backward
         (for/first ([vidx (in-range (if (eq? count 'this)
                                         ;; start at querying quad
                                         (vector-memq starting-q vec)
                                         ;; start at end 
                                         (sub1 maxidx)) -1 -1)]
                     #:when (pred (vector-ref vec vidx)))
           vidx)]
        [(== 'prev eq?) (error 'unimplemented)]
        [(== 'next eq?) (error 'unimplemented)]
        [count
         (for/fold ([vidx vidx]
                    ;; sub 1 because return values add 1
                    ;; and final result should be location of matching quad
                    #:result (and vidx (sub1 vidx)))
                   ([seen (in-range count)]
                    #:break (not vidx))
           (for/first ([vidx (in-range vidx maxidx)]
                       #:when (pred (vector-ref vec vidx)))
             ;; add 1 so on next iteration, we can find next matcher
             ;; and won't re-find this one immediately
             (add1 vidx)))]))
    (values res-idx (next-occurrence-between pred res-idx maxidx))))
          
(module+ test
  (require rackunit)
  
  (define counter 0)
  (define-syntax-rule (factory type proc)
    (make-quad #:type type
               #:elems (for/list ([i (in-range 3)])
                         (set! counter (add1 counter))
                         (define new-q (proc))
                         (quad-update! new-q
                                       [tag (format "~a[~a]-~a" 'proc counter (gensym))])
                         (hash-set! (quad-attrs new-q) 'count counter)
                         new-q)))

  (define (line) (make-quad #:type line-quad))
  (define (block) (factory block-quad line))
  (define (col) (factory column-quad block))
  (define (page) (factory page-quad col))
  (define (sec) (factory section-quad page))

  (define (count q) (and q (quad-ref q 'count)))
  (define doc (factory doc-quad sec))

  (check-eq? (query doc "page[this]" (query doc "line[2]")) (query doc "page[1]"))
  (check-equal? (count (query doc "sec[2]")) 242)
  (check-false (query doc "sec[102]:line[1]"))
  (check-equal? (count (query doc "sec[2]:pg[1]")) 162)
  (check-equal? (count (query doc "sec[2]:pg[1]:ln[3]")) 128))