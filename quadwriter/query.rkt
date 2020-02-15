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
  (for/fold ([vidx 0]
             #:result (and vidx (vector-ref vec vidx)))
            ([query-piece (in-list (parse-query query-str))]
             #:break (not vidx))
    (match query-piece
      [(cons pred 'this)
       ;; find the querying quad, and from there search backward
       ;; todo: `this` should also cut down the domain of searching
       (for/first ([vidx (in-range (vector-memq starting-q vec) -1 -1)]
                   #:when (pred (vector-ref vec vidx)))
         vidx)]
      [(cons pred 'last) (error 'unimplemented)]
      [(cons pred 'prev) (error 'unimplemented)]
      [(cons pred 'next) (error 'unimplemented)]
      [(cons pred count)
       (for/fold ([vidx vidx]
                  ;; sub 1 because return values add 1
                  ;; and final result should be location of matching quad
                  #:result (and vidx (sub1 vidx)))
                 ([seen (in-range count)]
                  #:break (not vidx))
         (for/first ([vidx (in-range vidx (vector-length vec))]
                     #:when (pred (vector-ref vec vidx)))
           ;; add 1 so on next iteration, we can find next matcher
           ;; and won't re-find this one immediately
           (add1 vidx)))])))
          
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