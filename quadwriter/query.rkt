#lang debug racket
(require quad/base "struct.rkt" "param.rkt")
(provide (all-defined-out))

;; we want to construct a query index once with reversed variant
;; so we don't have to keep generating it
(struct query-index (forward reverse) #:transparent)

(define (make-query-index q)
  (define qs (let loop ([q q])
               (cons q (append* (for/list ([elem (in-list (quad-elems q))]
                                           #:when (quad? elem))
                                  (loop elem))))))
  (query-index qs (reverse qs)))

(define (string->key str #:this [this? #false])
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
      [(list _ name arg) (cons (hash-ref preds (string->key name)) (or (string->number arg)
                                                                       (string->symbol arg)))])))

(define (query quad-or-index query-str [querying-q #false])
  (define qi (match quad-or-index
               [(? quad? q) (make-query-index q)]
               [idx idx]))
  (define vec (list->vector (query-index-forward qi)))
  (for/fold ([vidx 0]
             #:result (and vidx (vector-ref vec vidx)))
            ([query-piece (in-list (parse-query query-str))])
    (match query-piece
      [(cons pred 'this)
       ;; resolve `this` by finding the querying quad, and searching backward
       (define this-thing (findf pred (memq querying-q (query-index-reverse qi))))
       ;; once we have this-thing, locate it in the forward index and keep going
       (memq this-thing (query-index-forward qi))]
      [(cons pred count)
       (let loop ([vidx vidx][seen 0])
         (define idx (for*/first ([vi (in-range vidx (vector-length vec))]
                                  [val (in-value (vector-ref vec vi))]
                                  #:when (pred val))
                       vi))
                                        
         (and idx
              (let ([seen (add1 seen)])
                (cond
                  [(= seen count) idx]
                  [else (loop (add1 idx) seen)]))))])))
          
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

  (define (count q) (quad-ref q 'count))
  (define doc (factory doc-quad sec))

  (check-equal? (count (query doc "sec[2]")) 242)
  (check-equal? (count (query doc "sec[2]:pg[1]")) 162)
  (check-equal? (count (query doc "sec[2]:pg[1]:ln[3]")) 128))