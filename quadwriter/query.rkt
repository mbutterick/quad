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

(define (find-inclusive vec pred start-arg end-arg [count 1])
  ;; search from lidx to ridx inclusive
  (define-values (start end step)
    (cond
      ;; if lidx is bigger, search backward
      [(> start-arg end-arg) (values start-arg (sub1 end-arg) -1)]
      [else (values start-arg (add1 end-arg) 1)]))
  (for/fold ([adjusted-start (- start step)]) ; remove step for reason below
            ([seen (in-range count)]
             #:break (not adjusted-start))
    ;; add step so we find next matcher
    ;; and won't re-find the last one immediately
    (for/first ([idx (in-range (+ adjusted-start step) end step)]
                #:when (pred (vector-ref vec idx)))
               idx)))

(define (query-one vec pred subscript this-idx maxidx)
  (define (find start end count) (find-inclusive vec pred start end count))
  (let loop ([subscript subscript])
    (match subscript
      [(== 'this eq?) ; start at querying quad, then search 1 back
       (find this-idx 0 1)]
      [(== 'last eq?) (loop -1)]
      [(== 'prev eq?) ; search 2 back. same algo if current q is pred or not.
       (find this-idx 0 2)]
      [(== 'next eq?) ; search 1 ahead, but if current q is also pred, search 2 ahead
       (find this-idx maxidx (if (pred (vector-ref vec this-idx)) 2 1))]
      [(? number? count)
       (cond
         [(negative? count) ; search backward from end
          (find maxidx this-idx (abs count))]
         [else ; seach forward
          (find this-idx maxidx count)])]
      [_ #false])))

(define (multiquery? x) (memq x '(* all)))

(define (query quad-or-index query-str [query-q #false])
  (define vec (match quad-or-index
                [(? quad? q) (make-query-index q)]
                [idx idx]))
  (when query-q (unless (quad? query-q)
                  (raise-argument-error 'query "quad" query-q)))
  (define all-query-pieces (parse-query query-str))
  (and (pair? all-query-pieces)
       ;; initial subtree is whole tree
       ;; each subtree is a pair of start idx (initial node) + end idx (boundary of subtree)
       (let loop ([subtrees (list (cons (if query-q (vector-memq query-q vec) 0) (sub1 (vector-length vec))))]
                  [query-pieces all-query-pieces])
         (cond
           ;; if subtrees are null, we have eliminated all searchable domains
           [(null? subtrees) #false]
           [(null? query-pieces)
            ;; multi mode means one of the query subscripts is a wildcard
            (define multi-mode? (for/or ([(_ subscript) (in-dict all-query-pieces)])
                                        (multiquery? subscript)))
            (match (for/list ([(idx _) (in-dict subtrees)])
                             (vector-ref vec idx))
              [vals #:when multi-mode? vals] ; in multi mode, return list
              [(list val) val] ; otherwise return single value
              [_ (error 'should-never-have-multiple-vals-in-single-mode)])]
           [else
            (match-define (cons (cons pred subscript) other-query-pieces) query-pieces)
            (define finish-proc
              (if (null? other-query-pieces)
                  ;; don't need to calculate end-idxs if we're at the end of the query
                  (λ (idx end-idx) 'ignored-value)
                  (λ (idx end-idx) (cond ;; otherwise calculate new end-idx
                                     [(not idx) end-idx]
                                     ;; try searching for next occurence after this one, up to max.
                                     [(find-inclusive vec pred (add1 idx) end-idx 1)]
                                     [else end-idx]))))
            (loop
             (for*/list ([(start-idx end-idx) (in-dict subtrees)]
                         [idx (cond
                                [(multiquery? subscript)
                                 (in-list (for/list ([idx (in-range start-idx (add1 end-idx))]
                                                     #:when (pred (vector-ref vec idx)))
                                                    idx))]
                                [else (in-value (query-one vec pred subscript start-idx end-idx))])]
                         #:when idx)
                        (cons idx (finish-proc idx end-idx)))
             other-query-pieces)]))))
          
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
  
  (check-equal? (count (query doc "sec[2]")) 242)
  (check-false (query doc "sec[102]:line[1]"))
  (check-equal? (count (query doc "sec[2]:pg[1]")) 162)
  (check-equal? (count (query doc "sec[2]:pg[1]:ln[3]")) 128)
  (check-eq? (query doc "page[this]" (query doc "line[2]")) (query doc "page[1]"))
  (check-equal? (count (query doc "page[this]:line[last]" (query doc "line[2]"))) 41)
 
  (check-equal? (count (query doc "sec[next]" (query doc "sec[1]"))) (count (query doc "sec[2]")) )
  (check-equal? (count (query doc "sec[prev]" (query doc "sec[2]"))) (count (query doc "sec[1]")))

  (check-equal? (count (query doc "page[prev]" (query doc "page[2]:line[1]")))
                (count (query doc "page[1]")))
  (check-equal? (count (query doc "page[next]" (query doc "page[2]:line[1]")))
                (count (query doc "page[3]")))

  (check-equal? (count (query doc "page[next]" (query doc "page[2]:line[1]")))
                (count (query doc "page[3]")))

  (check-equal? (count (query doc "page[next]:page[next]" (query doc "page[1]:line[1]")))
                (count (query doc "page[3]")))

  (check-equal? (count (query doc "page[next]:page[this]:page[prev]" (query doc "page[1]:line[1]")))
                (count (query doc "page[1]")))


  (check-equal? (map count (query doc "sec[*]")) '(121 242 363))
  (check-equal? (map count (query doc "sec[*]:page[1]")) '(41 162 283))
  (check-equal? (map count (query doc "sec[2]:page[1]:line[*]"))
                '(126 127 128 130 131 132 134 135 136 139 140 141 143 144 145 147 148 149 152 153 154 156 157 158 160 161 162))
  
  )