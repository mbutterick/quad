#lang debug racket
(require quad/base "struct.rkt" "param.rkt")
(provide (all-defined-out))

(define (make-linear-index q)
  (cons q (append* (for/list ([elem (in-list (quad-elems q))]
                              #:when (quad? elem))
                             (make-linear-index elem)))))

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

(define quad-params (hasheq 'doc current-doc
                      'section current-section
                      'page current-page
                      'column current-column
                      'block current-block
                      'line current-line))

(define (parse-query str)
  (for/list ([piece (in-list (string-split str ":"))])
            (match (regexp-match #px"^(.*)\\[(.*?)\\]$" piece)
              [#false (cons (string->key piece) #false)]
              [(list _ name "this") (cons (let ([quad-param-val ((hash-ref quad-params (string->key name)))])
                                            (λ (q) (eq? q quad-param-val))) 1)]
              [(list _ name arg) (cons (hash-ref preds (string->key name)) (or (string->number arg)
                                                                               (string->symbol arg)))])))

(define (query quad-or-index query-str)
  (for/fold ([qs (match quad-or-index
                   [(? quad? q) (make-linear-index q)]
                   [idx idx])]
             #:result (and qs (car qs)))
            ([query-piece (in-list (parse-query query-str))])
    (match-define (cons pred count) query-piece)
    (let loop ([qs qs][seen 0])
      (define maybe-tail (memf pred qs))
      (and maybe-tail
           (let ([seen (add1 seen)])
             (cond
               [(= seen count) maybe-tail]
               [else (loop (cdr maybe-tail) seen)]))))))
          
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