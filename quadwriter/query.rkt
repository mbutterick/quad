#lang debug racket
(require quad/base "struct.rkt")
(provide (all-defined-out))

(define (make-linear-index q)
  (cons q (append* (for/list ([elem (in-list (quad-elems q))]
                                   #:when (quad? elem))
                          (make-linear-index elem)))))

(define (string->pred str)
  (match str
    ["doc" doc-quad?]
    [(or "section" "sec" "s") section-quad?]
    [(or "page" "pg" "p") page-quad?]
    [(or "column" "col" "c") column-quad?]
    [(or "block" "b") block-quad?]
    [(or "line" "ln" "l") line-quad?]))

(define (parse-query str)
  (for/list ([piece (in-list (string-split str ":"))])
    (match (regexp-match #px"^(.*)\\[(.*?)\\]$" piece)
      [#false (cons (string->pred piece) #false)]
      [(list all name arg) (cons (string->pred name) (or (string->number arg)
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