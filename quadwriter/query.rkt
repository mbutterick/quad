#lang debug racket
(require quad/base "struct.rkt")

(verbose-quad-printing? #t)

(define-syntax-rule (factory type proc)
  (make-quad #:type type
             #:elems (for/list ([i (in-range 2)])
                               (quad-update! (proc)
                                             [tag (format "~a[~a]-~a" 'proc (add1 i) (gensym))]))))

(define (line) (make-quad #:type line-quad))
(define (block) (factory block-quad line))
(define (col) (factory column-quad block))
(define (page) (factory page-quad col))
(define (sec) (factory section-quad page))
(define doc (factory doc-quad sec))

(define (parse-query-str str)
  (define (string->pred str)
    (match str
      ["doc" doc-quad?]
      [(or "section" "sec" "s") section-quad?]
      [(or "page" "pg" "p") page-quad?]
      [(or "column" "col" "c") column-quad?]
      [(or "block" "b") block-quad?]
      [(or "line" "ln" "l") line-quad?]))
  (for/list ([piece (in-list (string-split str ":"))])
            (match (regexp-match #px"^(.*)\\[(.*?)\\]$" piece)
              [#false (cons (string->pred piece) #false)]
              [(list all name arg) (cons (string->pred name) (or (string->number arg)
                                                                 (string->symbol arg)))])))

(define (query q query-str)
  (define query-assocs (parse-query-str query-str))
  (for/fold ([qs (flatten-quad q)]
             #:result (and qs (car qs)))
            ([qa (in-list query-assocs)])
    (match-define (cons pred count) qa)
    (let loop ([qs qs][seen 0])
      (define maybe-tail (memf pred qs))
      (and maybe-tail
           (let ([seen (add1 seen)])
             (cond
               [(= seen count) maybe-tail]
               [else (loop (cdr maybe-tail) seen)]))))))
          

(query doc "sec[2]:pg[1]")