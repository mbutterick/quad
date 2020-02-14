#lang debug racket
(require quad/base "struct.rkt")

(verbose-quad-printing? #t)

(define-syntax-rule (factory type proc)
  (make-quad #:type type
             #:elems (for/list ([i (in-range 2)])
                               (quad-update! (proc)
                                             [tag (format "~a[~a]" 'proc (add1 i))]))))

(define (line) (make-quad #:type line-quad))
(define (block) (factory block-quad line))
(define (col) (factory column-quad block))
(define (sec) (factory section-quad col))
(define doc (factory doc-quad sec))

(define (parse-query-str str)
  (define (string->pred str)
    (match str
      ["doc" doc-quad?]
      ["section" section-quad?]
      ["page" page-quad?]
      ["column" column-quad?]
      ["block" block-quad?]
      ["line" line-quad?]))
  (for/list ([piece (in-list (string-split str ":"))])
            (match (regexp-match #px"^(.*)\\[(.*?)\\]$" piece)
              [#false (cons (string->pred piece) #false)]
              [(list all name arg) (cons (string->pred name) (or (string->number arg)
                                                                   (string->symbol arg)))])))

(define (query q query-str)
  (define query-assocs (parse-query-str query-str))
  #R query-assocs
  (for/fold ([ctx q]
             #:result #R ctx)
            ([(pred count) (in-dict query-assocs)])
    #R pred
    (define matches null)
    (let loop ([this ctx])
      #R this
      (cond
        [(= (length matches) count) #R (car matches)]
        [(quad? this)
         (when (pred this)
             (set! matches (cons this matches)))
         (for-each loop (quad-elems this))]
        [else this]))))

(query doc "section[1]:line[2]")