#lang debug racket/base
(require racket/match racket/promise racket/dict racket/class)
(provide (all-defined-out))
(module+ test (require rackunit))

(define quad%
  (class object%
    (super-new)
    (init-field [@attrs null] [@elems null])
    ;; why 'nw and 'ne as defaults for in and out points:
    ;; if size is '(0 0), 'nw and 'ne are the same point,
    ;; and everything piles up at the origin
    ;; if size is otherwise, the items don't pile up (but rather lay out in a row)
    (field [@in 'nw]
           [@out 'ne]
           [@inner #f]
           [@printable #f]
           [@size '(0 0)]
           [@offset '(0 0)]
           [@origin '(0 0)])
    
    (define/public-final (elems) @elems)
    (define/public-final (attrs) @attrs)
    (define/public-final (in) @in)
    (define/public-final (out) @out)
    (define/public-final (inner) (or @inner (in)))
    (define/public-final (origin) @origin)
    (define/public-final (set-origin! val) (set! @origin val))
    (define/public (offset [signal #f]) @offset)
    
    (define/public (printable? [signal #f])
      (match (or @printable
                 (match @elems
                   [(list (and (? char?) (? char-whitespace?)))
                    #:when (memq signal '(start end)) #false]
                   [else #true]))
        [(? procedure? proc) (proc signal)]
        [(? promise? prom) (force prom)]
        [val val]))

    (define/public (size)
      (match @size
        [(? procedure? proc) (proc)]
        [(? promise? prom) (force prom)]
        [val val]))
    
    (define/public (draw [surface #f])
      (for-each (λ (e) (draw e surface)) @elems))))

(define (quad? x) (is-a? x quad%))
(define (quad-attrs? x) (and (hash? x) (hash-eq? x)))
(define (quad-elem? x) (or (char? x) (string? x) (quad? x)))
(define (quad-elems? xs) (and (pair? xs) (andmap quad-elem? xs)))
(define (quad #:type [type quad%] . xs)
  (match xs
    [(list #f xs ...) (apply quad #:type type (hasheq) xs)]
    [(list (list (? symbol? sym) rest ...) (? quad-elem? elems) ...)
     (make-object type (apply hasheq (cons sym rest)) elems)]
    [(list (? dict? attrs) (? quad-elem? elems) ...)
     (make-object type (for/hasheq ([(k v) (in-dict attrs)])
                         (values k v)) elems)]
    [(list (? quad-attrs? attrs) (? quad-elem? elems) ...) (make-object type attrs elems)]
    [(list (? quad-elem? elems) ...) (apply quad #:type type #f elems)]
    [else (error 'bad-quad-input)]))
(define q quad)
(define (quads? xs) (andmap quad? xs))
(define (atomic-quad? x) (and (quad? x) (match (send x elems)
                                          [(list (? char?)) #t]
                                          [else #f])))
(define (atomic-quads? xs) (andmap atomic-quad? xs))

(module+ test
  (check-true (atomic-quad? (make-object quad% '#hasheq() '(#\H))))
  (check-true (atomic-quads? (list (make-object quad% '#hasheq() '(#\H))))))




(define break% (class quad% (super-new)))
(define (break . xs) (apply quad #:type break% xs))
(define b break)



(module+ test
  (define x (make-object
                (class quad%
                  (super-new)
                  (define/override (draw [surface #f])
                    (println "foo"))) '(#\H #\e #\l #\o)))
  (send x draw))
