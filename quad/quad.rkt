#lang debug racket/base
(require racket/match racket/promise racket/dict racket/class)
(provide (all-defined-out))
(module+ test (require rackunit))

(define quad%
  (class* object% (equal<%>)
    (super-new)
    (init-field [(@attrs attrs) null] [(@elems elems) null])
    ;; why 'nw and 'ne as defaults for in and out points:
    ;; if size is '(0 0), 'nw and 'ne are the same point,
    ;; and everything piles up at the origin
    ;; if size is otherwise, the items don't pile up (but rather lay out in a row)
    (field [(@in in) 'nw]
           [(@out out) 'ne]
           [(@inner inner) #f]
           [@printable #f]
           [(@size size) '(0 0)]
           [(@offset offset) '(0 0)]
           [(@origin origin) '(0 0)])

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
    
    (define/public (pre-draw surface) (void))
    (define/public (post-draw surface) (void))

    (define/public (draw [surface #f])
      (pre-draw surface)
      (for-each (λ (e) (send e draw surface)) @elems)
      (post-draw surface))

    ;; equal<%> interface
    (define/public-final (equal-to? other recur)
      (define other-attrs (get-field attrs other))
      (define other-elems (get-field elems other))
      (and (list? @attrs)
           (list? other-attrs)
           (= (length @attrs) (length other-attrs))
           (andmap equal? (sort (hash->list @attrs) #:key car symbol<?)
                   (sort (hash->list other-attrs) #:key car symbol<?))
           (= (length @elems) (length other-elems))
           (andmap equal? @elems other-elems)))
 
    ;; The hash codes need to be insensitive to casing as well.
    ;; We'll just downcase the word and get its hash code.
    (define/public-final (equal-hash-code-of hash-code)
      (hash-code this))
 
    (define/public-final (equal-secondary-hash-code-of hash-code)
      (hash-code this))))

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
(define (atomic-quad? x) (and (quad? x) (match (get-field elems x)
                                          [(list (? char?)) #t]
                                          [else #f])))
(define (atomic-quads? xs) (andmap atomic-quad? xs))

(module+ test
  (check-true (atomic-quad? (make-object quad% '#hasheq() '(#\H))))
  (check-true (atomic-quads? (list (make-object quad% '#hasheq() '(#\H))))))

#|
(define break% (class quad% (super-new)))
(define (break . xs) (apply quad #:type break% xs))
(define b break)
|#

(module+ test
  (define x (make-object
                (class quad%
                  (super-new)
                  (define/override (draw [surface #f])
                    (println "foo"))) '(#\H #\e #\l #\o)))
  (send x draw))
