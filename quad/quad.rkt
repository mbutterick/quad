#lang debug racket/base
(require racket/match racket/function racket/promise racket/dict "generic.rkt")
(provide (all-defined-out))
(module+ test (require rackunit))

(define (default-size-proc q sig)
  (match (elems q)
    [(list (? (λ (x) (and (char? x) (char-whitespace? x))) c)) (case sig
                                                                 [(start end) '(0 0)]
                                                                 [else '(1 1)])]
    [else '(1 1)]))

(struct $quad (attrs elems) #:transparent #:mutable
  #:methods gen:quad
  [(define (elems q) ($quad-elems q))
   (define (attrs q) ($quad-attrs q))
   (define (start q) (hash-ref (attrs q) 'start 'nw))
   (define (end q) (hash-ref (attrs q) 'end 'ne))
   (define (inner q) (hash-ref (attrs q) 'inner (λ () (start q))))
   (define (size q [signal #f]) (let ([v (hash-ref (attrs q) 'size (λ () (default-size-proc q signal)))])
                                  (cond
                                    [(procedure? v) (v signal)]
                                    [(promise? v) (force v)]
                                    [else v])))
   (define (offset q [signal #f]) (hash-ref (attrs q) 'offset '(0 0)))
   (define (origin q) (hash-ref (attrs q) 'origin '(0 0)))
   (define (set-origin! q val) (set-$quad-attrs! q (hash-set (attrs q) 'origin val)))
   (define (draw q [surface #f] [origin #f]) ((hash-ref (attrs q) 'draw (λ () (λ () (println "<no draw routine>"))))))])

(define (quad-attrs? x) (and (hash? x) (hash-eq? x)))
(define (quad-elem? x) (or (char? x) (string? x) ($quad? x)))
(define (quad-elems? xs) (and (pair? xs) (andmap quad-elem? xs)))
(define (quad #:type [type $quad] . xs)
  (match xs
    [(list #f xs ...) (apply quad #:type type (hasheq) xs)]
    [(list (list (? symbol? sym) rest ...) (? quad-elem? elems) ...) (type (apply hasheq (cons sym rest)) elems)]
    [(list (? dict? attrs) (? quad-elem? elems) ...) (type (for/hasheq ([(k v) (in-dict attrs)])
                                                             (values k v)) elems)]
    [(list (? quad-attrs? attrs) (? quad-elem? elems) ...) (type attrs elems)]
    [(list (? quad-elem? elems) ...) (apply quad #:type type #f elems)]
    [else (error 'bad-quad-input)]))
(define q quad)
(define (quads? xs) (andmap quad? xs))
(define (atomic-quad? x) (and (quad? x) (match (elems x)
                                          [(list (? char?)) #t]
                                          [else #f])))
(define (atomic-quads? xs) (andmap atomic-quad? xs))
(module+ test
  (check-true (atomic-quad? ($quad '#hasheq() '(#\H))))
  (check-true (atomic-quads? (list ($quad '#hasheq() '(#\H))))))


(struct $break $quad () #:transparent)
(define (break . xs) (apply quad #:type $break xs))
(define b break)

(module+ test
  (define x ($quad (hasheq 'entrance 0
                           'exit 10+10i
                           'inner 5+5i
                           'size 8+8i
                           'draw (λ () (println "foo"))) '(#\H #\e #\l #\o)))
  (draw x))