#lang debug racket/base
(require racket/match racket/function "generic.rkt")
(provide (all-defined-out))
(module+ test (require rackunit))

(struct $quad (attrs elems) #:transparent
  #:methods gen:quad
  [(define (elems q) ($quad-elems q))
   (define (attrs q) ($quad-attrs q))
   (define (entrance-point q) (hash-ref (attrs q) 'entrance 'entrance))
   (define (exit-point q) (hash-ref (attrs q) 'exit 'exit))
   (define (inner-point q) (hash-ref (attrs q) 'inner 'inner))
   (define (size q [condition #f]) (hash-ref (attrs q) 'size (λ () (length (elems q)))))
   (define (draw q [surface #f] [origin #f]) ((hash-ref (attrs q) 'draw (λ () (λ () (println "<no draw routine>"))))))])

(define (quad-attrs? x) (and (hash? x) (hash-eq? x)))
(define (quad-elem? x) (or (char? x) (string? x) ($quad? x)))
(define (quad-elems? xs) (and (pair? xs) (andmap quad-elem? xs)))
(define (quad #:type [type $quad] . xs)
  (match xs
    [(list #f xs ...) (apply quad #:type type (hasheq) xs)]
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