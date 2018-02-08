#lang sugar/debug racket/base
(require racket/match racket/function)
(provide (all-defined-out))
(module+ test (require rackunit))

(struct $quad (attrs elems) #:transparent)
(define quad? $quad?)
(define (quad-attrs? x) (and (hash? x) (hash-eq? x)))
(define (quad-elem? x) (or (char? x) (string? x) ($quad? x)))
(define (quad-elems? xs) (and (pair? xs) (andmap quad-elem? xs)))
(define (quad #:type [type $quad] . xs)
  (match xs
    [(list #f xs ...) (apply quad #:type type (hasheq) xs)]
    [(list (? quad-attrs? attrs) (? quad-elem? elems) ...) (type attrs elems)]
    [(list (? quad-elem? elems) ...) (apply quad #:type type #f elems)]
    [else (error 'bad-quad-input)]))
(define (quads? xs) (and (pair? xs) (andmap quad? xs)))
(define (atomic-quad? x) (and (quad? x) (match (qe x)
                                          [(list (? char?)) #t]
                                          [else #f])))
(define (atomic-quads? xs) (andmap atomic-quad? xs))
(module+ test
  (check-true (atomic-quad? ($quad '#hasheq() '(#\H))))
  (check-true (atomic-quads? (list ($quad '#hasheq() '(#\H))))))

(define quad-attrs $quad-attrs)
(define quad-elems $quad-elems)

(define q quad)
(define q? quad?)
(define qs? quads?)
(define qa quad-attrs)
(define qe quad-elems)

(struct $break $quad () #:transparent)
(define (break . xs) (apply quad #:type $break xs))
(define b break)
