#lang debug racket/base
(require racket/match racket/function racket/promise racket/dict "generic.rkt")
(provide (all-defined-out))
(module+ test (require rackunit))

(define (default-visibility-proc q sig)
  (match (elems q)
    [(list (? (λ (x) (and (char? x) (char-whitespace? x))) c)) (case sig
                                                                 [(start end) #f]
                                                                 [else #t])]
    [else #t]))

(struct $quad (attrs elems) #:transparent #:mutable
  #:methods gen:quad
  [(define (elems q) ($quad-elems q))
   (define (attrs q) ($quad-attrs q))
   ;; why 'nw and 'ne as defaults for in and out points
   ;; if size is '(0 0), the points are the same, and everything piles up at the origin
   ;; if size is otherwise, the items don't pile up (but rather lay out in a row)
   (define (in q) (hash-ref (attrs q) 'in 'nw))
   (define (out q) (hash-ref (attrs q) 'out 'ne))
   (define (inner q) (hash-ref (attrs q) 'inner (λ () (in q))))
   (define (printable? q [signal #f]) 
     (let ([v (hash-ref (attrs q) 'printable? (λ () (default-visibility-proc q signal)))])
       (cond
         [(procedure? v) (v signal)]
         [(promise? v) (force v)]
         [else v])))
   (define (size q) (let ([v (hash-ref (attrs q) 'size '(0 0))])
                      (cond
                        [(procedure? v) (v)]
                        [(promise? v) (force v)]
                        [else v])))
   (define (offset q [signal #f]) (hash-ref (attrs q) 'offset '(0 0)))
   (define (origin q) (hash-ref (attrs q) 'origin '(0 0)))
   (define (set-origin! q val) (set-$quad-attrs! q (hash-set (attrs q) 'origin val)))
   (define (draw q [surface #f])
     (define (default-draw-proc q surface)
       (for-each (λ (e) (draw e surface)) (elems q)))
     ((hash-ref (attrs q) 'draw (const default-draw-proc)) q surface))])

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

#|
(struct $break $quad () #:transparent)
(define (break . xs) (apply quad #:type $break xs))
(define b break)
|#

(module+ test
  (define x ($quad (hasheq 'entrance 0
                           'exit 10+10i
                           'inner 5+5i
                           'size 8+8i
                           'draw (λ (q doc) (println "foo"))) '(#\H #\e #\l #\o)))
  (draw x))