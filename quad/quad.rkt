#lang debug racket/base
(require (for-syntax racket/base racket/syntax)
         racket/struct
         racket/format
         racket/list
         racket/string
         racket/promise
         racket/dict
         racket/match
         "param.rkt"
         "rebase.rkt"
         "proto.rkt")
(provide (all-defined-out))
(module+ test (require rackunit))

(define quad-attrs values)

(define (size q)
  (match (quad-size q)
    [(? procedure? proc) proc (proc q)]
    [(? promise? prom) (force prom)]
    [val val]))

(define (printable? q [signal #f])
  (match (quad-printable q)
    [(? procedure? proc) (proc q signal)]
    [val val]))

(define (draw q [surface (current-output-port)])
  ((quad-draw-start q) q surface)
  ((quad-draw q) q surface)
  ((quad-draw-end q) q surface))

(define (hashes-equal? h1 h2)
  (and (= (length (hash-keys h1)) (length (hash-keys h2)))
       (for/and ([(k v) (in-hash h1)])
         (and (hash-has-key? h2 k) (equal? (hash-ref h2 k) v)))))

(define (quad=? q1 q2 [recur? #t])
  (and
   ;; exclude attrs from initial comparison
   (for/and ([getter (in-list (list quad-elems quad-size quad-from-parent quad-from quad-to 
                                    quad-shift quad-shift-elems quad-from-parent quad-origin quad-printable
                                    quad-draw-start quad-draw-end quad-draw))])
     (equal? (getter q1) (getter q2)))
   ;; and compare them key-by-key
   (hashes-equal? (quad-attrs q1) (quad-attrs q2))))

;; keep this param here so you don't have to import quad/param to get it
(define verbose-quad-printing? (make-parameter #f))



(define (quad-ref q key [default-val #f])
  (hash-ref (quad-attrs q) key (match default-val
                                 [(? procedure? proc) (proc)]
                                 [val val])))

(define (quad-set! q key val)
  (hash-set! (quad-attrs q) key val)
  q)

(define (default-printable q [sig #f]) #t)

(define (default-draw q surface)
  (for-each (λ (qi) (draw qi surface)) (quad-elems q)))

;; why 'nw and 'ne as defaults for in and out points:
;; if size is '(0 0), 'nw and 'ne are the same point,
;; and everything piles up at the origin
;; if size is otherwise, the items don't pile up (but rather lay out in a row)

(define (make-quad-constructor type)
  (make-keyword-procedure (λ (kws kw-args . rest)
                            (keyword-apply make-quad #:type type kws kw-args rest))))

(define (derive-quad-constructor q)
  (define-values (x-structure-type _) (struct-info q))
  (struct-type-make-constructor x-structure-type))

(define-prototype quad
  [elems null]
  [type 'quad]
  [id #f]
  [size '(0 0)]
  [from-parent #false]
  [from 'ne]
  [to 'nw]
  [shift '(0 0)]
  [shift-elems '(0 0)]
  [origin '(0 0)]
  [printable default-printable]
  [draw-start void]
  [draw default-draw]
  [draw-end void])


;; todo: convert immutable hashes to mutable on input?


(define-syntax (define-quad stx)
  (syntax-case stx ()
    [(_ ID SUPER)
     #'(define-prototype ID SUPER)]))
  
(define q
  (make-keyword-procedure
   (λ (kws kwargs . args)
     (match args
       [(list (== #false) elems ...) (make-quad #:elems elems)]
       #;[(list (? hash? attrs) elems ...) (make-quad #:attrs attrs #:elems elems)]
       #;[(list (? dict? assocs) elems ...) assocs (make-quad #:attrs (make-hasheq assocs) #:elems elems)]
       [(list elems ..1) (make-quad #:elems elems)]
       ;; all cases end up below
       [null (keyword-apply make-quad kws kwargs args)]))))

(module+ test
  (require racket/port)
  (define q1 (q #f #\H #\e #\l #\o))
  (define q2 (q #f #\H #\e #\l #\o))
  (define q3 (q #f #\H #\e #\l))
  (check-true (equal? q1 q1))
  (check-true (equal? q1 q2))
  (check-false (equal? q1 q3))
  (define q4 (quad-copy q1
                        [draw (λ (q surface) (display "foo" surface))]))
  (check-equal? (with-output-to-string (λ () (draw q4))) "foo"))
