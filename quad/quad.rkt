#lang debug racket/base
(require racket/struct racket/dict racket/match)
(provide (all-defined-out))
(module+ test (require rackunit))

(define (printable? q [signal #f])
  ((quad-printable q) q signal))

(define (draw q [surface #f])
  ((quad-draw q) q surface))

(define (hashes-equal? h1 h2)
  (and (= (length (hash-keys h1)) (length (hash-keys h2)))
       (for/and ([(k v) (in-hash h1)])
                (and (hash-has-key? h2 k) (equal? (hash-ref h2 k) v)))))

(define (quad=? q1 q2 recur?)
  (and
   ;; exclude attrs from initial comparison
   (andmap equal? (cdr (struct->list q1)) (cdr (struct->list q2)))
   ;; and compare them key-by-key
   (hashes-equal? (quad-attrs q1) (quad-attrs q2))))

(struct quad (attrs
              elems
              in
              out
              inner
              offset
              origin
              size
              printable
              pre-draw
              post-draw
              draw) #:transparent #:mutable
  #:methods gen:equal+hash
  [(define equal-proc quad=?)
   (define (hash-proc h recur) (equal-hash-code h))
   (define (hash2-proc h recur) (equal-secondary-hash-code h))])

(define (default-printable [sig #f]) #f)

;; todo: convert immutable hashes to mutable on input?
(define make-quad
  (match-lambda*
    [(list (== #false) elems ...) elems (apply make-quad (make-hasheq) elems)]
    [(list (? hash? attrs) elems ...)
  ;; why 'nw and 'ne as defaults for in and out points:
  ;; if size is '(0 0), 'nw and 'ne are the same point,
  ;; and everything piles up at the origin
  ;; if size is otherwise, the items don't pile up (but rather lay out in a row)
  (define in 'nw)
  (define out 'ne)
  (define inner #f)
  (define offset '(0 0))
  (define origin '(0 0))
  (define size '(0 0))
  (define printable default-printable)
  (define pre-draw void)
  (define post-draw void)
  (define (draw q surface)
    ((quad-pre-draw q) q surface)
    (for-each (λ (qi) ((quad-draw qi) qi surface)) (quad-elems q))
    ((quad-post-draw q) q surface))
  (quad (or attrs (make-hasheq))
        elems
        in
        out
        inner
        offset
        origin
        size
        printable
        pre-draw
        post-draw
        draw)]
    [(list (? dict? assocs) elems ...) assocs (apply make-quad (make-hasheq assocs) elems)]
    [(list elems ...) (apply make-quad #f elems)]))

(define q make-quad)

(module+ test
  (define q1 (make-quad #f '(#\H #\e #\l #\o)))
  (define q2 (make-quad #f '(#\H #\e #\l #\o)))
  (define q3 (make-quad #f '(#\H #\e #\l)))
  (check-true (equal? q1 q1))
  (check-true (equal? q1 q2))
  (check-false (equal? q1 q3))
  (set-quad-draw! q1 (λ (q surface) "foo"))
  (check-equal? (draw q1) "foo"))
