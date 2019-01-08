#lang debug racket/base
(require racket/struct racket/format racket/list racket/string racket/promise racket/dict racket/match)
(provide (all-defined-out))
(module+ test (require rackunit))

(define (size q)
  (match (quad-size q)
    [(? procedure? proc) (proc q)]
    [(? promise? prom) (force prom)]
    [val val]))

(define (printable? q [signal #f])
  (match (quad-printable q)
    [(? procedure? proc) (proc q signal)]
    [val val]))

(define (draw q [surface #f])
  ((quad-draw q) q surface))

(define (hashes-equal? h1 h2)
  (and (= (length (hash-keys h1)) (length (hash-keys h2)))
       (for/and ([(k v) (in-hash h1)])
         (and (hash-has-key? h2 k) (equal? (hash-ref h2 k) v)))))

(define (quad=? q1 q2 recur?)
  (and
   ;; exclude attrs from initial comparison
   (for/and ([getter (in-list (list quad-elems quad-size quad-in quad-out quad-inner
                                    quad-offset quad-origin quad-printable
                                    quad-pre-draw quad-post-draw quad-draw))])
     (equal? (getter q1) (getter q2)))
   ;; and compare them key-by-key
   (hashes-equal? (quad-attrs q1) (quad-attrs q2))))

(struct quad (attrs
              elems
              size
              in
              out
              inner
              offset
              origin
              printable
              pre-draw
              post-draw
              draw)
  #:property prop:custom-write
  (λ (v p w?) (display
               (format "<quad ~a~a>"
                       (string-join (map ~v (flatten (hash->list (quad-attrs v))))
                                    " " #:before-first "(" #:after-last ")")
                       (string-join (map ~v (quad-elems v)) " " #:before-first " ")) p))
  #:methods gen:equal+hash
  [(define equal-proc quad=?)
   (define (hash-proc h recur) (equal-hash-code h))
   (define (hash2-proc h recur) (equal-secondary-hash-code h))])

(define (default-printable q [sig #f]) #f)

(define (default-draw q surface)
  ((quad-pre-draw q) q surface)
  (for-each (λ (qi) ((quad-draw qi) qi surface)) (quad-elems q))
  ((quad-post-draw q) q surface))

;; why 'nw and 'ne as defaults for in and out points:
;; if size is '(0 0), 'nw and 'ne are the same point,
;; and everything piles up at the origin
;; if size is otherwise, the items don't pile up (but rather lay out in a row)


;; todo: convert immutable hashes to mutable on input?
(define (make-quad
         #:type [type quad]
         #:attrs [attrs (make-hasheq)]
         #:elems [elems null]
         #:size [size '(0 0)]
         #:in [in 'nw]
         #:out [out 'ne]
         #:inner [inner #f]
         #:offset [offset '(0 0)]
         #:origin [origin '(0 0)]
         #:printable [printable default-printable]
         #:pre-draw [pre-draw void]
         #:post-draw [post-draw void]
         #:draw [draw default-draw]
         . args)
  (match args
    [(list (== #false) elems ...) (make-quad #:elems elems)]
    [(list (? hash? attrs) elems ...) (make-quad #:attrs attrs #:elems elems)]
    [(list (? dict? assocs) elems ...) assocs (make-quad #:attrs (make-hasheq assocs) #:elems elems)]
    [(list elems ..1) (make-quad #:elems elems)]
    [null (type attrs
                elems
                size
                in
                out
                inner
                offset
                origin
                printable
                pre-draw
                post-draw
                draw)]))
  
(define q make-quad)

(module+ test
  (define q1 (q #f #\H #\e #\l #\o))
  (define q2 (q #f #\H #\e #\l #\o))
  (define q3 (q #f #\H #\e #\l))
  (check-true (equal? q1 q1))
  (check-true (equal? q1 q2))
  (check-false (equal? q1 q3))
  (define q4 (struct-copy quad q1
                          [draw (λ (q surface) "foo")]))
  (check-equal? (draw q4) "foo"))
