#lang debug racket/base
(require (for-syntax racket/base racket/syntax)
         racket/struct racket/format racket/list racket/string racket/promise racket/dict racket/match)
(provide (all-defined-out))
(module+ test (require rackunit))

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

(define (quad=? q1 q2 recur?)
  (and
   ;; exclude attrs from initial comparison
   (for/and ([getter (in-list (list quad-elems quad-size quad-in quad-out quad-inner
                                    quad-offset quad-origin quad-printable
                                    quad-draw-start quad-draw-end quad-draw))])
            (equal? (getter q1) (getter q2)))
   ;; and compare them key-by-key
   (hashes-equal? (quad-attrs q1) (quad-attrs q2))))

(define verbose-quad-printing? (make-parameter #f))

(struct quad (attrs ; key-value pairs, arbitrary
              elems ; subquads or text
              ;; size is a two-dim pt
              size ; outer size of quad for layout (though not necessarily the bounding box for drawing)
              ;; in, out, inner are phrased in terms of cardinal position
              in ; alignment point matched to previous quad
              out ; alignment point matched to next quad
              inner ; alignment point for elems (might be different from in/out)
              ;; offset, origin are two-dim pts
              offset ; relocation of pen before quad is drawn
              origin ; reference point for all subsequent drawing ops in the quad. Calculated, not set directly
              printable ; whether the quad will print
              draw-start ; func called at the beginning of every draw event (for setup ops)
              draw ; func called in the middle of every daw event
              draw-end ; func called at the end of every draw event (for teardown ops)
              )
  #:transparent
  #:property prop:custom-write
  (λ (q p w?) (display
               (format "<~a~a~a>"
                       (object-name q)
                       (if (verbose-quad-printing?)
                           (string-join (map ~v (flatten (hash->list (quad-attrs q))))
                                    " " #:before-first "(" #:after-last ")")
                           "")
                       (match (quad-elems q)
                         [(? pair?) (string-join (map ~v (quad-elems q)) " " #:before-first " ")]
                         [_ ""])) p))
  #:methods gen:equal+hash
  [(define equal-proc quad=?)
   (define (hash-proc h recur) (equal-hash-code h))
   (define (hash2-proc h recur) (equal-secondary-hash-code h))])

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
         #:draw-start [draw-start void]
         #:draw [draw default-draw]
         #:draw-end [draw-end void]
         . args)
  (unless (andmap (λ (x) (not (pair? x))) elems)
    (raise-argument-error 'make-quad "elements that are not lists" elems))
  (match args
    [(list (== #false) elems ...) (make-quad #:elems elems)]
    [(list (? hash? attrs) elems ...) (make-quad #:attrs attrs #:elems elems)]
    [(list (? dict? assocs) elems ...) assocs (make-quad #:attrs (make-hasheq assocs) #:elems elems)]
    [(list elems ..1) (make-quad #:elems elems)]
    ;; all cases end up below
    [null (type attrs
                elems
                size
                in
                out
                inner
                offset
                origin
                printable
                draw-start
                draw
                draw-end)]))

(define-syntax (define-quad stx)
  (syntax-case stx ()
    [(_ ID SUPER ARGS . REST)
     (with-syntax ([MAKE-ID (format-id #'ID "make-~a" (syntax-e #'ID))])
       #'(begin
           (struct ID SUPER ARGS . REST)
           (define MAKE-ID (make-keyword-procedure (λ (kws kw-args . rest)
                                                     (keyword-apply make-quad #:type ID kws kw-args rest))))))]))
  
(define q make-quad)

(module+ test
  (require racket/port)
  (define q1 (q #f #\H #\e #\l #\o))
  (define q2 (q #f #\H #\e #\l #\o))
  (define q3 (q #f #\H #\e #\l))
  (check-true (equal? q1 q1))
  (check-true (equal? q1 q2))
  (check-false (equal? q1 q3))
  (define q4 (struct-copy quad q1
                          [draw (λ (q surface) (display "foo" surface))]))
  (check-equal? (with-output-to-string (λ () (draw q4))) "foo"))
