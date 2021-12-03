#lang debug racket/base
(require racket/contract racket/function rackunit)

(struct $posn (x y) #:transparent)
(struct $quad (posn char) #:transparent #:mutable)

(define/contract (posn-add p0 p1)
  ($posn? $posn? . -> . $posn?)
  ($posn (+ ($posn-x p0) ($posn-x p1)) (+ ($posn-y p0) ($posn-y p1))))

(define/contract (size char)
  ($quad? . -> . $posn?)
  ($posn 1 1))

(define/contract (advance char)
  ($quad? . -> . $posn?)
  ($posn 1 0))

(define pass/c ((listof $quad?) . -> . (listof $quad?)))

(define/contract (quadify str)
  (string? . -> . (listof $quad?))
  (for/list ([c (in-string str)])
            ($quad #f c)))

(define/contract (make-compiler . passes)
  (() #:rest (listof pass/c)
      . ->* . (string? . -> . (listof $quad?)))
  (apply compose1 (reverse (cons quadify passes))))

(define-syntax-rule (define-pass (PASS-NAME ARG)
                      #:precondition PRECOND-PROC
                      #:postcondition POSTCOND-PROC
                      EXPRS ...)
  (define/contract (PASS-NAME ARG)
    pass/c
    (unless (PRECOND-PROC ARG)
      (error 'precondition-failed))
    (define res (let () EXPRS ...))
    (unless (POSTCOND-PROC res)
      (error 'postcondition-failed))
    res))

(define (has-position? q) (not (eq? ($quad-posn q) #false)))

(struct $rect (x y width height) #:transparent #:mutable)

(define/contract (rect-contains-rect? outer inner)
  ($rect? $rect? . -> . boolean?)
  (define (min-x rect) ($rect-x rect))
  (define (max-x rect) (+ (min-x rect) ($rect-width rect)))
  (define (min-y rect) ($rect-y rect))
  (define (max-y rect) (+ (min-y rect) ($rect-height rect)))
  (and (<= (min-x outer) (min-x inner) (max-x inner) (max-x outer))
       (<= (min-y outer) (min-y inner) (max-y inner) (max-y outer))))
  

(define-pass (layout qs)
  #:precondition (λ (qs) (andmap (λ (q) (not (has-position? q))) qs))
  #:postcondition (λ (qs) (andmap has-position? qs))
  (define frame ($rect 0 0 5 30))
  (define (quad-fits? q posn)
    (define q-size (size q))
    (define quad-rect ($rect ($posn-x posn) ($posn-y posn)
                                 ($posn-x q-size) ($posn-y q-size)))
    (and (rect-contains-rect? frame quad-rect) posn))
  (for/fold ([posn ($posn 0 0)]
             #:result qs)
            ([q (in-list qs)])
    (define first-posn-on-next-line ($posn 0 (add1 ($posn-y posn))))
    (define winning-posn (or (ormap (λ (posn) (quad-fits? q posn)) (list posn first-posn-on-next-line)) (error 'no-posn-that-fits)))
    (set-$quad-posn! q winning-posn)
    (posn-add winning-posn (advance q))))

(define compile (make-compiler layout))
(check-equal? (compile "Hello world")
              (list
               ($quad ($posn 0 0) #\H)
               ($quad ($posn 1 0) #\e)
               ($quad ($posn 2 0) #\l)
               ($quad ($posn 3 0) #\l)
               ($quad ($posn 4 0) #\o)
               ($quad ($posn 0 1) #\space)
               ($quad ($posn 1 1) #\w)
               ($quad ($posn 2 1) #\o)
               ($quad ($posn 3 1) #\r)
               ($quad ($posn 4 1) #\l)
               ($quad ($posn 0 2) #\d)))