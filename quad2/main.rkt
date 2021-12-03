#lang debug racket/base
(require racket/contract racket/function rackunit)

(struct $point (x y) #:transparent #:mutable)
(struct $size (width height) #:transparent #:mutable)
(struct $rect (origin size) #:transparent #:mutable)

(struct $quad (posn char) #:transparent #:mutable)

(define/contract (posn-add p0 p1)
  ($point? $size? . -> . $point?)
  ($point (+ ($point-x p0) ($size-width p1)) (+ ($point-y p0) ($size-height p1))))

(define/contract (size char)
  ($quad? . -> . $size?)
  ($size 1 1))

(define/contract (advance char)
  ($quad? . -> . $size?)
  ($size 1 0))

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

(define (min-x rect) ($point-x ($rect-origin rect)))
(define (width rect) ($size-width ($rect-size rect)))
(define (max-x rect) (+ (min-x rect) (width rect)))
(define (min-y rect) ($point-y ($rect-origin rect)))
(define (height rect) ($size-height ($rect-size rect)))
(define (max-y rect) (+ (min-y rect) (height rect)))

(define/contract (rect-contains-point? rect pt)
  ($rect? $point? . -> . boolean?)
  (and (<= (min-x rect) ($point-x pt) (max-x rect))
       (<= (min-y rect) ($point-y pt) (max-y rect))))

(define/contract (rect-contains-rect? outer inner)
  ($rect? $rect? . -> . boolean?)
  (and (rect-contains-point? outer ($rect-origin inner))
       (rect-contains-point? outer ($point (max-x inner) (max-y inner)))))
  
(define (has-position? q) (not (eq? ($quad-posn q) #false)))
(define-pass (layout qs)
  #:precondition (λ (qs) (andmap (λ (q) (not (has-position? q))) qs))
  #:postcondition (λ (qs) (andmap has-position? qs))
  (define frame ($rect ($point 0 0) ($size 5 30)))
  (define (quad-fits? q posn)
    (define q-size (size q))
    (define quad-rect ($rect posn q-size))
    (and (rect-contains-rect? frame quad-rect) posn))
  (for/fold ([posn ($point 0 0)]
             #:result qs)
            ([q (in-list qs)])
    (define first-posn-on-next-line ($point 0 (add1 ($point-y posn))))
    (define winning-posn (or (ormap (λ (posn) (quad-fits? q posn)) (list posn first-posn-on-next-line)) (error 'no-posn-that-fits)))
    (set-$quad-posn! q winning-posn)
    (posn-add winning-posn (advance q))))

(define compile (make-compiler layout))
(check-equal? (compile "Hello world")
              (list
               ($quad ($point 0 0) #\H)
               ($quad ($point 1 0) #\e)
               ($quad ($point 2 0) #\l)
               ($quad ($point 3 0) #\l)
               ($quad ($point 4 0) #\o)
               ($quad ($point 0 1) #\space)
               ($quad ($point 1 1) #\w)
               ($quad ($point 2 1) #\o)
               ($quad ($point 3 1) #\r)
               ($quad ($point 4 1) #\l)
               ($quad ($point 0 2) #\d)))