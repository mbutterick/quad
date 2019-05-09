#lang br
(require quad racket/draw pict pict/convert)
(provide (all-defined-out))
(verbose-quad-printing? #t)

(define (quad->pict q)
  (match-define (list bbox-x bbox-y) (bounding-box q))
  (define scaling-factor 3)
  (define stroke-width 0.5)
  (unsafe-dc
   (λ (dc dx dy)
     (send dc scale scaling-factor scaling-factor)
     (send dc translate stroke-width stroke-width)
     (send dc set-pen
          (new pen% [width stroke-width] [color "slategray"]))
     (let loop ([q q])
       (define args (append (quad-origin q) (quad-size q)))
       (send dc draw-rectangle . args)
       (map loop (quad-elems q))))
   (* scaling-factor (+ bbox-x (* stroke-width 2)))
   (* scaling-factor (+ bbox-y (* stroke-width 2)))))

(struct quad-pict quad ()
  #:property prop:pict-convertible quad->pict)

(define make-quad (make-quad-constructor quad-pict))

(define q1 (make-quad #:size '(20 20)))
(define q2 (make-quad #:size '(15 15)))
(define p (make-quad #:size '(35 35)
                     #:elems (list q1)))

;; todo: make these equal
(bounding-box (position p))
(bounding-box p)
