#lang br
(require quad racket/draw racket/gui)
(verbose-quad-printing? #t)

(define q1 (make-quad #:size '(20 20)))
(define q2 (make-quad
            #:from 'bo
            #:to 'bi
            #:size '(15 15)))

  (define target (make-bitmap 800 150)) ; A 30x30 bitmap
(define (go qs)
  (define dc (new bitmap-dc% [bitmap target]))
  (send dc scale 3 3)
  (send dc translate 3 3)

  (for ([q qs])
    (define args (append (quad-origin q) (quad-size q)))
    (send dc draw-rectangle . args)) ; 30 pixels wide and 10 pixels high

  (make-object image-snip% target))

(go (position (list q1 q2)))
