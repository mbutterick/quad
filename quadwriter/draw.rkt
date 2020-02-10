#lang debug racket
(require "struct.rkt"
         "string.rkt"
         "debug.rkt"
         "param.rkt"
         "attrs.rkt"
         quad/quad
         quad/position
         pitfall)
(provide (all-defined-out))

(define (convert-draw-quad q)
  (quad-copy draw-quad q:draw
             [attrs (quad-attrs q)]
             [size (pt (quad-ref q :width 0) (quad-ref q :height 0))]))

(define q:draw (make-quad #:type draw-quad
                          #:from 'bo
                          #:to 'bi
                          #:draw (λ (q doc)
                                   (save doc)
                                   (apply translate doc (if (equal? (quad-ref q :position) "absolute")
                                                            (list 0 0)
                                                            (quad-origin q)))
                                   (match (quad-ref q :draw)
                                     ["line"
                                      (define x0 (quad-ref q :x 0))
                                      (define y0 (quad-ref q :y 0))
                                      (move-to doc x0 y0)
                                      (line-to doc (quad-ref q :x2 x0) (quad-ref q :y2 y0))
                                      (line-width doc (quad-ref q :stroke 1))
                                      (stroke doc (quad-ref q :color "black"))]
                                     ["text" (move-to doc 0 0)
                                             (q:string-draw q doc
                                                            #:origin (pt (quad-ref q :x 0) (quad-ref q :y 0))
                                                            #:text (quad-ref q :text))]
                                     [_ (void)])
                                   (restore doc))))