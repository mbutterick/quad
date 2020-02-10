#lang debug racket
(require "param.rkt"
         pitfall
         quad/position
         quad/quad)
(provide (all-defined-out))


(define (draw-debug q doc [fill-color "#f99"] [stroke-color "#fcc"] . _)
  (define stroke-width 0.5)
  (when (draw-debug?)
    (save doc)
    ;; draw layout box
    (line-width doc stroke-width)
    ; subtracting stroke-width keeps adjacent boxes from overlapping
    (save doc)
    (apply rect doc (append (pt+ (quad-origin q)) (map (λ (x) (- x stroke-width)) (size q))))
    (clip doc)
    (define pt (to-point q))
    (circle doc (pt-x pt) (pt-y pt) (+ 3 stroke-width))
    (fill doc fill-color)
    (restore doc)
    (apply rect doc (append (pt+ (quad-origin q)) (map (λ (x) (- x stroke-width)) (size q))))
    (stroke doc stroke-color)
    (restore doc)))