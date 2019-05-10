#lang br
(require quad pict)
(provide (all-defined-out))

(define (quad->pict q)
  (match-define (list xmin ymin xmax ymax) (bounding-box q))
  (define scaling-factor 3)
  (define stroke-width 0.5)
  (define margin 3)
  (unsafe-dc
   (λ (dc dx dy)
     (send dc scale scaling-factor scaling-factor)
     (send dc translate (+ (- xmin) stroke-width margin) (+ (- ymin) stroke-width margin))
     (let loop ([q q][idx 0])
       ;; outer edge
       (send dc set-pen "slategray" stroke-width 'solid)
       (send dc set-brush "white" 'solid)
       (define args (append (quad-origin q) (quad-size q)))
       (send dc draw-rectangle . args)
       ;; join pt
       (send dc set-pen "slategray" 0 'solid)
       (send dc set-brush (if (zero? idx) "black" "red") 'solid)
       (define pt-args (append (map sub1 (to-point q)) (list 2 2)))
       (send dc draw-rectangle . pt-args)
       (map (λ (qe) (loop qe (add1 idx))) (quad-elems q))))
   (* scaling-factor (+ (- xmax xmin) (* stroke-width 2) (* margin 2)))
   (* scaling-factor (+ (- ymax ymin) (* stroke-width 2) (* margin 2)))))
