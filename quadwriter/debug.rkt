#lang debug racket
(require pitfall
         quad/position
         quad/quad)
(provide (all-defined-out))

(define-for-syntax debug-mode #false)

(define-syntax (go stx)
  (datum->syntax stx
                 (cond
                   [debug-mode
                    '(begin
                       (define draw-debug? (make-parameter #true))
                       (define draw-debug-line? (make-parameter #true))
                       (define draw-debug-block? (make-parameter #false))
                       (define draw-debug-string? (make-parameter #true))
                       (define draw-debug-image? (make-parameter #false))
                       (define draw-debug-draw? (make-parameter #false))

                       (define debug-page-width (make-parameter 400))
                       (define debug-page-height (make-parameter 400))
                       (define debug-x-margin (make-parameter 50))
                       (define debug-y-margin (make-parameter 50))
                       (define debug-column-count (make-parameter 1))
                       (define debug-column-gap (make-parameter 36)))]
                   [else
                    '(begin
                       (define draw-debug? (make-parameter #false))
                       (define draw-debug-line? (make-parameter #true))
                       (define draw-debug-block? (make-parameter #true))
                       (define draw-debug-string? (make-parameter #true))
                       (define draw-debug-image? (make-parameter #true))
                       (define draw-debug-draw? (make-parameter #true))

                       (define debug-page-width (make-parameter #f))
                       (define debug-page-height (make-parameter #f))
                       (define debug-x-margin (make-parameter #f))
                       (define debug-y-margin (make-parameter #f))
                       (define debug-column-count (make-parameter #f))
                       (define debug-column-gap (make-parameter #f)))])))


(go)


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
