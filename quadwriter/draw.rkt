#lang debug racket
(require "struct.rkt"
         "string.rkt"
         "debug.rkt"
         "attrs.rkt"
         quad/quad
         sugar/coerce
         quad/position
         pitfall)
(provide (all-defined-out))


(define q:draw (make-quad #:type draw-quad))


(define (draw-line q doc)
  (define x1 (quad-ref q :x1 0))
  (define y1 (quad-ref q :y1 0))
  (move-to doc x1 y1)
  (line-to doc (quad-ref q :x2 x1) (quad-ref q :y2 y1))
  (line-width doc (quad-ref q :stroke 1))
  (stroke doc (quad-ref q :color "black")))

(define (draw-text q doc)
  (move-to doc 0 0)
  (q:string-draw q doc
                 #:origin (pt (quad-ref q :x 0) (quad-ref q :y 0))
                 #:text (quad-ref q :string "")))


(define (convert-draw-quad q)
  (cond
    [(memq (quad-tag q) '(line text))
     (quad-copy draw-quad q:draw
                [from (->symbol (quad-ref q :anchor-from (quad-from q:draw)))]
                [from-parent (match (quad-ref q :anchor-from-parent (quad-from-parent q:draw))
                               [#false #false]
                               [str (->symbol str)])]
                [to (->symbol (quad-ref q :anchor-to (quad-to q:draw)))]
                [elems (quad-elems q)]
                [tag (quad-tag q)]
                [attrs (quad-attrs q)]
                [size (match (quad-tag q)
                        [(== 'text eq?) (make-size-promise-for-string q (quad-ref q :string ""))]
                        [(== 'line eq?) (pt (abs (- (quad-ref q :x1) (quad-ref q :x2)))
                                            (abs (- (quad-ref q :y1) (quad-ref q :y2))))]
                        [_ (pt (quad-ref q :width 0) (quad-ref q :height 0))])]
                [draw-end (λ (q doc)
                            (when (draw-debug-draw?)
                              (draw-debug q doc "red" "red")))]                
                [draw (let ([draw-proc (match (quad-tag q)
                                         [(== 'line eq?) draw-line]
                                         [(== 'text eq?) draw-text])])
                        (λ (q doc)
                          (save doc)
                          (apply translate doc (quad-origin q))
                          (draw-proc q doc)
                          (restore doc)))])]
    [else #false]))