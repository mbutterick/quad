#lang quad/dev
(require "quads.rkt")

(define (split-quad x)
  (flatten
   (let loop ([x x][attrs default-attrs])
     (cond
       [(char? x) (quad attrs x)]
       [(string? x) (map (λ(xi) (loop xi attrs)) (string->list x))]
       [else
        (define x-attrs (quad-attrs x))
        (for ([i (in-range (vector-length attrs))])
             (unless (vector-ref x-attrs i)
               (vector-set! x-attrs i (vector-ref attrs i))))
        (map (λ(xi) (loop xi x-attrs)) (quad-list x))]))))

(split-quad (quad (make-quad-attrs #:size 10 #:font "Eq") "ba\nr" (quad (make-quad-attrs #:size 8) "zam") "q\tux"))