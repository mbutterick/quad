#lang quad/dev
(require racket/vector)
(provide (all-defined-out))

(define (atomize x)
  (flatten
   (let loop ([x x][loop-attrs default-attrs])
     (cond
       [(symbol? x) ($shim (attrs #:posn 0) x)]
       [(string? x)
        (for/list ([c (in-string x)])
                  (cons ($shim (attrs #:posn 0) 0)
                        (case c
                          [(#\space #\newline #\return) ($white (vector-copy loop-attrs) c)]
                          [else ($black (vector-copy loop-attrs) c)])))]
       [else
        (map (λ(xi) (loop xi ((quad-attrs x) . override-with . loop-attrs))) (quad-val x))]))))

(module+ test
  (require rackunit)
  (atomize (quad (attrs #:size 10 #:font "Eq") "ba" (line-break) "r" (quad (attrs #:size 8) "zam") "q\tux"))
  (atomize (quad #f "Meg is " (line-break) "\nan ally.")))
