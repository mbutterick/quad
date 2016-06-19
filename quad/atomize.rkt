#lang quad/dev
(require racket/vector)
(provide (all-defined-out))

(define (atomize x)
  (apply vector-immutable
   (flatten
   (let loop ([x x][loop-attrs default-attrs])
     (cond
       [(symbol? x) ($shim (make-attrs) 0 x)]
       [(string? x)
        (for/list ([c (in-string x)])
                  (cons ($shim (make-attrs) 0 0)
                        (case c
                          [(#\space #\newline #\return) ($white loop-attrs 0 c)]
                          [else ($black loop-attrs 0 c)])))]
       [else
        (map (λ(xi) (loop xi ((quad-attrs x) . override-with . loop-attrs))) (quad-val x))])))))

(module+ test
  (require rackunit)
  (atomize (quad (make-attrs #:size 10 #:font "Eq") "ba" (line-break) "r" (quad (make-attrs #:size 8) "zam") "q\tux"))
  (atomize (quad #f "Meg is " (line-break) "\nan ally.")))
