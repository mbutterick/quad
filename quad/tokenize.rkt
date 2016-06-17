#lang quad/dev
(require brag/support)
(provide (all-defined-out))

(define (tokenize x)
  (flatten
   (let loop ([x x][attrs default-attrs])
     (cond
       [(symbol? x) (token x #f)]
       [(string? x)
         (map (λ(xi) (token 'QUAD (quad attrs xi))) (string->list x))]
       [else
         (map (λ(xi) (loop xi ((quad-attrs x) . override-with . attrs))) (quad-list x))]))))

(module+ test
  (require rackunit)
  (tokenize (quad (attrs #:size 10 #:font "Eq") "ba" (line-break) "r" (quad (attrs #:size 8) "zam") "q\tux"))
  (tokenize (quad #f "Meg is" (block-break) " an ally.")))
