#lang quad/dev
(require brag/support)
(provide (all-defined-out))

(define (tokenize x)
  (flatten
   (let loop ([x x][attrs default-attrs])
     (cond
       [(symbol? x) (token x #f)]
       [(string? x)
        (for/list ([c (in-string x)])
                  (case c
                    [(#\space #\newline #\return) (token 'WHITESPACE (quad attrs c))]
                    [else (token 'QUAD (quad attrs c))]))]
       [else
        (map (λ(xi) (loop xi ((quad-attrs x) . override-with . attrs))) (quad-list x))]))))

(module+ test
  (require rackunit "parse.rkt")
  (tokenize (quad (attrs #:size 10 #:font "Eq") "ba" (line-break) "r" (quad (attrs #:size 8) "zam") "q\tux"))
  (syntax->datum (parse (tokenize (quad #f "Meg is " (line-break) "\nan ally.")))))
