#lang quad/dev
(require racket/string)
(provide (all-defined-out))

(define (atomize x)
  (define empty-attrs (make-attrs))
  (apply
   vector-immutable
   (flatten
    (let loop ([x x][loop-attrs default-attrs])
      (cond
        [(symbol? x) ($hard empty-attrs x #f)]
        [(string? x)
         ;; consolidate consecutive whitespaces into single word space
         (for/list ([c (in-string x)]) 
                   (cons ($hard empty-attrs #f #f)
                         ;; todo: is it feasible to box or otherwise object-ize a char
                         ;; so that all the quads with that char share that object
                         ;; and thus the measurement can be shared too?
                         ;; (object would have to be packaged with other typographic specs)
                         ((if (or (char=? c #\space) (char=? c #\-)) $soft $black) loop-attrs #f c)))]
        [else
         (map (λ(xi) (loop xi ((quad-attrs x) . override-with . loop-attrs))) (quad-val x))])))))

(module+ test
  (require rackunit)
  (atomize (quad (make-attrs #:size 10 #:font "Eq") "ba" (line-break) "r" (quad (make-attrs #:size 8) "zam") "q\tux"))
  (atomize (quad #f "Meg is-an ally.")))
