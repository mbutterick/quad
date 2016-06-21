#lang quad/dev
(require racket/string hyphenate)
(provide (all-defined-out))

(define (atomize x)
  (define empty-attrs (make-attrs))
  (apply
   vector-immutable
   (flatten
    (cons
     (let loop ([x x][loop-attrs default-attrs])
       (cond
         [($shim? x) x]
         [(string? x)
          ;; consolidate consecutive whitespaces into single word space
          ;; todo: hyphenate here? then they are in the quad stream
          (for/list ([c (in-string x)]) 
                    (cons ($shim empty-attrs #f #f)
                          ;; todo: is it feasible to box or otherwise object-ize a char
                          ;; so that all the quads with that char share that object
                          ;; and thus the measurement can be shared too?
                          ;; (object would have to be packaged with other typographic specs)
                          ((casev c
                                  [(#\space) $space]
                                  [(#\-) $hyphen]
                                  [(#\u00AD) $shy]
                                  [else $black]) loop-attrs #f c)))]
         [else
          (map (λ(xi) (loop xi ((quad-attrs x) . override-with . loop-attrs))) (quad-val x))]))
     ($eof empty-attrs #f #f))))) ; add eof so any in-vector loop consumes all the input vals

(module+ test
  (require rackunit)
  (atomize (quad (make-attrs #:size 10 #:font "Eq") "ba" (line-break) "r" (quad (make-attrs #:size 8) "zam") "q\tux"))
  (atomize (quad #f "snowman")))
