#lang quad/dev
(require racket/string hyphenate)
(provide (all-defined-out))

(define (atomize x)
  (apply
   vector-immutable
   (flatten
    (list
     (let loop ([x x][loop-attrs default-attrs])
       (cond
         [($shim? x) x]
         [(string? x)
          ;; consolidate consecutive whitespaces into single word space
          ;; todo: hyphenate here? then they are in the quad stream
          (for/list ([c (in-string x)]) 
                    (cons
                     ;; installing loop attrs allows us to recognize contiguous runs later
                     ($shim loop-attrs #f #f) 
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
     ($eof (make-empty-attrs) #f #f))))) ; add eof so any in-vector loop consumes all the input vals

(define (merge-runs xs)
  ; combine quads with same attrs into sublists
  (cond
    [(empty? xs) empty]
    [else
     (define target (car xs))
     (define-values (matches rest)
       (splitf-at (cdr xs) (λ(x) (eq? (quad-attrs target) (quad-attrs x)))))
     (list* (cons target matches) (merge-runs rest))]))

(module+ test
  (require rackunit)
  #;(atomize (quad (make-attrs #:size 10 #:font "Eq") "ba" (line-break) "r" (quad (make-attrs #:size 8) "zam") "q\tux"))
  (define qs (atomize (quad #f "A" (page-break) "B")))
  qs)
