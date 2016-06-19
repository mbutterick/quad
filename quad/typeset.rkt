#lang quad/dev
(provide (all-defined-out))
(require "measure.rkt")

(define last-bp-k #f)
(define line-measure 60)

(define (typeset qs)
  (for/fold ([line-pos 0])
            ([q (in-vector qs)])
    (measure! q)
    (when (and ($white? q) (let/cc bp-k (set! last-bp-k bp-k) #f))
      (quad-posn-set! q 'break-line))
    (cond
      [(eq? 'break-line (quad-posn q)) 0]
      [else (define next-line-pos (+ line-pos (quad-posn q)))
            (if (> next-line-pos line-measure)
                (last-bp-k #t)
                next-line-pos)]))
  qs)

(module+ test
  (require "atomize.rkt")
  (time (typeset (atomize (quad #f "Meg is an ally.")))))