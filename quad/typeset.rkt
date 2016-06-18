#lang quad/dev
(provide (all-defined-out))
(require "measure.rkt")

(define line-start-k #f)

(define (typeset qs)
  (for-each measure! qs)
  (for/fold ([line-pos 0])
            ([q (in-list qs)])
    (unless line-start-k
      (let/cc here-k (set! line-start-k here-k)))
    (define next-line-pos (+ line-pos (quad-posn q)))
    (if (and (> next-line-pos 84) ($white? q))
        (begin (quad-posn-set! q 'break-line) 0)
        next-line-pos))
  qs)

(module+ test
  (require "atomize.rkt")
  (define qs (atomize (quad #f "Meg is an ally.")))
  (typeset qs))