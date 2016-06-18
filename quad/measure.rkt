#lang quad/dev
(provide (all-defined-out))

(define (measure! q)
  (when (or ($black? q) ($white? q))
       (quad-posn-set! q 12)))