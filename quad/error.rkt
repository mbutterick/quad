#lang racket/base
(provide (all-defined-out))

(struct exn:quad-overflow exn:fail ())
(define (raise-overflow-error)
  (raise
   (exn:quad-overflow
    "overflow error: No breakpoint available. Increase line width"
    (current-continuation-marks))))