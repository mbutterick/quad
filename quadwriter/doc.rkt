#lang debug racket
(require "struct.rkt"
         quad/quad
         pitfall)
(provide (all-defined-out))

(define q:doc (make-quad
               #:type doc-quad
               #:draw-start (λ (q doc) (start-doc doc))
               #:draw-end (λ (q doc) (end-doc doc))))
