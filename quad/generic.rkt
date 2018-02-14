#lang debug racket/base
(require racket/generic)
(provide (all-defined-out))

(define-generics quad
  (elems quad)
  (attrs quad)
  (entrance-point quad)
  (exit-point quad)
  (inner-point quad)
  (size quad)
  (draw quad))