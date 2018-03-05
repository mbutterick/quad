#lang debug racket/base
(require racket/generic)
(provide (all-defined-out))

(define-generics quad
  (in quad)
  (out quad)
  (inner quad)

  (printable? quad [signal])
  (size quad)
  (offset quad)

  (origin quad)
  (set-origin! quad where)

  (draw quad [surface])

  (elems quad)
  (attrs quad))