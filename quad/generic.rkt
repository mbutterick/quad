#lang debug racket/base
(require racket/generic)
(provide (all-defined-out))

(define-generics quad
  (start quad)
  (end quad)
  (inner quad)

  (size quad [signal])
  (offset quad [signal])

  (origin quad)
  (set-origin! quad where)

  (draw quad [surface])

  (elems quad)
  (attrs quad))