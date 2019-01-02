#lang debug racket/base
(require racket/generic)
(provide (all-defined-out))

(define-generics quad
  #|
(start quad) ; called before draw (?)
  (end quad) ; called after draw (?)
  (wrap quad) ; called if quad is not clipping (?)
|#
  
  (in quad) ; returns inbound connection point
  (out quad) ; returns outbound connection point
  (inner quad) ; returns inner connection point

  (printable? quad [signal]) ; returns whether quad is printable (under `signal`)
  (size quad) ; returns outer size of quad (two dimensional)
  (offset quad) ; returns top left adjustment of drawing (may or may not clip to quad boundary)

  (origin quad) ; returns point where quad starts
  (set-origin! quad where) ; changes point where quad starts

  (draw quad [surface]) ; draws quad (imperatively)

  (elems quad) ; returns list of subquads
  (attrs quad)) ; returns list of attributes