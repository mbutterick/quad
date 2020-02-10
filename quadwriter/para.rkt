#lang debug racket
(require "attrs.rkt"
         "break.rkt"
         "string.rkt"
         "struct.rkt"
         quad/quad
         quad/position)
(provide (all-defined-out))

(define (insert-first-line-indents qs-in)
  ;; first line indents are quads inserted at the beginning of a paragraph
  ;; (that is, just after a paragraph break)
  ;; they need to be installed before line wrap
  ;; to be compatible with first-fit and best-fit.

  ;; stick a pbr on the front if there isn't one already
  ;; because of the "lookahead" style of iteration
  (define qs (match qs-in
               [(cons (? para-break-quad?) _) qs-in]
               [_ (cons q:page-break qs-in)]))
  (apply append
         (for/list ([q (in-list qs)]
                    [next-q (in-list (cdr qs))])
                   (match (and (para-break-quad? q)  (quad-ref next-q :first-line-indent 0))
                     [(or #false 0) (list next-q)]
                     [indent-val (list (make-quad #:from 'bo
                                                  #:to 'bi
                                                  #:draw-end q:string-draw-end
                                                  #:type first-line-indent-quad
                                                  #:attrs (quad-attrs next-q)
                                                  #:size (pt indent-val 10)) next-q)]))))