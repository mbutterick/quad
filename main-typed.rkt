#lang typed/racket/base
(require racket/list)
(require "quads-typed.rkt" "utils-typed.rkt" "wrap-typed.rkt" "measure-typed.rkt" "world-typed.rkt" "logger-typed.rkt")

(define-type Block-Type (Listof Quad))
(define-type Multicolumn-Type (Listof Block-Type))
(define-type Multipage-Type (Listof Multicolumn-Type))

(define/typed (cons-reverse xs ys)
  (All (A B) ((Listof A) (Listof B) -> (Pairof (Listof A) (Listof B))))
  ((inst cons (Listof A) (Listof B)) ((inst reverse A) xs) ys))

(provide input->nested-blocks)
(define/typed (input->nested-blocks i)
  (Quad . -> . (Listof Multipage-Type))
  (define-values (mps mcs bs b)
    (for/fold ([multipages : (Listof Multipage-Type) empty]
               [multicolumns : (Listof Multicolumn-Type) empty]
               [blocks : (Listof Block-Type) empty]
               [block-acc : Block-Type empty])
              ([q (in-list (split-quad i))])
      (cond
        [(page-break? q) (values (cons-reverse (cons-reverse (cons-reverse block-acc blocks) multicolumns) multipages) empty empty empty)]
        [(column-break? q) (values multipages (cons-reverse (cons-reverse block-acc blocks) multicolumns) empty empty)]
        [(block-break? q) (values multipages multicolumns (cons-reverse block-acc blocks) empty)]
        [else (values multipages multicolumns blocks (cons q block-acc))])))
  (reverse (cons-reverse (cons-reverse ((inst cons-reverse Quad Block-Type) b bs) mcs) mps)))
