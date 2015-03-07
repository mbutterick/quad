#lang typed/racket/base
(require racket/list math/flonum)
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

(provide merge-adjacent-within)
(define/typed (merge-adjacent-within q)
  (Quad . -> . Quad)
  (quad (quad-name q) (quad-attrs q) (join-quads (cast (quad-list q) (Listof Quad)))))

(provide hyphenate-quad-except-last-word)
(define/typed (hyphenate-quad-except-last-word q)
  (Quad . -> . Quad)
  (log-quad-debug "last word will not be hyphenated")
  (define-values (first-quads last-quad) ((inst split-last QuadListItem) (quad-list q)))
  (quad (quad-name q) (quad-attrs q) (snoc ((inst map QuadListItem QuadListItem) hyphenate-quad first-quads) last-quad)))

(provide average-looseness)
(define/typed (average-looseness lines)
  ((Listof Quad) . -> . Flonum)
  (if (<= (length lines) 1)
      0.0
      (let ([lines-to-measure (drop-right lines 1)]) ; exclude last line from looseness calculation
        (round-float (/ (foldl fl+ 0.0 ((inst map Flonum Quad) (λ(line) (cast (quad-attr-ref line world:line-looseness-key 0.0) Flonum)) lines-to-measure)) (- (fl (length lines)) 1.0))))))
