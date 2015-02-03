#lang typed/racket/base
(require sugar/list)
(require (for-syntax racket/syntax racket/base) racket/string racket/list sugar/debug racket/bool hyphenate racket/function math/flonum)
(require "quads-typed.rkt" "world-typed.rkt" "measure-typed.rkt")


;; predicate for use below
(: list-of-mergeable-attrs? (Any . -> . Boolean))
(define (list-of-mergeable-attrs? xs)
  (and (list? xs) (andmap (λ(x) (or (quad? x) (quad-attrs? x) (hashable-list? x))) xs)))

;; faster than (listof pair?)
(: pairs? (Any . -> . Boolean))
(define (pairs? x) (and (list? x) (andmap pair? x)))

;; push together multiple attr sources into one list of pairs.
;; mostly a helper function for the two attr functions below.
(provide join-attrs)
(: join-attrs ((Listof (U Quad QuadAttrs HashableList)) . -> . (Listof QuadAttrPair)))
(define (join-attrs quads-or-attrs-or-lists)
  ((inst append-map QuadAttrPair QuadAttrs) (inst hash->list QuadAttrKey QuadAttrValue) (map (λ(x)
                                                   (cond
                                                     [(quad? x) (quad-attrs x)]
                                                     [(quad-attrs? x) (cast x QuadAttrs)]
                                                     [(hashable-list? x) (quadattrs (cast x (Listof Any)))]
                                                     [else (cast hash QuadAttrs)])) quads-or-attrs-or-lists)))


