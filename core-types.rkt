#lang typed/racket/base
(require (for-syntax typed/racket/base racket/syntax) (only-in typed/racket/draw Font-Weight Font-Style) typed/sugar/define)
(provide (all-defined-out) (all-from-out typed/racket/draw))

(define-type+predicate QuadName Symbol)
(define-type+predicate QuadAttrKey Symbol)
(define-type+predicate QuadAttrValue (U Float Index String Symbol Boolean Quad QuadAttrs QuadList Integer))

;; QuadAttr could be a list, but that would take twice as many cons cells.
;; try the economical approach.
(define-type+predicate QuadAttr (Pairof QuadAttrKey QuadAttrValue))
(define-type+predicate QuadAttrs (Listof QuadAttr))
(define quad-attrs? QuadAttrs?)
#|
;; mutually recursive version
(define-type HashableListKey (U Null (Pairof QuadAttrKey HashableListValue)))
(define-type HashableListValue (Pairof QuadAttrValue HashableListKey))
(define-type+predicate HashableList  HashableListKey)
|#
(define-type+predicate HashableList  (Rec duo (U Null (List* QuadAttrKey Any duo))))
(define-type JoinableType (U Quad QuadAttrs HashableList)) 


(define-type+predicate QuadListItem (U String Quad))
(define-type+predicate QuadList (Listof QuadListItem))
(define-type+predicate GroupQuadListItem Quad)
(define-type+predicate GroupQuadList (Listof GroupQuadListItem))
(define-type (Treeof A) (Rec as (U A (Listof as))))


;; funky implementation
(define-type+predicate Quad (List* QuadName QuadAttrs QuadList))
(define-type+predicate GroupQuad (List* QuadName QuadAttrs GroupQuadList))
(define-predicate quad? Quad)

;; quad wants to be generic
;; if it's a function, it must impose a type on its output value
;; whereas if it's syntax, it can avoid demanding or imposing any typing
(define-syntax-rule (quad name attrs items)
  (list* name attrs items))

(define-type+predicate QuadSet (List QuadName QuadAttrs (Listof Quad)))


(define-type+predicate Font-Name String)
(define-type+predicate Font-Size Positive-Flonum)
(define-predicate Font-Weight? Font-Weight)
(define-predicate Font-Style? Font-Style)

(define-predicate Index? Index)

(define-type+predicate Breakpoint Nonnegative-Integer)
