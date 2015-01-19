#lang typed/racket/base
(require (for-syntax typed/racket/base racket/syntax))
(require/typed racket/list [flatten ((Listof QuadAttrPair) . -> . (Listof QuadAttrPair))]
               [empty? ((Listof Any) . -> . Boolean)]
               )

;; struct implementation

(define-type QuadAttrKey Symbol)
(define-type QuadAttrValue Any)
(define-type QuadAttrs (HashTable QuadAttrKey QuadAttrValue))
(define-type QuadList (Listof Quad))
(struct Quad ([attrs : QuadAttrs] [list : QuadList]) #:transparent
  #:property prop:sequence (λ(q) (Quad-list q)))

(define Quad-attr-ref
  (case-lambda
    [([q : Quad] [key : QuadAttrKey]) 
     (hash-ref (Quad-attrs q) key)]
    [([q : Quad] [key : QuadAttrKey] [default : QuadAttrValue]) 
     (hash-ref (Quad-attrs q) key (λ() default))]))


(define cannot-be-common-attrs '(width x y page))
(define attr-missing (gensym))
(define-type QuadAttrPair (Pairof QuadAttrKey QuadAttrValue))


(: gather-common-attrs ((Listof Quad) . -> . (Listof QuadAttrPair)))
(define (gather-common-attrs qs)
  (: check-cap (QuadAttrPair . -> . Boolean))
  (define (check-cap cap)
    (equal? (Quad-attr-ref (car qs) (car cap) attr-missing) (cdr cap)))
  (let loop 
    ([qs qs]
     [common-attr-pairs : (Listof QuadAttrPair) (if (Quad-attrs (car qs))
                                                    
                                                    null
                                                    null)])
    (cond
      [(empty? common-attr-pairs) #f]
      [(empty? qs) (flatten common-attr-pairs)]
      [else (loop (cdr qs) (filter check-cap common-attr-pairs))])))


(define-syntax (define-quad-type stx)
  (syntax-case stx ()
    [(_ Id) 
     (with-syntax (
                   [Ids? (format-id #'Id "~as?" #'Id)]
                   [Quads->Id (format-id #'Id "Quads->~a" #'Id)])
       #'(begin
           (struct Id Quad ())
           (define-predicate Ids? (Listof Id))
           ;; quad converter
           (: Quads->Id ((Listof Quad) . -> . Id))
           (define (Quads->Id qs)
             (Id #hash() '()))
           ))]))




(define-quad-type Hello)
(define-quad-type Gbye)
(define h (Hello #hash((foo . bar)) (list (Hello #hash() '()))))
(define h2 (Quads->Hello '()))
