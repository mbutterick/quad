#lang typed/racket/base
(require (for-syntax typed/racket/base racket/syntax))
(require/typed racket/list [flatten (All (A) ((Listof A) -> (Listof A)))]
               [empty? (All (A) ((Listof A) -> Boolean))])
(require/typed sugar/list [trimf (All (A) ((Listof A) (A . -> . Boolean) -> (Listof A)))]
               [filter-split (All (A) ((Listof A) (A . -> . Boolean) -> (Listof (Listof A))))])
(require sugar/debug)
(provide (all-defined-out))

;; struct implementation

(define-type QuadAttrKey Symbol)
(define-type QuadAttrValue Any)
(define-type QuadAttrs (HashTable QuadAttrKey QuadAttrValue))
(define-type QuadList (Listof (U Quad String)))
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


(provide gather-common-attrs)
(: gather-common-attrs ((Listof Quad) . -> . (U False (Listof QuadAttrPair))))
(define (gather-common-attrs qs)
  (: check-cap (QuadAttrPair . -> . Boolean))
  (define (check-cap cap)
    (equal? (Quad-attr-ref (car qs) (car cap) attr-missing) (cdr cap)))
  (let loop 
    ([qs qs]
     [common-attr-pairs : (Listof QuadAttrPair) (if (Quad-attrs (car qs))
                                                    
                                                    (for/list ([kv-pair (in-hash-pairs (Quad-attrs (car qs)))] 
                                                               #:unless (member (car kv-pair) cannot-be-common-attrs))  
                                                      kv-pair)
                                                    null)])
    (cond
      [(null? common-attr-pairs) #f]
      [(null? qs) common-attr-pairs]
      [else (loop (cdr qs) (filter check-cap common-attr-pairs))])))


(: quadattrs ((Listof Any) . -> . QuadAttrs))
(define (quadattrs xs)
  (let-values ([(ks vs even?) (for/fold 
                               ([ks : (Listof Any) null][vs : (Listof Any) null][even? : Boolean #t])
                               ([x (in-list xs)])
                                (if even?
                                    (values (cons x ks) vs #f)
                                    (values ks (cons x vs) #t)))]) 
    (when (not even?) (error 'bad-input))
    (cast (for/hash ([k (in-list ks)][v (in-list vs)])
      (values k v)) QuadAttrs)))



(define-syntax (define-quad-type stx)
  (syntax-case stx ()
    [(_ Id) 
     (with-syntax (
                   [id (format-id #'Id "~a" (string-downcase (symbol->string (syntax->datum #'Id))))]
                   [Ids? (format-id #'Id "~as?" #'Id)]
                   [Quads->Id (format-id #'Id "Quads->~a" #'Id)])
       #'(begin
           (struct Id Quad ())
           (define-predicate Ids? (Listof Id))
           ;; quad converter
           (: Quads->Id ((Listof Quad) . -> . Id))
           (define (Quads->Id qs)
             (Id #hash() '()))
           
           (provide id)
           (: id (case-> 
                  (-> Id)
                  ((Option (Listof Any)) (U String Quad) * . -> . Id)))
           (define (id [attrs #f] . xs)
             (Id (quadattrs (if (list? attrs) attrs '())) (cast xs QuadList)))
           ))]))

(: whitespace? ((Any) (Boolean) . ->* . Boolean))
(define (whitespace? x [nbsp? #f])
  ;((any/c)(boolean?) . ->* . coerce/boolean?)
  (cond
    [(Quad? x) (whitespace? (Quad-list x) nbsp?)]
    [(string? x) (or (and (regexp-match #px"\\p{Zs}" x) ; Zs = unicode whitespace category
                          (or nbsp? (not (regexp-match #px"\u00a0" x)))))] ; 00a0: nbsp
    [(list? x) (and (not (empty? x)) (andmap (λ(x) (whitespace? x nbsp?)) x))] ; andmap returns #t for empty lists
    [else #f]))

(define (whitespace/nbsp? x)
  (whitespace? x #t))

(define-syntax (define-break-type stx)
  (syntax-case stx ()
    [(_ Id) 
     (with-syntax ([split-on-id-breaks (format-id #'Id "split-on-~a-breaks" (string-downcase (symbol->string (syntax->datum #'Id))))]
                   [id-break (format-id #'Id "~a-break" #'Id)]
                   [id-break? (format-id #'Id "~a-break?" #'Id)]
                   [multi-id (format-id #'Id "multi~a" #'Id)]
                   [multi-id? (format-id #'Id "multi~a?" #'Id)]
                   [quads->multi-id (format-id #'Id "quads->multi~a" #'Id)])
       #'(begin
           (define-quad-type Id)
           (define-quad-type id-break)
           (define-quad-type multi-id)
           ;; breaker
           (: split-on-id-breaks ((Listof Quad) . -> . (Listof (Listof Quad))))
           (define (split-on-id-breaks xs)
             ;; omit leading & trailing whitespace, because they're superfluous next to a break
             (map (λ([xs : (Listof Quad)]) (trimf xs whitespace?)) (filter-split xs id-break?)))))]))

(define quad= equal?)

(: quad-has-attr? (Quad QuadAttrKey . -> . Boolean))
(define (quad-has-attr? q key)
  (hash-has-key? (Quad-attrs q) key))

(define-quad-type Word)
(define-break-type Block)
(define-break-type Page)
(split-on-page-breaks (list (word) (page-break) (word)))
