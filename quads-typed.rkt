#lang typed/racket/base
(require (for-syntax typed/racket/base racket/syntax racket/string))
(require "lib-typed.rkt")
;; note to self: a require/typed function with proper typing
;; is faster than a generic function + type assertion at location of call
(require/typed racket/list 
               [flatten ((Listof QuadAttrPair) . -> . HashableList)])
(require/typed sugar/list [trimf (All (A) ((Listof A) (A . -> . Boolean) -> (Listof A)))]
               [filter-split (All (A) ((Listof A) (A . -> . Boolean) -> (Listof (Listof A))))])
(require/typed racket/string [string-append* ((Listof String) . -> . String)])
(require/typed sugar/string [ends-with? (String String . -> . Boolean)])
(require sugar/debug)
(provide (all-defined-out))


(define-syntax (define/typed stx)
  (syntax-case stx ()
    [(_ (proc-name arg ... . rest-arg) type-expr body ...)
     #'(define/typed proc-name type-expr
         (λ(arg ... . rest-arg) body ...))]
    [(_ proc-name type-expr body ...)
     #'(begin
         (: proc-name type-expr)
         (define proc-name body ...))]))

(define-syntax (define/typed+provide stx)
  (syntax-case stx ()
    [(_ (proc-name arg ... . rest-arg) type-expr body ...)
     #'(begin
         (provide proc-name)
         (define/typed proc-name type-expr
           (λ(arg ... . rest-arg) body ...)))]
    [(_ proc-name type-expr body ...)
     #'(begin
         (provide proc-name)
         (begin
           (: proc-name type-expr)
           (define proc-name body ...)))]))


(define-syntax-rule (even-members xs)
  (for/list : (Listof Any) ([(x i) (in-indexed xs)] #:when (even? i))
    x))


(define-type QuadName Symbol)
(define-predicate QuadName? QuadName)

(define-type QuadAttrKey Symbol)
(define-predicate QuadAttrKey? QuadAttrKey)
(define-type QuadAttrValue Any)
(define-predicate QuadAttrValue? QuadAttrValue)
(define-type QuadAttrs (HashTable QuadAttrKey QuadAttrValue))
;;(define-predicate QuadAttrs? QuadAttrs) ;; won't work because it generates a chaperone contract
(define-type HashableList  (Rec duo (U Null (List* QuadAttrKey Any duo))))
(provide HashableList?)
(define-predicate HashableList? HashableList)

(: quad-attrs? (Any . -> . Boolean))
(define (quad-attrs? x)
  (and (hash? x) (andmap QuadAttrKey? (hash-keys x))))

(define-type QuadListItem (U Quad String))
(define-predicate QuadListItem? QuadListItem)
(define-type QuadList (Listof QuadListItem))
(define-predicate QuadList? QuadList)
(define-type (Treeof A) (Rec as (U A (Listof as))))

(struct quad ([name : QuadName] [attrs : QuadAttrs] [list : QuadList]) #:transparent)

(define-type Quad quad)
(define-predicate Quad? Quad)


(define quad-attr-ref
  (case-lambda
    [([q : Quad] [key : QuadAttrKey]) 
     (hash-ref (quad-attrs q) key)]
    [([q : Quad] [key : QuadAttrKey] [default : QuadAttrValue]) 
     (hash-ref (quad-attrs q) key (λ() default))]))

(define-syntax (quad-attr-ref/parameter stx)
  (syntax-case stx ()
    [(_ q key)
     (with-syntax ([world:key-default (format-id stx "~a-default" (string-trim (symbol->string (syntax->datum #'key)) "-key"))])
       #'(quad-attr-ref q key (world:key-default)))]))

(define cannot-be-common-attrs '(width x y page))
(define attr-missing (gensym))
(define-type QuadAttrPair (Pairof QuadAttrKey QuadAttrValue))


(: quad-ends-with? (Quad String . -> . Boolean))
(define (quad-ends-with? q str)
  (cond
    [(not (empty? (quad-list q)))
     (define last-item (list-ref (quad-list q) (length (quad-list q))))
     (cond
       [(string? last-item) (ends-with? last-item str)]
       [(quad? last-item) (quad-ends-with? last-item str)]
       [else #f])]
    [else #f]))

(: quad-append (Quad QuadListItem . -> . Quad))
(define (quad-append q new-item)
  (quad (quad-name q) (quad-attrs q) (append (quad-list q) (list new-item))))


(: quad->string (Quad . -> . String))
(define (quad->string x)
  (let loop : String ([x : (U Quad String) x])
    (cond 
      [(string? x) x]
      ;; else branch relies on fact that x is either Quad or String
      [else (string-append* ((inst map String QuadListItem) loop (quad-list x)))])))

(define/typed+provide (gather-common-attrs qs)
  ((Listof Quad) . -> . (Option HashableList))
  (: check-cap (Quad QuadAttrPair . -> . Boolean))
  (define (check-cap q cap) ; cap = candidate-attr-pair
    (equal? (quad-attr-ref q (car cap) attr-missing) (cdr cap)))
  (and (not (null? qs))
       (let loop 
         ([qs qs]
          ;; start with the set of pairs in the first quad, then filter it down
          [candidate-attr-pairs : (Listof QuadAttrPair) (let ([first-attrs (quad-attrs (car qs))])
                                                          (if first-attrs
                                                              (for/fold ([kvps null]) ([k (in-list (hash-keys first-attrs))])  
                                                                (if (member k cannot-be-common-attrs)
                                                                    kvps
                                                                    (cons (cons k (hash-ref first-attrs k)) kvps)))
                                                              null))])
         (cond
           [(null? candidate-attr-pairs) #f] ; ran out of possible pairs, so return #f
           [(null? qs) (flatten candidate-attr-pairs)] ; ran out of quads, so return common-attr-pairs
           ;; todo: reconsider type interface between output of this function and input to quadattrs
           [else (loop (cdr qs) (filter (λ([cap : QuadAttrPair]) (check-cap (car qs) cap)) candidate-attr-pairs))]))))

(define/typed (quadattrs xs)
  ((Listof (U QuadAttrKey QuadAttrValue)) . -> . QuadAttrs)
  (let-values ([(ks vs even?) (for/fold 
                               ([ks : (Listof QuadAttrKey) null][vs : (Listof QuadAttrValue) null][even? : Boolean #t])
                               ([x (in-list xs)])
                                (if even?
                                    ;; todo: how to avoid cast here by using HashableList typing?
                                    (values (cons (cast x QuadAttrKey) ks) vs #f)
                                    (values ks (cons (cast x QuadAttrValue) vs) #t)))]) 
    (when (not even?) (error 'quadattrs "odd number of elements in ~a" xs))
    (for/hash : QuadAttrs ([k (in-list ks)][v (in-list vs)])
      (values k v))))



(define-syntax (define-quad-type stx)
  (syntax-case stx ()
    [(_ id) 
     (with-syntax ([id? (format-id #'id "~a?" #'id)]
                   [quads->id (format-id #'id "quads->~a" #'id)])
       #'(begin
           ;; quad converter
           (define/typed (quads->id qs)
             ((Listof Quad) . -> . Quad)
             (apply id (gather-common-attrs qs) qs))
           
           #;(define/typed (id [attrs #f] . xs)
             (case-> 
              (-> Quad)
              (((U False QuadAttrs HashableList)) #:rest QuadListItem . ->* . Quad))
             (quad 'id (cond 
                         ;; need this cast because no predicate can be made for QuadAttrs
                         [(quad-attrs? attrs) (cast attrs QuadAttrs)]
                         [(list? attrs)
                          (if (HashableList? attrs)
                              (quadattrs attrs)
                              (error 'id "got non-hashable list ~a" attrs))]
                         [else (quadattrs '())]) (assert xs QuadList?)))
           
           ;; much slower than version above ... why?
           (define/typed id
               (case->
                (((U False QuadAttrs HashableList)) #:rest QuadListItem . ->* . Quad)
                (-> Quad))
               (case-lambda
                 [(attrs . xs)
                  (quad 'id (if attrs
                                (if (list? attrs)
                                    (quadattrs attrs)
                                    attrs)
                                (quadattrs null)) xs)]
                 [else (displayln "making quado") (quad 'id (quadattrs null) null)]))
           
           (: id? (Any . -> . Boolean))
           (define (id? x)
             (and (quad? x) (equal? (quad-name x) 'id)))
           
           ))]))

(define/typed (whitespace? x [nbsp? #f])
  ((Any) (Boolean) . ->* . Boolean)  
  (cond
    [(quad? x) (whitespace? (quad-list x) nbsp?)]
    [(string? x) (or (and (regexp-match #px"\\p{Zs}" x) ; Zs = unicode whitespace category
                          (or nbsp? (not (regexp-match #px"\u00a0" x)))))] ; 00a0: nbsp
    [(list? x) (and (not (empty? x)) (andmap (λ(x) (whitespace? x nbsp?)) x))] ; andmap returns #t for empty lists
    [else #f]))

(define (whitespace/nbsp? x)
  (whitespace? x #t))

(define-syntax (define-break-type stx)
  (syntax-case stx ()
    [(_ id) 
     (with-syntax ([split-on-id-breaks (format-id #'id "split-on-~a-breaks" #'id)]
                   [id-break (format-id #'id "~a-break" #'id)]
                   [id-break? (format-id #'id "~a-break?" #'id)]
                   [multi-id (format-id #'id "multi~a" #'id)]
                   [multi-id? (format-id #'id "multi~a?" #'id)]
                   [quads->multi-id (format-id #'id "quads->multi~a" #'id)])
       #'(begin
           (define-quad-type id)
           (define-quad-type id-break)
           (define-quad-type multi-id)
           ;; breaker
           (: split-on-id-breaks ((Listof Quad) . -> . (Listof (Listof Quad))))
           (define (split-on-id-breaks xs)
             ;; omit leading & trailing whitespace, because they're superfluous next to a break
             (map (λ([xs : (Listof Quad)]) (trimf xs whitespace?)) (filter-split xs id-break?)))))]))

(define quad= equal?)


(define/typed (quad-car q)
  (Quad . -> . QuadListItem)
  (define ql (quad-list q))
  (if (not (empty? ql))
      ((inst car QuadListItem QuadList) ql)
      (error 'quad-car "quad-list empty")))

(define/typed (quad-cdr q)
  (Quad . -> . QuadList)
  (define ql (quad-list q))
  (if (not (empty? ql))
      ((inst cdr QuadListItem QuadList) ql)
      (error 'quad-car "quad-list empty")))

(: quad-has-attr? (Quad QuadAttrKey . -> . Boolean))
(define (quad-has-attr? q key)
  (hash-has-key? (quad-attrs q) key))


(define-quad-type box)

(begin
(define-quad-type spacer)
(define-quad-type kern)
(define-quad-type optical-kern)
(define-quad-type flag)
(define-quad-type doc)
(define-quad-type input)
(define-quad-type piece)
(define-quad-type run)


(define-break-type word)
(define/typed (word-string c) 
  (Quad . -> . String)
  (define ql (quad-list c))
  (if (and (not (null? ql)) (string? (car ql)))
      (car ql)
      ""))
(define-break-type page)
(define-break-type column)
(define-break-type block)
(define-break-type line))

