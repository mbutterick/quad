#lang typed/racket/base
(require (for-syntax typed/racket/base racket/syntax racket/string))
(require "lib-typed.rkt")
;; note to self: a require/typed function with proper typing
;; is faster than a generic function + type assertion at location of call
(require/typed racket/list 
               [flatten ((Listof QuadAttr) . -> . HashableList)])
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

(define-syntax (define-type+predicate stx)
  (syntax-case stx ()
    [(_ id basetype)
     (with-syntax ([id? (format-id stx "~a?" #'id)])
       #'(begin
           (define-type id basetype)
           (define-predicate id? id)))]))

(define-type+predicate QuadName Symbol)
(define-type+predicate QuadAttrKey Symbol)
(define-type+predicate QuadAttrValue (U Float Index String Symbol))
(define-type+predicate QuadAttr (List QuadAttrKey QuadAttrValue))
(define-type+predicate QuadAttrs (Listof QuadAttr))
(provide HashableList?)
(define-type+predicate HashableList  (Rec duo (U Null (List* QuadAttrKey Any duo))))

(: quad-attrs? (Any . -> . Boolean))
(define (quad-attrs? x)
  (and (hash? x) (andmap QuadAttrKey? (hash-keys x))))

(define-type QuadListItem (U String Quad))
(define-type QuadList (Listof QuadListItem))
(define-type (Treeof A) (Rec as (U A (Listof as))))


;; funky implementation
(define-type+predicate Quad (Pairof QuadName (Pairof QuadAttrs (Listof (U String Quad)))))
(define-predicate quad? Quad)
(define/typed (quad name attrs items)
  (QuadName QuadAttrs QuadList . -> . Quad)
  `(,name ,attrs ,@items))

(define-type+predicate QuadSet (List QuadName QuadAttrs (Listof Quad)))

(define/typed (quad-name q)
  (Quad . -> . QuadName)
  (car q))

(define/typed (quad-attrs q)
  (Quad . -> . QuadAttrs)
  (cadr q))

(define/typed (quad-attr-keys qas)
  (QuadAttrs . -> . (Listof QuadAttrKey))
  (if (empty? qas)
      qas
      ((inst map QuadAttrKey QuadAttr) car qas)))

(define/typed (quad-list q)
  (Quad . -> . QuadList)
  (cddr q))



(define/typed (quad-attr-ref q-or-qas key [default attr-missing])
  (((U Quad QuadAttrs) QuadAttrKey) (QuadAttrValue) . ->* . QuadAttrValue)
  (define qas (if (quad? q-or-qas) (quad-attrs q-or-qas) q-or-qas))
  (define qa-result (memf (λ([qap : QuadAttr]) (equal? key (car qap))) qas))
  (if qa-result
      ;; car beacause result of memf is a list tail; cadr because second element in pair
      (cadr (car qa-result))
      (if (not (equal? default attr-missing)) default (error 'key-not-found))))


(define-syntax (quad-attr-ref/parameter stx)
  (syntax-case stx ()
    [(_ q key)
     (with-syntax ([world:key-default (format-id stx "~a-default" (string-trim (symbol->string (syntax->datum #'key)) "-key"))])
       #'(quad-attr-ref q key (world:key-default)))]))


(define cannot-be-common-attrs '(width x y page))
(define attr-missing (gensym))

(: quad-ends-with? (Quad String . -> . Boolean))
(define (quad-ends-with? q str)
  (cond
    [(not (empty? (quad-list q)))
     (define last-item (list-ref (quad-list q) (sub1 (length (quad-list q)))))
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
  (: check-cap (Quad QuadAttr . -> . Boolean))
  (define (check-cap q cap) ; cap = candidate-attr-pair
    (equal? (quad-attr-ref q (car cap) attr-missing) (cdr cap)))
  (and (not (null? qs))
       (let loop 
         ([qs qs]
          ;; start with the set of pairs in the first quad, then filter it down
          [candidate-attr-pairs : (Listof QuadAttr) (let ([first-attrs (quad-attrs (car qs))])
                                                      (if first-attrs
                                                          (for/fold ([kvps : QuadAttrs null]) ([k (in-list (quad-attr-keys first-attrs))])  
                                                            (if (member k cannot-be-common-attrs)
                                                                kvps
                                                                (cons (list k (quad-attr-ref first-attrs k)) kvps)))
                                                          null))])
         (cond
           [(null? candidate-attr-pairs) #f] ; ran out of possible pairs, so return #f
           [(null? qs) (flatten candidate-attr-pairs)] ; ran out of quads, so return common-attr-pairs
           ;; todo: reconsider type interface between output of this function and input to quadattrs
           [else (loop (cdr qs) (filter (λ([cap : QuadAttr]) (check-cap (car qs) cap)) candidate-attr-pairs))]))))

(define/typed (make-quadattrs xs)
  ;; no point typing the input as (U QuadAttrKey QuadAttrValue) 
  ;; because QuadAttrValue is Any, so that's the same as plain Any
  ((Listof Any) . -> . QuadAttrs)
  (let-values ([(ks vs even?) (for/fold 
                               ([ks : (Listof QuadAttrKey) null][vs : (Listof QuadAttrValue) null][even? : Boolean #t])
                               ([x (in-list xs)])
                                (if (and even? (QuadAttrKey? x))
                                    (values (cons x ks) vs #f)
                                    (values ks (cons (assert x QuadAttrValue?) vs) #t)))]) 
    (when (not even?) (error 'quadattrs "odd number of elements in ~a" xs))
    (for/list : QuadAttrs ([k (in-list ks)][v (in-list vs)])
      (list k v))))



(define-syntax (define-quad-type stx)
  (syntax-case stx ()
    [(_ id) 
     (with-syntax ([id? (format-id #'id "~a?" #'id)]
                   [IdQuad (format-id #'id "~aQuad" (string-titlecase (symbol->string (syntax->datum #'id))))]
                   [IdQuad? (format-id #'id "~aQuad?" (string-titlecase (symbol->string (syntax->datum #'id))))]
                   [quads->id (format-id #'id "quads->~a" #'id)])
       #'(begin
           ;; quad converter
           (define/typed (quads->id qs)
             ((Listof Quad) . -> . Quad)
             (apply id (gather-common-attrs qs) qs))
           
           (define-type IdQuad (List 'id QuadAttrs QuadList))
           (define-predicate IdQuad? IdQuad)
           (define id? IdQuad?)
           
           (define/typed (id [attrs '()] #:zzz [zzz 0] . xs)
             (() (QuadAttrs #:zzz Zero) #:rest QuadListItem . ->* . Quad)
             (quad 'id (if (list? attrs)
                           (make-quadattrs attrs)
                           attrs) xs))))]))

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
  (and ((inst member QuadAttrKey) key (quad-attr-keys (quad-attrs q))) #t))


(define-quad-type box)

#|
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
(define-break-type line)
|#
