#lang typed/racket/base
(require (for-syntax typed/racket/base racket/syntax racket/string))
(require "lib-typed.rkt" "core-types.rkt")
;; note to self: a require/typed function with proper typing
;; is faster than a generic function + type assertion at location of call
(require/typed racket/list 
               [flatten ((Listof QuadAttr) -> QuadAttrs)])
(require/typed racket/string [string-append* ((Listof String) -> String)])
(require typed/sugar/debug typed/sugar/string typed/sugar/list typed/sugar/define)
(provide (all-defined-out) (all-from-out "core-types.rkt"))

(define-syntax-rule (even-members xs)
  (for/list : (Listof Any) ([(x i) (in-indexed xs)] #:when (even? i))
    x))

(define/typed (quad-name q)
  (Quad -> QuadName)
  (car q))

(define/typed (quad-attrs q)
  (Quad -> QuadAttrs)
  (car (cdr q)))

(define/typed (make-quadattr k v)
  (QuadAttrKey QuadAttrValue -> QuadAttr)
  (cons k v))

(define/typed (quadattr-key qa)
  (QuadAttr -> QuadAttrKey)
  (car qa))

(define/typed (quadattr-value qa)
  (QuadAttr -> QuadAttrValue)
  (cdr qa))

(define/typed (quad-attr-keys qas)
  (QuadAttrs -> (Listof QuadAttrKey))
  (if (empty? qas)
      qas
      ((inst map QuadAttrKey QuadAttr) car qas)))

(define/typed (quad-list q)
  (case->
   (GroupQuad -> GroupQuadList)
   (Quad -> QuadList))
  (cdr (cdr q)))


(define/typed (quad-attr-ref q-or-qas key [default attr-missing])
  (((U Quad QuadAttrs) QuadAttrKey) (QuadAttrValue) . ->* . QuadAttrValue)
  (define qas (if (quad? q-or-qas) (quad-attrs q-or-qas) q-or-qas))
  (define qa-result (memf (λ([qap : QuadAttr]) (equal? key (car qap))) qas))
  (if qa-result 
      ;; car beacause result of memf is a list tail; cadr because second element in pair
      (quadattr-value (car qa-result))
      (if (not (equal? default attr-missing)) default (error 'quad-attr-ref (format "Key ~v not found in quad attributes ~v" key qas)))))


(define-syntax (quad-attr-ref/parameter stx)
  (syntax-case stx ()
    [(_ q key)
     (with-syntax ([world:key-default (format-id stx "~a-default" (string-trim (symbol->string (syntax->datum #'key)) "-key"))])
       #'(quad-attr-ref q key (world:key-default)))]))


(define cannot-be-common-attrs '(width x y page))
(define attr-missing (gensym))

(: quad-ends-with? (Quad String -> Boolean))
(define (quad-ends-with? q str)
  (cond
    [(not (empty? (quad-list q)))
     (define last-item (list-ref (quad-list q) (sub1 (length (quad-list q)))))
     (cond
       [(string? last-item) (ends-with? last-item str)]
       [(quad? last-item) (quad-ends-with? last-item str)]
       [else #f])]
    [else #f]))

(: quad-append (Quad QuadListItem -> Quad))
(define (quad-append q new-item)
  (quad (quad-name q) (quad-attrs q) (append (quad-list q) (list new-item))))


(: quad->string (Quad -> String))
(define (quad->string x)
  (let loop : String ([x : (U Quad String) x])
    (cond 
      [(string? x) x]
      ;; else branch relies on fact that x is either Quad or String
      [else (string-append* ((inst map String QuadListItem) loop (quad-list x)))])))

(define/typed+provide (gather-common-attrs qs)
  ((Listof Quad) -> QuadAttrs)
  (if (null? qs)
      qs
      (let loop 
        ([qs qs]
         ;; start with the set of pairs in the first quad, then filter it down
         [candidate-attr-pairs : (Listof QuadAttr) (let ([first-attrs (quad-attrs (car qs))])
                                                     (if first-attrs
                                                         (for/fold ([caps : QuadAttrs null]) ([cap (in-list first-attrs)])  
                                                           (if (member (car cap) cannot-be-common-attrs)
                                                               caps
                                                               (cons cap caps)))
                                                         null))])
        (cond
          [(null? candidate-attr-pairs) null] ; ran out of possible pairs, so return #f
          [(null? qs) candidate-attr-pairs] ; ran out of quads, so return common-attr-pairs
          ;; todo: reconsider type interface between output of this function and input to quadattrs
          [else (loop (cdr qs) (filter (λ([cap : QuadAttr]) (member cap (quad-attrs (car qs)))) candidate-attr-pairs))]))))

(define/typed (make-quadattrs xs)
  ;; no point typing the input as (U QuadAttrKey QuadAttrValue) 
  ;; because QuadAttrValue is Any, so that's the same as plain Any
  ((Listof Any) -> QuadAttrs)
  (let-values ([(ks vs even?) (for/fold 
                               ([ks : (Listof QuadAttrKey) null][vs : (Listof QuadAttrValue) null][even? : Boolean #t])
                               ([x (in-list xs)])
                                (if (and even? (QuadAttrKey? x))
                                    (values (cons x ks) vs #f)
                                    (values ks (cons (assert x QuadAttrValue?) vs) #t)))]) 
    (when (not even?) (error 'quadattrs "odd number of elements in ~a" xs))
    ;; use for/fold rather than for/list to impliedly reverse the list
    ;; (having been reversed once above, this puts it back in order)
    (for/fold ([qas : QuadAttrs null])([k (in-list ks)][v (in-list vs)])
      (cons (make-quadattr k v) qas))))



(define-syntax (define-quad-type stx)
  (syntax-case stx ()
    [(_ id)
     #'(define-quad-type id #f)]
    [(_ id wants-group?) 
     (with-syntax ([id? (format-id #'id "~a?" #'id)]
                   [IdQuad (format-id #'id "~aQuad" (string-titlecase (symbol->string (syntax->datum #'id))))]
                   [IdQuad? (format-id #'id "~aQuad?" (string-titlecase (symbol->string (syntax->datum #'id))))]
                   [quads->id (format-id #'id "quads->~a" #'id)])
       #`(begin
           ;; quad converter
           (define/typed (quads->id qs)
             ((Listof Quad) -> IdQuad)
             (apply id (gather-common-attrs qs) qs))
           
           (define-type IdQuad (List* 'id QuadAttrs #,(if (syntax->datum #'wants-group?)
                                                          #'GroupQuadList
                                                          #'QuadList)))
           (define-predicate IdQuad? IdQuad)
           (define id? IdQuad?)
           
           (define/typed (id [attrs '()] #:zzz [zzz 0] . xs)
             (() ((U QuadAttrs HashableList) #:zzz Zero) #:rest #,(if (syntax->datum #'wants-group?)
                                                                      #'GroupQuadListItem
                                                                      #'QuadListItem) . ->* . IdQuad)
             (quad 'id (if (QuadAttrs? attrs)
                           attrs
                           (make-quadattrs attrs)) xs))))]))

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
     #'(define-break-type id #f)]
    [(_ id wants-group?)  
     (with-syntax ([split-on-id-breaks (format-id #'id "split-on-~a-breaks" #'id)]
                   [id-break (format-id #'id "~a-break" #'id)]
                   [id-break? (format-id #'id "~a-break?" #'id)]
                   [multi-id (format-id #'id "multi~a" #'id)]
                   [multi-id? (format-id #'id "multi~a?" #'id)]
                   [quads->multi-id (format-id #'id "quads->multi~a" #'id)])
       #'(begin
           (define-quad-type id wants-group?)
           (define-quad-type id-break) ; break is not necessarily a group
           (define-quad-type multi-id wants-group?) ; multi-id is always a group
           ;; breaker
           (: split-on-id-breaks ((Listof Quad) -> (Listof (Listof Quad))))
           (define (split-on-id-breaks xs)
             ;; omit leading & trailing whitespace, because they're superfluous next to a break
             (map (λ([xs : (Listof Quad)]) (trimf xs whitespace?)) (filter-split xs id-break?)))))]))

(define quad= equal?)


(define/typed (quad-car q)
  (Quad -> QuadListItem)
  (define ql (quad-list q))
  (if (not (empty? ql))
      ((inst car QuadListItem QuadList) ql)
      (error 'quad-car "quad-list empty")))

(define/typed (quad-cdr q)
  (Quad -> QuadList)
  (define ql (quad-list q))
  (if (not (empty? ql))
      ((inst cdr QuadListItem QuadList) ql)
      (error 'quad-car "quad-list empty")))

(: quad-has-attr? (Quad QuadAttrKey -> Boolean))
(define (quad-has-attr? q key)
  (and ((inst member QuadAttrKey) key (quad-attr-keys (quad-attrs q))) #t))


(define-quad-type box)


(define-quad-type spacer)
(define-quad-type kern)
(define-quad-type optical-kern)
(define-quad-type flag)
(define-quad-type doc)
(define-quad-type input)
(define-quad-type piece #t)
(define-quad-type run)


(define-break-type word)
(define/typed (word-string c) 
  (Quad -> String)
  (define ql (quad-list c))
  (if (and (not (null? ql)) (string? (car ql)))
      (car ql)
      ""))


(define-break-type page #t)
(define-break-type column #t)
(define-break-type block)
(define-break-type line #t)
