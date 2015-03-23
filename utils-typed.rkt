#lang typed/racket/base
(require/typed sugar/list [slice-at ((Listof (U QuadAttrKey QuadAttrValue)) Positive-Integer . -> . (Listof (List QuadAttrKey QuadAttrValue)))])
(require/typed racket/list [flatten (All (A) (Rec as (U Any (Listof as))) -> (Listof Any))])
(require/typed hyphenate [hyphenate (String #:min-length Nonnegative-Integer #:min-left-length Nonnegative-Integer #:min-right-length Nonnegative-Integer . -> . String)])
(require (for-syntax racket/syntax racket/base) racket/string (except-in racket/list flatten) sugar/debug racket/bool racket/function math/flonum)
(require "quads-typed.rkt" "world-typed.rkt" "measure-typed.rkt")

(define/typed+provide (quad-map proc q)
  ((QuadListItem . -> . QuadListItem) Quad . -> . Quad)
  (quad (quad-name q) (quad-attrs q) (map proc (quad-list q))))


;; predicate for use below
(: list-of-mergeable-attrs? (Any . -> . Boolean))
(define (list-of-mergeable-attrs? xs)
  (and (list? xs) (andmap (λ(x) (or (quad? x) (quad-attrs? x) (HashableList? x))) xs)))

;; faster than (listof pair?)
(: pairs? (Any . -> . Boolean))
(define (pairs? x) (and (list? x) (andmap pair? x)))

;; push together multiple attr sources into one list of pairs.
;; mostly a helper function for the two attr functions below.
(define/typed+provide (join-attrs quads-or-attrs-or-lists)
  ((Listof (U Quad QuadAttrs HashableList)) . -> . (Listof QuadAttrPair))
  ((inst append-map QuadAttrPair QuadAttrs) (inst hash->list QuadAttrKey QuadAttrValue) (map (λ(x)
                                                                                               (cond
                                                                                                 [(quad? x) (quad-attrs x)]
                                                                                                 [(quad-attrs? x) (cast x QuadAttrs)]
                                                                                                 [(HashableList? x) (make-quadattrs (cast x (Listof Any)))]
                                                                                                 [else ;; something that will have no effect on result 
                                                                                                  (cast (hash) QuadAttrs)])) quads-or-attrs-or-lists)))


;; flatten merges attributes, but applies special logic suitable to flattening
;; for instance, resolving x and y coordinates.
(define/typed+provide (flatten-attrs . quads-or-attrs-or-falses)
  ((U Quad QuadAttrs) * . -> . QuadAttrs)
  (define all-attrs (join-attrs quads-or-attrs-or-falses))
  (define-values (x-attrs y-attrs other-attrs-reversed)
    (for/fold ([xas : (Listof QuadAttrPair) null]
               [yas : (Listof QuadAttrPair) null]
               [oas : (Listof QuadAttrPair) null])
              ([attr (in-list all-attrs)])
      (cond
        [(equal? (car attr) world:x-position-key) (values (cons attr xas) yas oas)]
        [(equal? (car attr) world:y-position-key) (values xas (cons attr yas) oas)]
        [else (values xas yas (cons attr oas))])))
  (: make-cartesian-attr (QuadAttrKey (Listof QuadAttrPair) . -> . (Listof QuadAttrPair)))
  (define (make-cartesian-attr key attrs) 
    (if (empty? attrs) 
        empty 
        (list (cons key (apply + (cast ((inst map QuadAttrValue QuadAttrPair) cdr attrs) (Listof Float)))))))
  (define x-attr (make-cartesian-attr world:x-position-key x-attrs))
  (define y-attr (make-cartesian-attr world:y-position-key y-attrs))
  (for/hash : QuadAttrs ([kv-pair (in-list (append x-attr y-attr (reverse other-attrs-reversed)))])
    (values (car kv-pair) (cdr kv-pair))))

;; merge concatenates attributes, with later ones overriding earlier.
;; most of the work is done by join-attrs.
(define/typed+provide (merge-attrs . quads-or-attrs-or-lists)
  ((U Quad QuadAttrs HashableList) * . -> . QuadAttrs)
  (for/hash : QuadAttrs ([kv-pair (in-list (join-attrs quads-or-attrs-or-lists))])
    (values (car kv-pair) (cdr kv-pair))))



;; pushes attributes down from parent quads to children, 
;; resulting in a flat list of quads.
(define/typed+provide (flatten-quad q)
  (Quad . -> . (Listof Quad))
  (cast (flatten 
         (let loop : (Treeof Quad)
           ([x : QuadListItem q][parent : Quad (box)])
           
           (cond       
             [(quad? x)
              (let ([x-with-parent-attrs (quad (quad-name x) 
                                               (flatten-attrs parent x) ; child positioned last so it overrides parent attributes 
                                               (quad-list x))])
                (if (empty? (quad-list x))
                    x-with-parent-attrs ; no subelements, so stop here
                    ((inst map (Treeof Quad) QuadListItem) (λ(xi) (loop xi x-with-parent-attrs)) (quad-list x))))] ; replace quad with its elements
             [else ;; it's a string
              (quad (quad-name parent) (quad-attrs parent) (list x))]))) (Listof Quad)))


;; flatten quad as above, 
;; then dissolve it into individual character quads while copying attributes
;; input is often large, so macro allows us to avoid allocation
(define/typed+provide (split-quad q)
  (Quad . -> . (Listof Quad))
  (: do-explode ((QuadListItem) (Quad) . ->* . (Treeof Quad)))
  (define (do-explode x [parent (box)])
    (cond
      [(quad? x)
       (if (empty? (quad-list x))
           x ; no subelements, so stop here
           ((inst map (Treeof Quad) QuadListItem) (λ(xi) (do-explode xi x)) (quad-list x)))] ; replace quad with its elements, exploded
      [else ;; it's a string
       ((inst map (Treeof Quad) QuadListItem) (λ(xc) (quad world:split-quad-key (quad-attrs parent) (list xc))) (regexp-match* #px"." x))]))
  (cast (flatten (map do-explode (flatten-quad q))) (Listof Quad)))


;; merge chars into words (and boxes), leave the rest
;; if two quads are mergeable types, and have the same attributes,
;; they get merged.
;; input is often large, so macro allows us to avoid allocation
(define/typed+provide (join-quads qs-in)
  ((Listof Quad) . -> . (Listof Quad))
  
  (let ([make-matcher (λ ([base-q : Quad])
                        (λ([q : Quad])
                          (and (member (quad-name q) world:mergeable-quad-types) 
                               (not (whitespace/nbsp? q))
                               ;; if key doesn't exist, it is compared against the default value.
                               ;; this way, a nonexistent value will test true against a default value.
                               (andmap (λ([key : Symbol] default) (equal? (quad-attr-ref base-q key default) (quad-attr-ref q key default)))
                                       (list world:font-name-key 
                                             world:font-size-key 
                                             world:font-weight-key 
                                             world:font-style-key)
                                       (list (world:font-name-default) 
                                             (world:font-size-default) 
                                             (world:font-weight-default) 
                                             (world:font-style-default))))))])
    (let loop ([qs : (Listof Quad) qs-in][acc : (Listof Quad) null])
      (if (null? qs)
          (reverse acc)
          (let* ([base-q (first qs)]
                 [mergeable-and-matches-base? (make-matcher base-q)]) ; make a new predicate function for this quad
            (cond
              [(mergeable-and-matches-base? base-q)
               ;; take as many quads that match, using the predicate function
               (define-values (matching-qs other-qs) (splitf-at (cdr qs) mergeable-and-matches-base?))
               (define new-word-strings (append-map quad-list (cons base-q matching-qs)))
               (define new-word 
                 (if (andmap string? new-word-strings)
                  (word (quad-attrs base-q) (string-append* new-word-strings))
                  (error 'join-quads "expected string")))
               (loop other-qs (cons new-word acc))]
              ;; otherwise move on to the next in line
              [else (loop (cdr qs) (cons base-q acc))]))))))


;; propagate x and y adjustments throughout the tree,
;; using parent x and y to adjust children, and so on.
(define/typed+provide (compute-absolute-positions qli)
  (Quad . -> . Quad)
  (define result 
    (let loop : QuadListItem ([qli : QuadListItem qli][parent-x : Float 0.0][parent-y : Float 0.0])
    (cond
      [(quad? qli) 
       (define adjusted-x (round-float (+ (cast (quad-attr-ref qli world:x-position-key 0.0) Float) parent-x)))
       (define adjusted-y (round-float (+ (cast (quad-attr-ref qli world:y-position-key 0.0) Float) parent-y)))
       (quad (quad-name qli) (merge-attrs qli (list world:x-position-key adjusted-x world:y-position-key adjusted-y)) ((inst map QuadListItem QuadListItem) (λ(qlii) (loop qlii adjusted-x adjusted-y)) (quad-list qli)))]
      [else ;; it's a string
       qli])))
  (if (string? result)
      (error 'compute-absolute-positions "got string as result: ~v" result)
      result))


;; functionally update a quad attr. Similar to hash-set
(define/typed+provide (quad-attr-set q k v)
  (Quad QuadAttrKey QuadAttrValue . -> . Quad)
  (quad (quad-name q) (merge-attrs (quad-attrs q) (list k v)) (quad-list q)))


;; functionally update multiple quad attrs. Similar to hash-set*
(define/typed+provide (quad-attr-set* q . kvs)
  (Quad (U QuadAttrKey QuadAttrValue) * . -> . Quad)
  (for/fold ([current-q q])([kv-list (in-list (slice-at kvs 2))])
    (apply quad-attr-set current-q kv-list)))

;; functionally remove a quad attr. Similar to hash-remove
(define/typed+provide (quad-attr-remove q k)
  (Quad QuadAttrKey . -> . Quad)
  (if (quad-attrs q)
      (quad (quad-name q) (hash-remove (quad-attrs q) k) (quad-list q))
      q))

;; functionally remove multiple quad attrs. Similar to hash-remove*
(define/typed+provide (quad-attr-remove* q . ks)
  (Quad QuadAttrKey * . -> . Quad)
  (for/fold ([current-q q])([k (in-list ks)])
    (quad-attr-remove current-q k)))


;; the last char of a quad
(define/typed+provide (quad-last-char q)
  (Quad . -> . (Option String))
  (define split-qs (split-quad q)) ; split makes it simple, but is it too expensive?
  (if (or (empty? split-qs) (empty? (quad-list (last split-qs))))
      #f
      (let ([result((inst car QuadListItem QuadListItem) (quad-list (last split-qs)))])
        (if (quad? result)
            (error 'quad-last-char "last element is not a string: ~v" result)
            result))))

;; the first char of a quad
(define/typed+provide (quad-first-char q)
  (Quad . -> . (Option String))
  (define split-qs (split-quad q)) ; explosion makes it simple, but is it too expensive?
  (if (or (empty? split-qs) (empty? (quad-list (first split-qs))))
      #f
      (let ([result((inst car QuadListItem QuadListItem) (quad-list (first split-qs)))])
        (if (quad? result)
            (error 'quad-first-char "first element is not a string: ~v" result)
            result))))


;; todo: how to guarantee line has leading key?
(define/typed+provide (compute-line-height line)
  (Quad . -> . Quad)
  (quad-attr-set line world:height-key (quad-attr-ref/parameter line world:leading-key)))

(define/typed (fixed-height? q)
  (Quad . -> . Boolean)
  (quad-has-attr? q world:height-key))

(define/typed+provide (quad-height q)
  (Quad . -> . Float)
  (cast (quad-attr-ref q world:height-key 0.0) Float))

;; use heights to compute vertical positions
(define/typed+provide (add-vert-positions starting-quad)
  (Quad . -> . Quad)
  (define-values (new-quads final-height)
    (for/fold ([new-quads : (Listof Quad) empty][height-so-far : Float 0.0])([q (in-list (cast (quad-list starting-quad) (Listof Quad)))])
      (values (cons (quad-attr-set q world:y-position-key height-so-far) new-quads) 
              (round-float (+ height-so-far (quad-height q))))))
  (quad (quad-name starting-quad) (quad-attrs starting-quad) (reverse new-quads)))

;; recursively hyphenate strings in a quad
(define/typed+provide (hyphenate-quad x)
  (QuadListItem . -> . QuadListItem)
  (cond
    [(quad? x) (quad-map hyphenate-quad x)]
    [(string? x) (hyphenate x
                            #:min-length 6	 	 	 	 
                            #:min-left-length 3	 	 	 	 
                            #:min-right-length 3)]
    [else x]))

;; just because it comes up a lot
(define/typed+provide (split-last xs)
  (All (A) ((Listof A) -> (values (Listof A) A)))
  (let-values ([(first-list last-list) ((inst split-at-right A) xs 1)])
    (values first-list (car last-list))))

;; like cons, but joins a list to an atom
(provide snoc)
(define-syntax-rule (snoc xs x)
  (append xs (list x)))
