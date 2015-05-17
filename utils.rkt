#lang racket/base
(require sugar/list sugar/define)
(require (for-syntax racket/syntax racket/base) racket/string racket/contract racket/list sugar/debug racket/bool hyphenate racket/function math/flonum)
(require "quads.rkt" "world.rkt" "measure.rkt")


;; predicate for use below
(define (list-of-mergeable-attrs? xs)
  (and (list? xs) (andmap (λ(x) (or (quad? x) (quad-attrs? x) (hashable-list? x))) xs)))

;; faster than (listof pair?)
(define (pairs? x) (and (list? x) (andmap pair? x)))

;; push together multiple attr sources into one list of pairs.
;; mostly a helper function for the two attr functions below.
(define+provide (join-attrs quads-or-attrs-or-lists)
  (list-of-mergeable-attrs? . -> . pairs?)
  (append-map hash->list (filter-not false? (map (λ(x)
                                                   (cond
                                                     [(quad? x) (quad-attrs x)]
                                                     [(quad-attrs? x) x]
                                                     [(hashable-list? x) (apply hash x)]
                                                     [else #f])) quads-or-attrs-or-lists))))


;; merge concatenates attributes, with later ones overriding earlier.
;; most of the work is done by join-attrs.
(define+provide (merge-attrs . quads-or-attrs-or-lists)
  (() #:rest list-of-mergeable-attrs? . ->* . quad-attrs?)
  (define all-attrs (join-attrs quads-or-attrs-or-lists))
  (apply hash (flatten all-attrs)))

;; functionally update a quad attr. Similar to hash-set
(define+provide (quad-attr-set q k v)
  (quad? symbol? any/c . -> . quad?)
  (quad (quad-name q) (merge-attrs (quad-attrs q) (list k v)) (quad-list q)))


;; functionally update multiple quad attrs. Similar to hash-set*
(define+provide (quad-attr-set* q . kvs)
  ((quad?) #:rest hashable-list? . ->* . quad?)
  (for/fold ([current-q q])([kv-list (in-list (slice-at kvs 2))])
    (apply quad-attr-set current-q kv-list)))

;; functionally remove a quad attr. Similar to hash-remove
(define+provide (quad-attr-remove q k)
  (quad? symbol? . -> . quad?)
  (if (quad-attrs q)
      (quad (quad-name q) (hash-remove (quad-attrs q) k) (quad-list q))
      q))

;; functionally remove multiple quad attrs. Similar to hash-remove
(define+provide (quad-attr-remove* q . ks)
  ((quad?) #:rest (λ(ks) (and (list? ks) (andmap symbol? ks))) . ->* . quad?)
  (for/fold ([current-q q])([k (in-list ks)])
    (quad-attr-remove current-q k)))

(define+provide (quad-map proc q)
  (procedure? quad? . -> . quad?)
  (quad (quad-name q) (quad-attrs q) (map proc (quad-list q))))


;; flatten merges attributes, but applies special logic suitable to flattening
;; for instance, resolving x and y coordinates.
(define+provide (flatten-attrs . quads-or-attrs-or-falses)
  (() #:rest (listof (or/c quad? quad-attrs?)) . ->* . quad-attrs?)
  (define all-attrs (join-attrs quads-or-attrs-or-falses))
  (define-values (x-attrs y-attrs other-attrs-reversed)
    (for/fold ([xas null][yas null][oas null])([attr (in-list all-attrs)])
      (cond
        [(equal? (car attr) world:x-position-key) (values (cons attr xas) yas oas)]
        [(equal? (car attr) world:y-position-key) (values xas (cons attr yas) oas)]
        [else (values xas yas (cons attr oas))])))
  (define (make-cartesian-attr key attrs) (if (empty? attrs) empty (cons key (apply + (map cdr attrs)))))
  (define-values (x-attr y-attr) (apply values (map make-cartesian-attr (list world:x-position-key world:y-position-key) (list x-attrs y-attrs))))
  (apply hash (flatten (list* x-attr y-attr (reverse other-attrs-reversed)))))

;; pushes attributes down from parent quads to children, 
;; resulting in a flat list of quads.
;; input is often large, so macro allows us to avoid allocation
(provide flatten-quad)
(define-syntax-rule (flatten-quad q)
  ;  (quad? . -> . quads?)
  (flatten
   (let loop ([x q][parent #f])
     (cond       
       [(quad? x)
        (let ([x-with-parent-attrs (quad (quad-name x) 
                                         (flatten-attrs parent x) ; child positioned last so it overrides parent attributes 
                                         (quad-list x))])
          (if (empty? (quad-list x))
              x-with-parent-attrs ; no subelements, so stop here
              (map (λ(xi) (loop xi x-with-parent-attrs)) (quad-list x))))] ; replace quad with its elements
       [(string? x) (quad (quad-name parent) (quad-attrs parent) (list x))]))))

;; flatten quad as above, 
;; then dissolve it into individual character quads while copying attributes
;; input is often large, so macro allows us to avoid allocation
(define+provide (split-quad q)
  ;(quad? . -> . quads?)
  (letrec ([do-explode (λ(x [parent #f])
                         (cond
                           [(quad? x)
                            (if (empty? (quad-list x))
                                x ; no subelements, so stop here
                                (map (λ(xi) (do-explode xi x)) (quad-list x)))] ; replace quad with its elements, exploded
                           [else (map (λ(xc) (quad 'word (quad-attrs parent) (list xc))) (regexp-match* #px"." x))]))])
    (flatten (map do-explode (flatten-quad q)))))



;; merge chars into words (and boxes), leave the rest
;; if two quads are mergeable types, and have the same attributes,
;; they get merged.
;; input is often large, so macro allows us to avoid allocation
(provide join-quads)
(define-syntax-rule (join-quads qs-in)
  ;((quads?)(quads?) . ->* . quads?)
  
  (let ([make-matcher (λ (base-q)
                        (λ(q)
                          (and (member (quad-name q) world:mergeable-quad-types) 
                               (not (whitespace/nbsp? q))
                               ;; if key doesn't exist, it is compared against the default value.
                               ;; this way, a nonexistent value will test true against a default value.
                               (andmap (λ(key default) (equal? (quad-attr-ref base-q key default) (quad-attr-ref q key default)))
                                       (list world:font-name-key 
                                             world:font-size-key 
                                             world:font-weight-key 
                                             world:font-style-key)
                                       (list (world:font-name-default) 
                                             (world:font-size-default) 
                                             (world:font-weight-default) 
                                             (world:font-style-default))))))])
    (let loop ([qs qs-in][acc null])
      (if (null? qs)
          (reverse acc)
          (let* ([base-q (first qs)]
                 [mergeable-and-matches-base? (make-matcher base-q)]) ; make a new predicate function for this quad
            (cond
              [(mergeable-and-matches-base? base-q)
               ;; take as many quads that match, using the predicate function
               (define-values (matching-qs other-qs) (splitf-at (cdr qs) mergeable-and-matches-base?))
               (define new-word (word (quad-attrs base-q) (string-append* (append-map quad-list (cons base-q matching-qs)))))
               (loop other-qs (cons new-word acc))]
              ;; otherwise move on to the next in line
              [else (loop (cdr qs) (cons base-q acc))]))))))



;; the last char of a quad
(define+provide (quad-last-char q)
  (quad? . -> . (or/c #f string?))
  (define split-qs (split-quad q)) ; split makes it simple, but is it too expensive?
  (if (or (empty? split-qs) (empty? (quad-list (last split-qs))))
      #f
      (car (quad-list (last split-qs)))))

;; the first char of a quad
(define+provide (quad-first-char q)
  (quad? . -> . (or/c #f string?))
  (define split-qs (split-quad q)) ; explosion makes it simple, but is it too expensive?
  (if (or (empty? split-qs) (empty? (quad-list (first split-qs))))
      #f
      (car (quad-list (first split-qs)))))


;; propagate x and y adjustments throughout the tree,
;; using parent x and y to adjust children, and so on.
(define+provide (compute-absolute-positions i [parent-x 0][parent-y 0])
  ((quad?) (integer? integer?) . ->* . quad?)
  (cond
    [(quad? i) 
     (define adjusted-x (round-float (+ (quad-attr-ref i world:x-position-key 0) parent-x)))
     (define adjusted-y (round-float (+ (quad-attr-ref i world:y-position-key 0) parent-y)))
     (quad (quad-name i) (merge-attrs i (list world:x-position-key adjusted-x world:y-position-key adjusted-y)) (map (λ(ii) (compute-absolute-positions ii adjusted-x adjusted-y)) (quad-list i)))]
    [else i]))

;; simple assert. should get moved to sugar/debug
(provide assert)
(define-syntax-rule (assert pred expr)
  (let ([result expr])
    (if (pred result)
        result
        (error 'assert-failure (format "\n~a\nevaluates to:\n~a\nwhich is not:\n~a" 'expr result 'pred)))))

;; peeks at arguments and times execution
(provide snoop)
(define-syntax (snoop stx)
  (syntax-case stx ()
    [(_ proc arg ... . rest) 
     (with-syntax ()
       #'(begin
           (displayln (format "Evaluating ~s" '(proc arg ... . rest)))
           (let ([start (current-milliseconds)]
                 [result (proc arg ... . rest)]
                 [end (current-milliseconds)])
             (displayln (format "Evaluation of ~s took ~a ms\nResult ~a" '(proc arg ... . rest) (- end start) result))
             result)))]))

;; find total pages in doc by searching on page count key.
(define+provide (pages-in-doc doc)
  (doc? . -> . integer?)
  (add1 (apply max (map (curryr quad-attr-ref world:page-key 0) (quad-list doc)))))


;; todo: how to guarantee line has leading key?
(define+provide (compute-line-height line)
  (line? . -> . line?)
  (quad-attr-set line world:height-key (quad-attr-ref/parameter line world:leading-key)))

(define (fixed-height? q) (quad-has-attr? q world:height-key))
(define+provide (quad-height q)
  (quad? . -> . number?)
  (quad-attr-ref q world:height-key 0))

;; use heights to compute vertical positions
(define+provide (add-vert-positions starting-quad)
  (quad? . -> . quad?)
  (define-values (new-quads final-height)
    (for/fold ([new-quads empty][height-so-far 0])([q (in-list (quad-list starting-quad))])
      (values (cons (quad-attr-set q world:y-position-key height-so-far) new-quads) 
              (round-float (+ height-so-far (quad-height q))))))
  (quad (quad-name starting-quad) (quad-attrs starting-quad) (reverse new-quads)))

;; recursively hyphenate strings in a quad
(define+provide (hyphenate-quad x)
  (quad? . -> . quad?)
  (cond
    [(quad? x) (quad-map hyphenate-quad x)]
    [(string? x) (hyphenate x
                            #:min-length 6	 	 	 	 
                            #:min-left-length 3	 	 	 	 
                            #:min-right-length 3)]
    [else x]))

;; just because it comes up a lot
(provide split-last)
(define-syntax-rule (split-last xs)
  (let-values ([(first-list last-list) (split-at-right xs 1)])
    (values first-list (car last-list))))

;; like cons, but joins a list to an atom
(provide snoc)
(define-syntax-rule (snoc xs x)
  (append xs (list x)))


;; folded flonum operators 
;; (for use with multiple args, standard flonum ops have arity = 2)

(define-syntax (define-folded-op stx)
  (syntax-case stx ()
    [(_ op starting-val)
     (with-syntax ([fold-op (format-id stx "fold-~a" #'op)]
                   [ops (format-id stx "~as" #'op)])
       #'(begin
           (provide fold-op ops)
           (define-syntax-rule (ops x (... ...))
             (fold-op (list x (... ...))))
           (define-syntax-rule (fold-op xs)
             (foldl op starting-val xs))))]))

(define-folded-op fl+ 0.0)
(define-folded-op fl- 0.0)
(define-folded-op fl* 1.0)
(define-folded-op fl/ 1.0)