#lang racket
(require "quads.rkt")

;; push together multiple attr sources into one list of pairs.
;; mostly a helper function for the two attr functions below.
#;(define (join-attrs quads-or-attrs-or-lists)
    (append-map hash->list (filter-not false? (map (λ(x)
                                                     (cond
                                                       [(quad? x) (quad-attrs x)]
                                                       [(quad-attrs? x) x]
                                                       #;[(hashable-list? x) (apply hash x)]
                                                       [else #f])) quads-or-attrs-or-lists))))


;; flatten merges attributes, but applies special logic suitable to flattening
;; for instance, resolving x and y coordinates.
#;(define (flatten-attrs . quads-or-attrs-or-falses)
    (define all-attrs (join-attrs quads-or-attrs-or-falses))
    (define-values (x-attrs y-attrs other-attrs-reversed)
      (for/fold ([xas null][yas null][oas null])([attr (in-list all-attrs)])
        (cond
          [(equal? (car attr) 'x) (values (cons attr xas) yas oas)]
          [(equal? (car attr) 'y) (values xas (cons attr yas) oas)]
          [else (values xas yas (cons attr oas))])))
    (define (make-cartesian-attr key attrs) (if (empty? attrs) empty (cons key (apply + (map cdr attrs)))))
    (define-values (x-attr y-attr) (apply values (map make-cartesian-attr (list 'x 'y) (list x-attrs y-attrs))))
    (apply hash (flatten (list* x-attr y-attr (reverse other-attrs-reversed)))))

;; pushes attributes down from parent quads to children, 
;; resulting in a flat list of quads.
#;(define (flatten-quad q)
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



(require sugar/debug)
;; flatten quad as above, 
;; then dissolve it into individual character quads while copying attributes

#;(define (split-quad q)
    (letrec ([do-explode (λ(x [parent #f])
                           (cond
                             [(quad? x)
                              (if (empty? (quad-list x))
                                  x ; no subelements, so stop here
                                  (map (λ(xi) (do-explode xi x)) (quad-list x)))] ; replace quad with its elements, exploded
                             ;; todo: figure out why newlines foul up the input stream. Does it suffice to ignore them? 
                             [else (map (λ(xc) (quad 'atom (quad-attrs parent) (list xc))) (regexp-match* #px"[^\r\n]" x))]))])
      (flatten (map do-explode (flatten-quad q)))))

(require (for-syntax syntax/strip-context sugar/debug))
(define-for-syntax ctx #'here)
(define-syntax (stx-quad stx)
  (syntax-case stx ()
    [(_ QUAD-NAME ((ATTR-NAME ATTR-VAL) ...) XS)
     (with-syntax ([(NEW-ATTR-NAME ...) (map (λ(an) (datum->syntax stx (syntax->datum an))) (syntax->list #'(ATTR-NAME ...)))]
                   [(ALL-ATTR-NAME ...) (map (λ(n) (datum->syntax stx n)) '(size font))])
       #'(let ([NEW-ATTR-NAME ATTR-VAL] ...)
           (append-map (λ(x) (if (string? x)
                                 (for/list ([c (in-string x)])
                                           (vector ALL-ATTR-NAME ... c))
                                 x)) XS)))]))


(require racket/generator)
#;(define y (generator () (stx-quad ((size 10)) (list "bar" (stx-quad () '("zam"))))))

#|
(define x (quad 'foo (hash 'size 10) (list "bar" (quad 'foo (hash 'size 8) '("zam")) "qux")))
(split-quad x)
|#

;(define x2 (stx-quad ((size 10)(font "Eq")) (list "bar" (stx-quad ((size 8)) '("zam")) "qux")))
;x2

(stx-quad 'foo ((size 10)(font "Eq")) (list "bar" (stx-quad 'foo ((size 8)) '("zam")) "qux"))