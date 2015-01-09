#lang racket/base
(require racket/list sugar)
(require "samples.rkt" "quads.rkt" "utils.rkt")

(define ti (block '(measure 54 leading 18) "Meg is " (box '(foo 42)) " ally."))

;ti


(define (tokenize-quad0 q)
  (define-values (all-tokens last-tidx)
    (let loop ([q q][starting-tidx 0])
      (for/fold ([token-list empty][tidx starting-tidx])
                ([item (in-list (quad-list q))])
        (cond
          [(quad? item)
           (define-values (sub-token-list sub-last-tidx) (loop item tidx))
           (values (cons sub-token-list token-list) sub-last-tidx)]
          [(string? item)
           (define atoms (regexp-match* #rx"." item))
           (values (cons atoms token-list) (+ tidx (length atoms)))]
          [else (values (cons item token-list) (+ tidx 1))]))))
  (values (list->vector (flatten (reverse all-tokens))) last-tidx))


(define (tokenize-quad quad-in)
  (define-values (all-tokens all-attrs last-tidx)
    (let loop ([current-quad quad-in][attr-acc empty][starting-tidx 0])
      (cond
        [(empty? (quad-list current-quad)) ; no subelements, so treat this quad as single token
         (values (quad (quad-name current-quad) #f empty)
                 (if (quad-attrs current-quad)
                     (cons (vector (quad-attrs current-quad) starting-tidx (add1 starting-tidx)) attr-acc)
                     attr-acc)
                 (add1 starting-tidx))]
        [else ; replace quad with its tokens, exploded
         (define-values (tokens-from-fold subattrs-from-fold last-tidx-from-fold)
           (for/fold ([token-acc empty][subattr-acc empty][tidx starting-tidx])
                     ([item (in-list (quad-list current-quad))])
             (cond
               [(quad? item)
                (define-values (sub-tokens sub-attrs sub-last-tidx) (loop item attr-acc tidx))
                (values (cons sub-tokens token-acc) (cons sub-attrs subattr-acc) sub-last-tidx)]
               [(string? item)
                (define atoms (regexp-match* #rx"." item))
                (values (cons atoms token-acc) subattr-acc (+ tidx (length atoms)))]
               [else 
                (values (cons item token-acc) subattr-acc (+ tidx 1))])))
         (values tokens-from-fold
                 (if (quad-attrs current-quad)
                     (cons (vector (quad-attrs current-quad) starting-tidx last-tidx-from-fold) subattrs-from-fold)
                     subattrs-from-fold)
                 last-tidx-from-fold)])))
  (values (list->vector (flatten (reverse all-tokens))) (flatten (reverse all-attrs))))


(define-values (tokens attrs) (tokenize-quad (ti2)))
tokens
attrs
(filter (λ(idx) (box? (vector-ref tokens idx))) (range (vector-length tokens)))
