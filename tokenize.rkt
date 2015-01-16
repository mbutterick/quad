#lang racket/base
(require racket/list sugar/define)
(require "samples.rkt" "quads.rkt" "utils.rkt")


(define tib (block '(measure 240 font "Equity Text B" leading 16 size 13.5 x-align justify x-align-last-line left)  (block #f (block '(weight bold font "Equity Caps B") "Hello") (block-break) (box '(width 15)))))

;ti

;; convert quad into tokenized representation:
;; 1) vector of atomic tokens
;; 2) list of (attribute + range of tokens it applies to)
;; this representation is designed to:
;; 1) preserve all information in the original quad
;; 2) be compact / not duplicate information unnecessarily
;; 3) allow sequential access to the tokens
;; 4) allow fast computation of token state (i.e., attrs that apply)
(define+provide (make-tokens-and-attrs quad-in)
  (define-values (all-tokens all-attrs _)
    (let loop ([current-quad quad-in][attr-acc empty][starting-tidx 0])
      (cond
        [(empty? (quad-list current-quad)); no subelements, so treat this quad as single token
         (let ([current-quad-attrs (quad-attrs current-quad)]
               [ending-tidx (add1 starting-tidx)])
           (values (quad (quad-name current-quad) #f empty)
                   (if current-quad-attrs
                       (cons (vector current-quad-attrs starting-tidx ending-tidx) attr-acc)
                       attr-acc)
                   ending-tidx))]
        [else ; replace quad with its tokens, exploded
         (define-values (tokens-from-fold subattrs-from-fold ending-tidx-from-fold)
           (for/fold ([token-acc empty][subattr-acc empty][tidx starting-tidx])
                     ([item (in-list (quad-list current-quad))])
             (cond
               [(quad? item)
                (define-values (sub-tokens sub-attrs sub-last-tidx) (loop item attr-acc tidx))
                (values (cons sub-tokens token-acc) (cons sub-attrs subattr-acc) sub-last-tidx)]
               [else ; item is a string of length > 0 (quad contract guarantees this)
                (define-values (exploded-chars last-idx-of-exploded-chars)
                  (for/fold ([chars empty][last-idx #f])([(c i) (in-indexed item)])
                    (values (cons c chars) i))) ; fold manually to get reversed items & length at same time
                (values (cons exploded-chars token-acc) subattr-acc (+ tidx last-idx-of-exploded-chars 1))])))
         (values tokens-from-fold
                 (let ([current-quad-attrs (quad-attrs current-quad)])
                   (if current-quad-attrs
                       (cons (vector current-quad-attrs starting-tidx ending-tidx-from-fold) subattrs-from-fold)
                       subattrs-from-fold))
                 ending-tidx-from-fold)])))
  (values (list->vector (reverse (cons (current-eof) (flatten all-tokens)))) (flatten all-attrs)))



(define+provide current-tokens (make-parameter #f))
(define+provide current-token-attrs (make-parameter #f))
(define+provide current-eof (make-parameter (gensym)))
(define+provide (eof? x) (equal? x (current-eof)))

(define+provide (quad->current-tokens q)
  (define-values (tokens attrs) (make-tokens-and-attrs q))
  (current-tokens tokens)
  (current-token-attrs attrs))

;(filter (λ(idx) (box? (vector-ref tokens idx))) (range (vector-length tokens)))

(define (attr-ref-hash a) (vector-ref a 0))
(define (attr-ref-start a) (vector-ref a 1))
(define (attr-ref-end a) (vector-ref a 2))

(define (calc-attrs tref)
  (map attr-ref-hash (filter (λ(attr) (<= (attr-ref-start attr) tref (sub1 (attr-ref-end attr)))) (current-token-attrs))))
