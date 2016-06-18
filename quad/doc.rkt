#lang quad/dev
(require sugar/list)
(provide (all-defined-out))

(struct $doc (xs) #:transparent)
(define (doc . xs) ($doc xs))

(define (multipage . xs) xs)

(define (multicolumn . xs) xs)

(define (multiblock . xs) xs)

(define (multiline . xs)
  (wrap-lines xs))

(struct $line $quad () #:transparent)
(define (wrap-lines xs)
  (map (λ(xis) ($line (gather-common-attrs xis) xis)) (slice-at xs 6)))
