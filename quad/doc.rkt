#lang quad/dev
(require sugar/list)
(provide (all-defined-out))

(struct $doc (xs) #:transparent)
(define (doc . xs) ($doc xs))

(define (multipage . xs) xs)

(define (multicolumn . xs) xs)

(define (multiblock . xs) xs)

(define (multiline . xs)
  (break-lines xs))

(struct $line (xs) #:transparent)
(define (break-lines xs)
  (map (λ(xis) ($line xis)) (slice-at xs 6)))
