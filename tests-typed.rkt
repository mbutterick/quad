#lang racket/base
(require "utils-typed.rkt" "quads-typed.rkt" "world-typed.rkt" racket/list racket/format)
(require rackunit)

(check-equal? (join-attrs (list (box '(width 10.0)) (quad-attrs (box '(x 10.0))) (list 'width 20.0))) 
              (list (cons 'width 10.0) (cons 'x 10.0) (cons 'width 20.0)))

(check-equal? (flatten-attrs (box '(foo bar)) (hash 'x 10.0)) (apply hash '(foo bar x 10.0)))
(check-equal? (flatten-attrs (hash 'x -5.0) (hash 'x 10.0)) (apply hash '(x 5.0)))
(check-equal? (merge-attrs (hash 'x -5.0) (hash 'x 10.0)) (apply hash '(x 10.0)))
