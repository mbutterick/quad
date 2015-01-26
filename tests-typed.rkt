#lang racket/base
(require "utils.rkt" "wrap.rkt" "quads-typed.rkt" "world.rkt" racket/list racket/format)
(require rackunit)

(check-equal? (join-attrs (list (box '(width 10)) (box #f "foobar") (hash 'x 10) (list 'width 20))) 
              (list (cons 'width 10) (cons 'x 10) (cons 'width 20)))
