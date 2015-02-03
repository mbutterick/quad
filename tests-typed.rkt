#lang racket/base
(require "utils-typed.rkt" "quads-typed.rkt" "world-typed.rkt" racket/list racket/format)
(require rackunit)

(check-equal? (join-attrs (list (box '(width 10)) (quad-attrs (box '(x 10))) (list 'width 20))) 
              (list (cons 'width 10) (cons 'x 10) (cons 'width 20)))
