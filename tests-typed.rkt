#lang racket/base
(require "utils-typed.rkt" "quads-typed.rkt" "world-typed.rkt" racket/list racket/format)
(require rackunit)

(check-equal? (join-attrs (list (box '(width 10.0)) (quad-attrs (box '(x 10.0))) (list 'width 20.0))) 
              (list (cons 'width 10.0) (cons 'x 10.0) (cons 'width 20.0)))

(check-equal? (flatten-attrs (box '(foo bar)) (hash 'x 10.0)) (apply hash '(foo bar x 10.0)))
(check-equal? (flatten-attrs (hash 'x -5.0) (hash 'x 10.0)) (apply hash '(x 5.0)))
(check-equal? (merge-attrs (hash 'x -5.0) (hash 'x 10.0)) (apply hash '(x 10.0)))

(check-equal? (gather-common-attrs (list (box '(foo bar)) (box '(foo bar goo bar zam zino)) (box '(foo bar)))) (list (cons 'foo 'bar)))
(check-equal? (gather-common-attrs (list (box) (box '(foo bar goo bar zam zino)) (box '(foo bar)))) #f)
(check-equal? (gather-common-attrs (list (box '(width bar)) (box '(width bar)) (box '(width bar)))) #f)

(define b1 (box '(x 10.0) "1st" (box '(foo bar) "2nd") "3rd"))
(define b1-flattened (list (box '(x 10.0) "1st") (box '(x 10.0 foo bar) "2nd") (box '(x 10.0) "3rd")))

(define b3 (box #f (word) (line) (page)))
(check-true (sequence? b3))

(check-true (quad= (flatten-quad b1) b1-flattened))

(define b2 (box '(x 10.0) (spacer) (box '(x 15.0) (spacer) (spacer)) (spacer)))
(define b2-flattened (list (spacer '(x 10.0)) (spacer '(x 25.0)) (spacer '(x 25.0)) (spacer '(x 10.0))))

(check-true (quad= (flatten-quad b2) b2-flattened))
;(check-true (quad= (split-quad b2) b2-flattened))

