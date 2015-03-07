#lang typed/racket/base
(require typed/rackunit)
(require "main-typed.rkt" "quads-typed.rkt")


(check-equal? (input->nested-blocks (input #f (block #f "1" (block-break) "2"))) 
              (list (list (list (list (quad 'word '#hash() '("1"))) (list (quad 'word '#hash() '("2")))))))
(check-equal? (input->nested-blocks (input #f (block #f "1" (column-break) "2"))) 
              (list (list (list (list (quad 'word '#hash() '("1")))) (list (list (quad 'word '#hash() '("2")))))))
(check-equal? (list (list (list (list (quad 'word '#hash() '("1"))))) (list (list (list (quad 'word '#hash() '("2"))))))
(input->nested-blocks (input #f (block #f "1" (page-break) "2"))))

(check-equal? (merge-adjacent-within (line #f (word #f "b") (word #f "a") (word #f "r"))) (line #f (word #f "bar")))

(check-equal? (hyphenate-quad-except-last-word (line #f "snowman" "snowman")) (line #f "snow\u00ADman" "snowman"))