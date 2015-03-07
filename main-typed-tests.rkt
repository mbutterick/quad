#lang typed/racket/base
(require typed/rackunit)
(require "main-typed.rkt" "quads-typed.rkt" "world-typed.rkt")


(check-equal? (input->nested-blocks (input #f (block #f "1" (block-break) "2"))) 
              (list (list (list (list (quad 'word '#hash() '("1"))) (list (quad 'word '#hash() '("2")))))))
(check-equal? (input->nested-blocks (input #f (block #f "1" (column-break) "2"))) 
              (list (list (list (list (quad 'word '#hash() '("1")))) (list (list (quad 'word '#hash() '("2")))))))
(check-equal? (list (list (list (list (quad 'word '#hash() '("1"))))) (list (list (list (quad 'word '#hash() '("2"))))))
(input->nested-blocks (input #f (block #f "1" (page-break) "2"))))

(check-equal? (merge-adjacent-within (line #f (word #f "b") (word #f "a") (word #f "r"))) (line #f (word #f "bar")))

(check-equal? (hyphenate-quad-except-last-word (line #f "snowman" "snowman")) (line #f "snow\u00ADman" "snowman"))

(define al-test-line (line (list world:line-looseness-key 42.0) (word #f "bar")))
(define al-test-line2 (line (list world:line-looseness-key 30.0) (word #f "bar")))
(check-equal? (average-looseness (list)) 0.0) ; default value for no lines
(check-equal? (average-looseness (list al-test-line)) 0.0) ; default value for one line
(check-equal? (average-looseness (list al-test-line al-test-line2)) 42.0) ; last line excluded by default
(check-equal? (average-looseness (list al-test-line al-test-line2 al-test-line)) 36.0) ; last line excluded by default


(check-equal? (log-debug-lines (list (line (list world:line-looseness-key 42.0) (word #f "bar")))) '("0/1: \"bar\" 42.0"))


;; todo next: debug this line
(block->lines (block #f (word '(measure 50.0) "Meg is an ally.")))