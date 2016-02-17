#lang racket

(module foo quad/lang/quad
  (block '(measure 240.0 font "Times New Roman" leading 16.0 vmeasure 300.0 size 13.5 x-align justify x-align-last-line left) (box '(width 15.0)) (block '() (block '(weight bold) "Hot " (word '(size 22.0) "D") "ang, My Fellow Americans."))))

(require (prefix-in o: 'foo))
(require quad quad/quads quad/render)

(quad? o:out)
(define o:ts (typeset o:out))
(time (send (new pdf-renderer%) render-to-file o:ts "foo-test.pdf"))
(require "foo2.rkt")
(define ts (typeset out))
(time (send (new pdf-renderer%) render-to-file ts "foo2-test.pdf"))
