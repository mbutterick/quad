#lang sugar/debug racket

(define 1-out
 (block '(measure 240.0 font "Times New Roman" leading 16.0 vmeasure 300.0 size 13.5 x-align justify x-align-last-line left) (box '(width 15.0)) (block '() (block '(weight bold) "Hot " (word '(size 22.0) "D") "ang, My Fellow Americans.") " This " (block '(no-break #t) "is some truly") " nonsense generated from my typesetting system, which is called Quad. I’m writing this in a source file in DrRacket. When I click [Run], a PDF pops out. Not bad\u200a—\u200aand no LaTeX needed. Quad, however, does use the fancy linebreaking algorithm developed for TeX. (It also includes a faster linebreaking algorithm for when speed is more important than quality.) Of course, it can also handle " (block '(font "Courier") "different fonts,") (block '(style italic) " styles, ") (word '(size 14.0 weight bold) "and sizes-") " within the same line. As you can see, it can also justify paragraphs.")))

(require quad/typeset quad/quads quad/render)

;(time (send (new pdf-renderer%) render-to-file (typeset 1-out) "f1-test.pdf"))

(require (prefix-in 2- "foo2.rkt"))
(time (send (new pdf-renderer%) render-to-file (typeset 2-out) "f2-test.pdf"))

(require (prefix-in 3- "foo3.rkt"))
;(time (send (new pdf-renderer%) render-to-file (typeset 3-out) "f3-test.pdf"))
