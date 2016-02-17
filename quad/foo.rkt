#lang racket

(module foo quad/lang/quad
 (block '(measure 240.0 font "Times New Roman" leading 16.0 vmeasure 300.0 size 13.5 x-align justify x-align-last-line left) (box '(width 15.0)) (block '() (block '(weight bold) "Hot " (word '(size 22.0) "D") "ang, My Fellow Americans.") " This " (block '(no-break #t) "is some truly") " nonsense generated from my typesetting system, which is called Quad. I’m writing this in a source file in DrRacket. When I click [Run], a PDF pops out. Not bad\u200a—\u200aand no LaTeX needed. Quad, however, does use the fancy linebreaking algorithm developed for TeX. (It also includes a faster linebreaking algorithm for when speed is more important than quality.) Of course, it can also handle " (block '(font "Courier") "different fonts,") (block '(style italic) " styles, ") (word '(size 14.0 weight bold) "and sizes-") " within the same line. As you can see, it can also justify paragraphs." (block-break) (box '(width 15.0)) (block '() "“Each horizontal row represents " (box '(color "Red" background "Yellow") "an OS-level thread,") " and the colored dots represent important events in the execution of the program (they are color-coded to distinguish one event type from another). The upper-left blue dot in the timeline represents the future’s creation. The future executes for a brief period (represented by a green bar in the second line) on thread 1, and then pauses to allow the runtime thread to perform a future-unsafe operation.") (column-break)  (box '(width 15.0))(block '() "In the Racket implementation, future-unsafe operations fall into one of two categories. A blocking operation halts the evaluation of the future, and will not allow it to continue until it is touched. After the operation completes within touch, the remainder of the future’s work will be evaluated sequentially by the runtime thread. A synchronized operation also halts the future, but the runtime thread may perform the operation at any time and, once completed, the future may continue running in parallel. Memory allocation and JIT compilation are two common examples of synchronized operations." (page-break) "another page"))))

(require (prefix-in 1- 'foo))
(require quad quad/quads quad/render)

(time (send (new pdf-renderer%) render-to-file (typeset 1-out) "foo-test.pdf"))

(require (prefix-in 2- "foo2.rkt"))
(time (send (new pdf-renderer%) render-to-file (typeset 2-out) "foo2-test.pdf"))

(require (prefix-in 3- "foo3.rkt"))
(time (send (new pdf-renderer%) render-to-file (typeset 3-out) "foo3-test.pdf"))
