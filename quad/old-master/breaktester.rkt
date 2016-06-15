#lang racket
(require hyphenate "quads.rkt" "world.rkt" "render.rkt" "typeset.rkt" "utils.rkt")

(define (make-test-blocks string)
  (let ([string string])
    (add-between (list
   (block '(quality 100 x-align justify) string)
;   (block '(quality 100 x-align justify) string)
   ) (block-break))))
   
         
(define test-block (block '(font "Equity Text B" measure 265 leading 8 size 10 x-align-last-line left) (apply block #f
       (make-test-blocks (hyphenate "“This is a PDF generated from my typesetting system, which is called Quad. I’m writing this in a source file in DrRacket. When I click “Run”, a PDF pops out. Not bad — and no LaTeX needed. Quad, however, does use the fancy linebreaking algorithm developed for TeX. (It also includes a faster linebreaking algorithm for when speed is more important than quality.) This tutorial provides a brief introduction to the Racket programming language by using one of its picture-drawing libraries. Even if you don’t intend to use Racket for your artistic endeavours, the picture library supports interesting and enlightening examples. After all, a picture is worth five hundred “hello world”s.")))))
                          ;(block-break) (block #f (~a (random)))))

(define ti5 (block '(font "Equity Text B" x-align justify x-align-last-line left) (block #f (block '(x-align-last-line center weight bold font "Equity Caps B") "Greetings" (block-break) "Matthew & Robby!") (block-break) (block #f " This is a PDF generated from my Racket typesetting language, which is called " (word '(style italic) "Quad.") " I’m writing this in a source file in DrRacket. When I click “Run”, a PDF pops out. Not bad — and no LaTeX needed." (block-break) (box '(width 15)) (word '(font "Concourse T2") "Quad takes some of the good ideas from TeX, like its fancy algorithm for breaking paragraphs into lines. Though respectfully to Prof. Knuth, I believe I’ve even improved it.") (block-break) (box '(width 15)) "Of course, Quad can also handle " (word '(font "Avenir") "different fonts,") (word '(style italic) " styles, ") (word '(size 14 weight bold) "and sizes") " within the same line. As you can see, it can also justify paragraphs. (This sample also uses the new fractional point sizes.)" (block-break) (box '(width 15)) "Truly, the combination of Lisp and typesetting is an unprecedented confluence of geekery. I hope that Quad can become a useful part of the Racket ecosystem." (block-break) (word '(x-align-last-line center weight bold font "Equity Caps B") "thank you for your help" (block-break) "mb" )))))

(define ti6 (block #f (apply block '(column-count 3 column-gutter 15 measure 170) (add-between (map (λ(r) (quad-attr-set* ti5 'size r 'leading (* 1.25 r))) (range 8.5 13 .5)) (column-break))) 
                   (page-break) 
                   (apply block '(column-count 2 column-gutter 25 measure 240) (add-between (map (λ(r) (quad-attr-set* ti5 'size r 'leading (* 1.25 r))) (range 10 14 .5)) (column-break))) 
                   (page-break) 
                   (apply block '(column-count 1 column-gutter 15 measure 360) (add-between (map (λ(r) (quad-attr-set* ti5 'size r 'leading (* 1.25 r))) (range 15 18)) (column-break)))))

(parameterize ([world:quality-default world:max-quality]
               [world:paper-width-default 792]
               [world:paper-height-default 612])
  (send (new pdf-renderer%) render-to-file (time (typeset ti6)) "foo.pdf")  
  )