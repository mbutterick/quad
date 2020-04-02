#lang info
(define collection 'multi)
(define deps '("at-exp-lib"
               "base"
               "beautiful-racket-lib"
               "fontland"
               "hyphenate"
               "pitfall"
               "pollen"
               "rackunit-lib"
               "sugar"
               "txexpr"
               "markdown"
               "pict-lib"
               "debug"))
(define build-deps '("draw-lib"
                     "draw-doc"
                     "racket-doc"
                     "scribble-lib"))
(define update-implies '("fontland" "pitfall"))