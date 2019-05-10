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
               "csp"))
(define build-deps '("draw-lib"
                     "draw-doc"
                     "racket-doc"
                     "scribble-lib"
                     "debug"))
(define update-implies '("fontland" "pitfall"))