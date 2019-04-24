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
               "csp"))
(define build-deps '("draw-lib"
                     "racket-doc"
                     "scribble-lib"
                     "debug"))
(define update-implies '("fontland" "pitfall"))
(define test-omit-paths 'all)
(define compile-omit-paths '("qtest"))