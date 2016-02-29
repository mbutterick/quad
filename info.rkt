#lang info
(define collection 'multi)
(define deps '(("base" #:version "6.0") "sugar" "csp" "rackunit-lib" "hyphenate" "at-exp-lib" "data-lib" "draw-lib" "gui-lib" "math-lib" "plot-gui-lib" "plot-lib" "profile-lib" "typed-racket-lib"))
(define build-deps '("racket-doc" "scribble-lib"))
(define update-implies '("sugar"))
