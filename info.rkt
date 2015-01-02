#lang info
(define collection "quad")
(define deps '(("base" #:version "6.0") "sugar" "csp" "rackunit-lib" "hyphenate"))
(define update-implies '("sugar"))
(define compile-omit-paths '("tests.rkt" "tests-ocm.rkt"))
