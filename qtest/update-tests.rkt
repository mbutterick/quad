#lang racket/base
(require quadwriter "paths-to-test.rkt")

(for-each update-test-pdf (find-test-paths))
