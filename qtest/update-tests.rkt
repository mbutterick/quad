#lang racket/base
(require quadwriter
         "paths-to-test.rkt")

(for ([test-path (in-list (test-paths))])
  (define-values (dir name _) (split-path test-path))
  (displayln (path->string name))
  (time (parameterize ([quadwriter-test-mode #t])
          (render-pdf (dynamic-require test-path 'doc) (test-pdf-name test-path) test-path #:compress #false))))