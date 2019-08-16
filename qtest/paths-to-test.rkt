#lang racket/base
(require racket/runtime-path)
(provide (all-defined-out))

(define-runtime-path here ".")

(define (test-paths)
  (for/list ([test-path (in-directory here)]
             #:when (regexp-match #rx"test-.*.rkt$" (path->string test-path)))
    test-path))


(define (test-pdf-name path)
  (path-add-extension (path-replace-extension path #".pdf") #"" #"-tester."))