#lang racket/base
(require racket/runtime-path
         racket/match
         quadwriter)
(provide (all-defined-out))

(define-runtime-path here ".")

(define (find-test-paths)
  (for/list ([test-path (in-directory here)]
             #:when (regexp-match #rx"test-.*.rkt$" (path->string test-path)))
            test-path))


(define (test-pdf-name path)
  (path-add-extension (path-replace-extension path #".pdf") #"" #"-tester."))

(define (update-test-pdf test-path-arg)
  (define test-path (match test-path-arg
                      [(? absolute-path? ap) ap]
                      [rp (build-path here rp)]))
  (define-values (dir name _) (split-path test-path))
  (displayln (path->string name))
  (time (parameterize ([quadwriter-test-mode #t])
          (render-pdf (dynamic-require test-path 'doc) (test-pdf-name test-path) test-path))))