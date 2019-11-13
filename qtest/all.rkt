#lang racket
(require quadwriter
         pitfall/check-pdf
         racket/runtime-path
         "paths-to-test.rkt")

(for ([test-path (in-list (test-paths))])
  (define pdf-path (path-replace-extension test-path #".pdf"))
  (define-values (dir name _) (split-path test-path))
  (displayln (path->string name))
  (check-pdfs-equal? (time (parameterize ([quadwriter-test-mode #t]
                                          [current-output-port (open-output-nowhere)])
                             (render-pdf (dynamic-require test-path 'doc) pdf-path test-path)
                             pdf-path))
                     (test-pdf-name test-path)))