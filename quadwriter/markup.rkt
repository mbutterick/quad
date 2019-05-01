#lang debug racket/base
(require "lang-helper.rkt"
         "tags.rkt"
         (only-in "markdown.rkt" doc-proc))
(provide #%top #%datum #%app #%top-interaction
         (all-from-out "tags.rkt")
         q)

(make-module-begin doc-proc)

(module reader racket/base
  (require "lang-helper.rkt" pollen/decode)
  (provide read-syntax get-info)
  (define read-syntax
    (make-read-syntax 'quadwriter/markup
                      (λ (path-string ip)
                        (detect-paragraphs (syntax->datum (quad-at-reader path-string ip)))))))