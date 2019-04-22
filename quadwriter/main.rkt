#lang debug racket/base
(require pollen/tag "lang-helper.rkt")
(provide #%top #%datum #%app #%top-interaction q)

(define q (default-tag-function 'q))
(define (doc-proc strs) (apply q strs))
(make-module-begin doc-proc)

(module reader racket/base
  (require "lang-helper.rkt")
  (provide read-syntax get-info)
  (define read-syntax (make-read-syntax 'quadwriter quad-at-reader)))