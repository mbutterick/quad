#lang debug racket/base
(require pollen/tag "lang-helper.rkt")
(provide (except-out (all-from-out racket/base) #%module-begin))

(define (doc-proc strs) (apply q strs))
(make-module-begin doc-proc)

