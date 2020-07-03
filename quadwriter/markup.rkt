#lang debug racket/base
(require "lang-helper.rkt"
         "tags.rkt"
         pollen/decode
         (only-in "markdown.rkt" doc-proc))
(provide #%top #%datum #%app #%top-interaction
         (all-from-out "tags.rkt")
         q)

(make-module-begin
 (λ (exprs)
   (doc-proc
    (decode-paragraphs exprs
                       #:force? #true
                       #:linebreak-proc (λ (x) (decode-linebreaks x '(line-break)))))))

(module reader racket/base
  (require "lang-helper.rkt")
  (provide read-syntax get-info)
  (define get-info get-info-texty)
  (define read-syntax
    (make-read-syntax 'quadwriter/markup
                      (λ (path-string ip)
                        (syntax->datum (quad-at-reader path-string ip))))))