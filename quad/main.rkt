#lang racket/base
(require "quads.rkt")
(provide (all-from-out racket/base "quads.rkt"))


(module reader racket/base
  (require br/reader-utils "parse.rkt" "tokenize.rkt")
  
  (define-read-and-read-syntax (source-path input-port)
    #`(module quad-mod 
        #,(parse source-path (tokenize input-port)))))
