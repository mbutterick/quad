#lang racket/base
(require "top.rkt")
(provide (except-out (all-from-out racket/base) #%top)
         (rename-out [~top #%top]))


(module reader syntax/module-reader
  #:language 'quad)