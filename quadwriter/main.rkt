#lang debug racket/base
(require "core.rkt")
(provide (all-from-out "core.rkt"))

(module reader racket/base
  (require "lang-helper.rkt")
  (provide (rename-out [rs read-syntax]) get-info)
  (define rs (make-read-syntax 'quadwriter/lang
                               (λ (path ip)
                                 (for/list ([tok (in-port read ip)])
                                           tok)))))