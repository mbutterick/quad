#lang debug racket/base
(require "core.rkt")
(provide (all-from-out "core.rkt"))

(module docmod racket/base
  (define doc 'just-for-label)
  (provide doc))
(require (for-label 'docmod))
(provide (for-label doc)) ; stub for scribble labels

(module reader racket/base
  (require "lang-helper.rkt")
  (provide (rename-out [rs read-syntax]) get-info)
  (define rs (make-read-syntax 'quadwriter/lang
                               (λ (path ip)
                                 (for/list ([tok (in-port read ip)])
                                           tok)))))