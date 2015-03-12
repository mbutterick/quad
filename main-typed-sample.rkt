#lang typed/racket/base
(require "main-typed.rkt" "logger-typed.rkt" "world-typed.rkt")

(require "render.rkt" racket/class profile)
(require "samples.rkt")
(activate-logger quad-logger)
(parameterize ([world:quality-default world:draft-quality]
               [world:paper-width-default 600.0]
               [world:paper-height-default 700.0])
  (define sample (ti5))
  sample
  #;(define to (begin (time (typeset sample))))
#;  (time (send (new pdf-renderer%) render-to-file to "foo.pdf")))