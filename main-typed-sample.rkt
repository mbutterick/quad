#lang typed/racket/base
(require "main-typed.rkt" "logger-typed.rkt" "world-typed.rkt" "samples-typed.rkt" "quads-typed.rkt")

(require/typed contract-profile
                [contract-profile-thunk ((-> Any) . -> . Quad)])

(require "render-typed.rkt" racket/class optimization-coach)
(activate-logger quad-logger)

(parameterize ([world:quality-default world:draft-quality]
               [world:paper-width-default 600.0]
               [world:paper-height-default 700.0])
  (define sample (ti5))
  (define to (time (typeset sample)))
  (time (send (new pdf-renderer%) render-to-file to "foo-typed.pdf")))