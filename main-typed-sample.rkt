#lang typed/racket/base/no-check
(require typed/sugar/debug)
(require "main-typed.rkt" "logger-typed.rkt" "world-typed.rkt" "samples-typed.rkt" "quads-typed.rkt")

(require/typed profile
               [profile-thunk (All (A) ((-> A) [#:delay Float] -> A))])

(require "render-typed.rkt" typed/racket/class)
(activate-logger quad-logger)

(parameterize ([world:quality-default world:draft-quality]
               [world:paper-width-default 600.0]
               [world:paper-height-default 700.0])
  (define sample (ti3))
  ;  (define to (time (profile-thunk #:delay 0.001 (λ () (typeset sample)))))
  (define to (time (typeset sample)))
  to
  (time (send (new pdf-renderer%) render-to-file to "foo-typed.pdf")))