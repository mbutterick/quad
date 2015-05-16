#lang racket/base

(module quick-test-typed typed/racket/base
  (require "main-typed.rkt" "world-typed.rkt" "quick-sample-typed.rkt"
           "render-typed.rkt" typed/racket/class)
  (parameterize ([world:quality-default world:draft-quality])
    (displayln "Typed Quad")
    (displayln "Typesetting:")
    (define to (time (typeset (quick-sample))))
    (displayln "PDF rendering:")
    (time (send (new pdf-renderer%) render-to-file to "quick-test-typed.pdf"))))

(module quick-test-untyped racket/base
  (require "main.rkt"  "world.rkt" "quick-sample.rkt"
           "render.rkt" racket/class)
  (parameterize ([world:quality-default world:draft-quality])
    (displayln "Untyped Quad")
    (displayln "Typesetting:")
    (define to (time (typeset (quick-sample))))
    (displayln "PDF rendering:")
    (time (send (new pdf-renderer%) render-to-file to "quick-test-untyped.pdf"))))

(require 'quick-test-typed)
(require 'quick-test-untyped)