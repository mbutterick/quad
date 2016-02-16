#lang racket/base

(require "main.rkt"  "world.rkt" "quick-sample.rkt"
           "render.rkt" racket/class)
  (parameterize ([world:quality-default world:draft-quality])
    (displayln "Untyped Quad")
    (displayln "Typesetting:")
    (define to (time (typeset (quick-sample))))
    (displayln "PDF rendering:")
    (time (send (new pdf-renderer%) render-to-file to "quick-test-untyped.pdf")))