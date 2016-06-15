#lang racket/base
(require "typeset.rkt" "samples.rkt" "render.rkt" "world.rkt" racket/class "logger.rkt")

(module+ main
  (define line-limit (with-handlers ([exn:fail? (λ(exn) #f)])
                       (string->number (vector-ref (current-command-line-arguments) 0))))
  (activate-logger quad-logger)
  (parameterize ([world:quality-default world:max-quality]
                 [world:paper-width-default 412]
                 [world:paper-height-default 600])
    (define path "texts/segfault.txt")
    (displayln "Making text sample")
    (define text-sample (time (make-sample path line-limit)))
    (displayln "Typsetting sample")
    (define typeset-sample (time (typeset text-sample)))
    (displayln "Rendering sample to PDF")
    (time (send (new pdf-renderer%) render-to-file typeset-sample "texts/segfault.pdf"))))