#lang debug racket/base
(require racket/logging
         racket/match
         "log.rkt")

(with-logging-to-port
    (current-error-port)
  (λ ()
    (match (with-handlers ([exn:fail? (λ (exn) #f)])
             (vector-ref (current-command-line-arguments) 0))
      ["test" (dynamic-require 'qtest/all-tests #f)]
      ["update" (dynamic-require 'qtest/update-tests #f)]
      [_ (displayln "no cmd given")]))
  #:logger qtest-logger
  'info
  'qtest)