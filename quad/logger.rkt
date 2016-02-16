#lang racket/base
(require (for-syntax racket/base racket/syntax) sugar/coerce racket/contract)
(require "world.rkt" racket/match sugar/debug racket/date racket/list)
(module+ test (require rackunit))
(provide (all-defined-out))


(define-syntax-rule (define-orphan-logger name)
  (begin
    (define remember-cl (current-logger))
    (define dummy-cl (make-logger))
    (current-logger dummy-cl)
    (define-logger name)
    (current-logger remember-cl)))

(define-logger quad)

(define-syntax-rule (activate-logger logger)
  (begin
    (define logger-receiver (make-log-receiver logger (world:logging-level))) 
    (define log-file (build-path (current-directory) (format "~a.txt" 'logger)))
    (with-output-to-file log-file #:exists 'truncate void)
    (void (thread
           (λ ()
             (let loop ()
               (match (sync logger-receiver)
                 [(vector event-level event-message event-value name)
                  (define msg (format "[~a] ~a\n" event-level event-message))
                  ;   (eprintf msg)
                  (flush-output)
                  (with-output-to-file log-file #:exists 'append (λ () (display msg)))])
               (loop))))
          (log-quad-info "started at ~a" (date->string (current-date) #t)))))


(define-syntax-rule (log-quad-debug-report x)
  (begin 
    (log-quad-debug "~a = ~a" 'x x)
    x))

(define-syntax-rule (log-quad-debug* xs)
  (when (equal? (world:logging-level) 'debug)
    (map (λ(x) (log-quad-debug x)) xs)))

(module+ main
  (activate-logger quad-logger)
  (log-quad-fatal "Exterminate! Exterminate!")
  (log-quad-error "~a" (time (apply + (range 1000))))
  (log-quad-debug "What's the red button for?"))

