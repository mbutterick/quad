#lang racket/base
(require racket/logging)
(provide (all-defined-out))

;; creates `quad-logger` and associated functions:
;; log-quad-fatal, log-quad-error, log-quad-warning, 
;; log-quad-info, and log-quad-debug
(define-logger quad)