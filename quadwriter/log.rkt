#lang debug racket
(require racket/logging)
(provide (all-defined-out) with-logging-to-port)

;; creates `quadwriter-logger` and associated functions:
;; log-quadwriter-fatal, log-quadwriter-error, log-quadwriter-warning, 
;; log-quadwriter-info, and log-quadwriter-debug
(define-logger quadwriter)

(define-syntax-rule (time-log NAME EXPR)
  (let-values ([(res ms real-ms gc) (time-apply (λ () EXPR) null)])
    (log-quadwriter-info (format "~a: ~ams" 'NAME real-ms))
    (car res)))