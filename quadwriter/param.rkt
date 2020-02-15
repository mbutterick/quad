#lang debug racket
(provide (all-defined-out))
(define current-doc (make-parameter #false))

(define current-pdf (make-parameter #false))
(define current-line-wrap (make-parameter #f)) ; because kp is slow and maybe we want to disable for "draft" mode
(define section-pages-used (make-parameter 0))
         
(define quadwriter-test-mode (make-parameter #f)) ; used during rackunit to suppress nondeterministic elements, like timestamp in header

(define zoom-factor (make-parameter 1))
(define log-clipping? (make-parameter 'warn))