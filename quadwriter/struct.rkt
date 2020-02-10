#lang debug racket
(require quad/quad)
(provide (all-defined-out))

(define-quad break-quad)
(define-quad line-break-quad break-quad)
(define-quad para-break-quad line-break-quad)
(define-quad hr-break-quad line-break-quad)
(define-quad column-break-quad line-break-quad)
(define-quad page-break-quad column-break-quad)
(define-quad section-break-quad page-break-quad)

(define-quad line-quad)
(define-quad line-spacer-quad line-break-quad)

(define-quad filler-quad)
(define-quad offsetter-quad)

(define-quad column-quad)
(define-quad column-spacer-quad)

(define-quad page-quad)

(define-quad doc-quad)

(define-quad section-quad)

(define-quad block-quad)

(define-quad first-line-indent-quad)

(define-quad string-quad)
(define-quad image-quad)
(define-quad draw-quad)
