#lang debug racket
(require "struct.rkt"
         quad/quad)
(provide (all-defined-out))

(define q:section (make-quad #:type section-quad
                             #:id 'section))
