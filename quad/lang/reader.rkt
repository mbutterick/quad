#lang s-exp syntax/module-reader
quad/lang/quad
#:read quad-read
#:read-syntax quad-read-syntax
#:whole-body-readers? #t ;; need this to make at-reader work
(require scribble/reader)

(define (quad-read p)
  (syntax->datum (quad-read-syntax (object-name p) p)))

(define (quad-read-syntax path-string p)
  (define quad-at-reader (make-at-reader
                          #:command-char #\◊
                          #:syntax? #t 
                          #:inside? #t))
  (define source-stx (quad-at-reader path-string p))
  (println (syntax->datum source-stx))
  source-stx)