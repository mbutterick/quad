#lang racket/base
(provide (all-defined-out))
(require (for-syntax racket/string racket/base racket/syntax))

(struct $quad (attrs val) #:transparent #:mutable)
(struct $black $quad () #:transparent #:mutable)
(struct $white $quad () #:transparent #:mutable)
(struct $skip $quad () #:transparent #:mutable)
(struct $shim $quad () #:transparent #:mutable)

(define quad? $quad?)

(define quad-attrs $quad-attrs)
(define quad-val $quad-val)

(define (quad-attrs? x) (list? x))


#|
Attrs needed to specify rendered appearance:
(font) family
(font) style 
(font) size
color
background
position
measure (line width)

|#
(define default-attrs (vector 12 "Courier" 0))
(define (quad attr . xs)
  ($quad (or attr (attrs)) xs))

(define (attrs #:size [size #f]
               #:font [font #f]
               #:posn [posn #f])
  (vector size font posn))


(define (quad-posn q)
  (vector-ref ($quad-attrs q) 2))

(define (quad-posn-set! q val)
  (vector-set! ($quad-attrs q) 2 val))

(define (override-with dest source)
  ;; replace missing values in dest with values from source
  (for ([i (in-range (vector-length source))])
       (unless (vector-ref dest i)
         (vector-set! dest i (vector-ref source i))))
  dest)

(require (for-syntax sugar/debug))
(define-syntax (define-break stx)
  (syntax-case stx ()
    [(_ name)
     (with-syntax ([BREAK-NAME (string->symbol (string-upcase (symbol->string (syntax->datum #'name))))])
       #'(define (name) (quad #f 'BREAK-NAME)))]))

(define-break page-break)
(define-break column-break)
(define-break block-break)
(define-break line-break)


(module+ test
  (require rackunit)
  (define q (quad #f "bar"))
  (check-true (quad? q))
  (check-false (quad? 42))
  (check-equal? (quad-attrs q) (attrs))
  (check-equal? (quad-val q) '("bar")))