#lang debug br/quicklang
(require racket/promise "quad.rkt" "atomize.rkt" "break.rkt")
(provide (rename-out [mb #%module-begin]))

(define optional-break? (λ (q) (and (quad? q) (memv (car (qe q)) '(#\space)))))
(struct $slug $quad () #:transparent)
(define (slug . xs) ($slug #f xs))
(define (lbs xs size [debug #f])
  (insert-breaks xs size debug
                 #:break-val 'lb
                 #:optional-break-proc optional-break?
                 #:size-proc (λ (q) (let ([val (hash-ref (qa q) 'size (λ ()
                                                                        (if (memv (car (qe q)) '(#\space))
                                                                            (delay (values 0 1 0))
                                                                            (delay (values 1 1 1)))))])
                                      (if (promise? val) (force val) (val))))
                 #:finish-segment-proc (λ (pcs) (list ($slug #f pcs)))))

(define (pbs xs size [debug #f])
    (insert-breaks xs size debug
                   #:break-val 'pb
                   #:optional-break-proc (λ (x) (eq? x 'lb))
                   #:size-proc (λ (q) (case q
                                        [(lb) (values 0 0 0)]
                                        [else (values 1 1 1)]))))

(define (typeset args)
  (pbs (lbs (atomize (apply quad #f args)) 5) 2))

(define-syntax-rule (mb lang-line-config-arg . args)
  (#%module-begin
   (typeset (list . args))))

(module reader syntax/module-reader
  quad/typewriter
  #:read quad-read
  #:read-syntax quad-read-syntax
  #:whole-body-readers? #t ;; need this to make at-reader work
  (require scribble/reader)
  
  (define (quad-read p) (syntax->datum (quad-read-syntax (object-name p) p)))
  
  (define (quad-read-syntax path-string p)
    (define quad-at-reader (make-at-reader
                            #:syntax? #t 
                            #:inside? #t))
    (quad-at-reader path-string p)))