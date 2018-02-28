#lang debug br/quicklang
(require racket/promise racket/list sugar/list sugar/debug "quad.rkt" "atomize.rkt" "wrap.rkt" "qexpr.rkt" "generic.rkt" "position.rkt")
(require pitfall/document)
(provide (rename-out [mb #%module-begin]) (except-out (all-from-out br/quicklang) #%module-begin))

(define optional-break? (λ (q) (and (quad? q) (memv (car (elems q)) '(#\space #\-)))))
(struct $shim $quad () #:transparent)
(struct $char $quad () #:transparent)
(define (charify q) ($char (hash-set* (attrs q) 'size (const '(7.2 12))
                                      'draw (λ (q doc) (send/apply doc text (apply string (elems q)) (origin q)))) (elems q)))
(struct $line $quad () #:transparent)
(struct $page $quad () #:transparent)
(struct $doc $quad () #:transparent)
(struct $break $quad () #:transparent)
(define (break . xs) ($break (hasheq 'size '(0 0)) xs))

(define line-height 16)
(define (line-wrap xs size [debug #f])
  (wrap xs size debug
        #:break-val (break #\newline)
        #:optional-break-proc optional-break?
        #:finish-segment-proc (λ (pcs) (list ($line (hasheq 'size (list +inf.0 line-height) 'out 'sw) pcs)))))

(define (page-wrap xs size [debug #f])
  (wrap xs size debug
        #:break-val (break #\page)
        #:optional-break-proc $break?
        #:finish-segment-proc (λ (pcs) (list ($page (hasheq) (filter-not $break? pcs))))))

(define (typeset args)
  (define chars 33)
  (define line-width (* 7.2 chars))
  (define lines-per-page (* 40 line-height))
  (position ($doc (hasheq 'origin '(36 36)) (page-wrap (line-wrap (map charify (atomize (apply quad #f args))) line-width) lines-per-page))))


(define-macro (mb . ARGS)
  (with-pattern ([PS (syntax-property #'ARGS 'ps)])
    #'(#%module-begin
       (define q (typeset (list . ARGS)))
       ;q
       (let ([doc (make-object PDFDocument
                    (hasheq 'compress #f
                            'size '(300 400)))])
         (send* doc
           [pipe (open-output-file PS #:exists 'replace)]
           [font "Courier"]
           [fontSize 12])
         (draw q doc)
         (send doc end))
       (void))))

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
    (syntax-property (quad-at-reader path-string p) 'ps (path-replace-extension path-string #".pdf"))))