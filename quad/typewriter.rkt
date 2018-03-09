#lang debug br/quicklang
(require racket/promise racket/list sugar/list sugar/debug "quad.rkt" "atomize.rkt" "wrap.rkt" "qexpr.rkt" "generic.rkt" "position.rkt")
(require pitfall/document)
(provide (rename-out [mb #%module-begin]) (except-out (all-from-out br/quicklang) #%module-begin))

(define optional-break? (λ (q) (and (quad? q) (memv (car (elems q)) '(#\space #\- #\u00AD)))))
(struct $shim $quad () #:transparent)
(struct $char $quad () #:transparent)
(define (charify q)
  ($char (hash-set* (attrs q)
                    'size (const '(7.2 12))
                    'printable? (case (car (elems q))
                                  [(#\u00AD) (λ (sig) (memq sig '(end)))]
                                  [(#\space) (λ (sig) (not (memq sig '(start end))))]
                                  [else #t])
                    'draw (λ (q doc)
                            (send doc fontSize 12)
                            (send/apply doc text (apply string (elems q)) (origin q)))) (elems q)))
(struct $line $quad () #:transparent)
(struct $page $quad () #:transparent)
(struct $doc $quad () #:transparent)
(struct $break $quad () #:transparent)
(define page-count 1)
(define (break . xs) ($break (hasheq 'printable? #f 'size '(0 0)) xs))

(define line-height 16)
(define consolidate-into-runs? #t)
(define (line-wrap xs size [debug #f])
  (wrap xs size debug
        #:break-val (break #\newline)
        #:optional-break-proc optional-break?
        #:finish-wrap-proc (λ (pcs) (list ($line (hasheq 'size (list +inf.0 line-height) 'out 'sw)
                                                 ;; consolidate chars into a single run (naively)
                                                 ;; by taking attributes from first (including origin)
                                                 ;; this only works because there's only one run per line
                                                 ;; that is, it suffices to position the first letter
                                                 (if consolidate-into-runs?
                                                     (list ($char (attrs (car pcs)) (append-map elems pcs)))
                                                     pcs))))))
(define pb ($break (hasheq 'printable? #f
                           'size '(0 0)
                           'draw (λ (q doc)
                                   (send doc addPage)
                                   (send doc fontSize 10)
                                   (send doc text (string-append "page " (number->string page-count)) 10 10)
                                   (set! page-count (add1 page-count)))) '(#\page)))
(define (page-wrap xs size [debug #f])
  (wrap xs size debug
        #:break-before? #t
        #:break-val pb
        #:optional-break-proc $break?
        #:finish-wrap-proc (λ (pcs) (list ($page (hasheq) (filter-not $break? pcs))))))

(define (typeset args)
  (define chars 25)
  (define line-width (* 7.2 chars))
  (define lines-per-page (* 4 line-height))
  (position ($doc (hasheq 'origin '(36 36)) (page-wrap (line-wrap (map charify (atomize (apply quad #f args))) line-width) lines-per-page))))


(require hyphenate racket/runtime-path pollen/unstable/typography)
(define-runtime-path fira-mono "FiraMono-Regular.ttf")
(define-macro (mb . ARGS)
  (with-pattern ([PS (syntax-property #'ARGS 'ps)])
    #'(#%module-begin
       (define q (typeset (map hyphenate (map smart-quotes (list . ARGS)))))
       ;q
       (let ([doc (make-object PDFDocument
                    (hasheq 'compress #t
                            'autoFirstPage #f
                            'size '(300 400)))])
         (send* doc
           [pipe (open-output-file PS #:exists 'replace)]
           [registerFont "Fira-Mono" (path->string fira-mono)]
           [font "Fira-Mono"]
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