#lang debug br/quicklang
(require racket/promise racket/list sugar/list sugar/debug "quad.rkt" "atomize.rkt" "break.rkt" "qexpr.rkt" "generic.rkt" "position.rkt")
(require pitfall/document)
(require hyphenate racket/runtime-path pollen/unstable/typography pollen/tag)
(provide (rename-out [mb #%module-begin]) (except-out (all-from-out br/quicklang) #%module-begin))


(define-runtime-path charter "charter.ttf")

(define soft-break? (λ (q) (and (quad? q) (memv (car (elems q)) '(#\space #\- #\u00AD)))))
(struct $shim $quad () #:transparent)
(struct $char $quad () #:transparent)
(define current-doc (make-parameter #f))
(define (draw-debug q doc)
  (send doc save)
  (send doc lineWidth 0.25)
  (send/apply doc rect (append (origin q) (size q)))
  (send doc stroke "#fcc")
  (send/apply doc rect (append (origin q) (size q)))
  (send doc clip)
  (send doc circle (pt-x (in-point q)) (pt-y (in-point q)) 1)
  (send doc circle (pt-x (out-point q)) (pt-y (out-point q)) 1)
  (send doc fill "#f99")  
  (send doc restore))

(define char-sizes (make-hash))
(define (charify q)
  ($char (hash-set* (attrs q)
                    'in 'bi
                    'out 'bo
                    'font charter
                    'size 
                    (delay
                      (define fontsize (string->number (hash-ref (attrs q) 'fontsize "12")))
                      (define str (apply string (elems q)))
                      (send* (current-doc)
                        [fontSize fontsize]
                        [font (path->string charter)])
                      (list
                       (send (current-doc) widthOfString str)
                       (send (current-doc) currentLineHeight)))
                    'printable? (case (car (elems q))
                                  [(#\u00AD) (λ (sig) (memq sig '(end)))]
                                  [(#\space) (λ (sig) (not (memq sig '(start end))))]
                                  [else #t])
                    'draw (λ (q doc)
                            (draw-debug q doc)
                            (send doc fontSize (string->number (hash-ref (attrs q) 'fontsize "12")))
                            (let ([str (apply string (elems q))])
                              (cond
                                [(hash-ref (attrs q) 'link #f)
                                 =>
                                 (λ (url-str) (apply as-link doc str url-str (origin q)))]
                                [else
                                 #;(println str)
                                 (send/apply doc text str (origin q))])))) (elems q)))
(struct $line $quad () #:transparent)
(struct $page $quad () #:transparent)
(struct $doc $quad () #:transparent)
(struct $break $quad () #:transparent)
(define page-count 1)
(define (make-break . xs) ($break (hasheq 'printable? #f 'size '(0 0)) xs))

(define (run-attrs-match left right)
  (define missing (gensym))
  (for/and ([k (in-list '(link weight fontsize))])
    (equal? (hash-ref (attrs left) k missing) (hash-ref (attrs right) k missing))))

(define (consolidate-runs pcs)
  (for/fold ([runs empty]
             [pcs pcs]
             #:result (reverse runs))
            ([i (in-naturals)]
             #:break (empty? pcs))
    (define-values (run-pcs rest) (splitf-at pcs (λ (p) (run-attrs-match (car pcs) p))))
    (define new-run ($char (hash-set (attrs (car pcs))
                                     'size (delay (list (pt-x (apply map + (map size run-pcs)))
                                                        (pt-y (size (car pcs))))))
                           (append-map elems run-pcs)))
    (values (cons new-run runs) rest)))

(define line-height 16)
(define consolidate-into-runs? #t)
(define (line-wrap xs size [debug #f])
  (break xs size debug
         #:break-val (make-break #\newline)
         #:soft-break-proc soft-break?
         #:finish-wrap-proc (λ (pcs) (list ($line (hasheq 'size (list +inf.0 line-height)
                                                          'out 'sw)
                                                  ;; consolidate chars into a single run (naively)
                                                  ;; by taking attributes from first (including origin)
                                                  ;; this only works because there's only one run per line
                                                  ;; that is, it suffices to position the first letter
                                                  (if consolidate-into-runs?
                                                      (consolidate-runs pcs)
                                                      pcs))))))

(define (as-link doc str url-str [x 0] [y 0])
  (send doc save)
  (send doc fillColor "blue")
  (define width (send doc widthOfString str))
  (define height (send doc currentLineHeight))
  (send doc text str x y)
  (send doc link x y width height url-str)
  (send doc restore))

(define pb ($break (hasheq 'printable? #f
                           'size '(0 0)
                           'draw (λ (q doc)
                                   (send doc addPage)
                                   (send doc fontSize 10)
                                   (define str (string-append "page " (number->string page-count)))
                                   ;; page number
                                   (as-link doc str "https://practicaltypography.com" 10 10)
                                   (set! page-count (add1 page-count)))) '(#\page)))
(define (page-wrap xs size [debug #f])
  (break xs size debug
         #:break-before? #t
         #:break-val pb
         #:soft-break-proc $break?
         #:finish-wrap-proc (λ (pcs) (list ($page (hasheq 'offset '(36 36)) (filter-not $break? pcs))))))

(define (typeset qarg)
  (define chars 25)
  (define line-width (* 7.2 chars))
  (define lines-per-page (* 4 line-height))
  (let* ([x (time-name line-wrap (line-wrap (map charify (atomize qarg)) line-width))]
         [x (time-name page-wrap (page-wrap x lines-per-page))]
         [x (time-name position (position ($doc (hasheq) x)))])
    x))


(provide quad)
(define quad (default-tag-function 'quad))

(define (run qin [path "test.pdf"])
  (define doc (time-name make-doc
                         (make-object PDFDocument
                           (hasheq 'compress #t
                                   'autoFirstPage #f
                                   'size '(300 200)))))
  (parameterize ([current-doc doc])
    (time-name config-doc
               (send* doc
                 [pipe (open-output-file path #:exists 'replace)]
                 [font (path->string charter)]
                 [fontSize 12]))
    (define q (typeset qin))
    (time-name draw (draw q doc))
    (time-name end-doc (send doc end))))

(define-macro (mb . ARGS)
  (with-pattern ([PS (syntax-property #'ARGS 'ps)])
    #'(#%module-begin
       (run (qexpr->quad (quad . ARGS)) PS)
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
                            #:inside? #t
                            #:command-char #\◊))
    (syntax-property (quad-at-reader path-string p) 'ps (path-replace-extension path-string #".pdf"))))
