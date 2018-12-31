#lang debug br/quicklang
(require racket/promise racket/list sugar/debug "quad.rkt" "atomize.rkt" "break.rkt" "qexpr.rkt" "generic.rkt" "position.rkt" pitfall/document pitfall/vector pitfall/font pitfall/annotation pitfall/color pitfall/text fontland/font racket/runtime-path pollen/tag)
(provide (rename-out [mb #%module-begin]) (except-out (all-from-out br/quicklang) #%module-begin))

(define-runtime-path charter "../qtest/fonts/charter.ttf")

(define (soft-break? q)
  (and (quad? q)
       (or (memv (car (elems q)) '(#\space #\- #\u00AD))
           (member  (car (elems q)) (map string '(#\space #\- #\u00AD))))))
(struct $shim $quad () #:transparent)
(define (draw-debug q doc)
  (save doc)
  (line-width doc 0.25)
  (apply rect doc (append (origin q) (size q)))
  (stroke doc "#fcc")
  (apply rect doc (append (origin q) (size q)))
  (clip doc)
  (circle doc (pt-x (in-point q)) (pt-y (in-point q)) 1)
  (circle doc (pt-x (out-point q)) (pt-y (out-point q)) 1)
  (fill doc "#f99")  
  (restore doc))

(define draw-counter 0)
(define (quadify doc q)
  ($quad (hash-set* (attrs q)
                    'in 'bi
                    'out 'bo
                    'font charter
                    'size 
                    (delay
                      (define fontsize (string->number (hash-ref (attrs q) 'fontsize "12")))
                      (define str (car (elems q)))
                      [font-size doc fontsize]
                      [font doc (path->string charter)]
                      (list
                       (string-width doc str)
                       (current-line-height doc)))
                    'printable? (case (car (elems q))
                                  [(" " #\u00AD) (λ (sig) (memq sig '(end)))]
                                  [(" " #\space) (λ (sig) (not (memq sig '(start end))))]
                                  [else #true])
                    'draw (λ (q doc)
                            (set! draw-counter (add1 draw-counter ))
                            #;(draw-debug q doc)
                            (font-size doc (string->number (hash-ref (attrs q) 'fontsize "12")))
                            (let ([str (car (elems q))])
                              (cond
                                [(hash-has-key? (attrs q) 'link)
                                 (text doc str (first (origin q)) (second (origin q)) (hasheq 'link (hash-ref (attrs q) 'link)))]
                                [else
                                 #;(println str)
                                 (void)
                                 (apply text doc str (origin q))])))) (elems q)))
(struct $line $quad () #:transparent)
(struct $page $quad () #:transparent)
(struct $doc $quad () #:transparent)
(struct $break $quad () #:transparent)
(define page-count 1)
(define (make-break . xs) ($break (hasheq 'printable? #f 'size '(0 0)) xs))



(define (consolidate-runs pcs)
  (for/fold ([runs empty]
             [pcs pcs]
             #:result (reverse runs))
            ([i (in-naturals)]
             #:break (empty? pcs))
    (define-values (run-pcs rest) (splitf-at pcs (λ (p) (same-run? (car pcs) p))))
    (define new-run ($quad (hash-set (attrs (car pcs))
                                     'size (delay (list (pt-x (apply map + (map size run-pcs)))
                                                        (pt-y (size (car pcs))))))
                           (merge-adjacent-strings (append-map elems run-pcs))))
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

(define pb ($break (hasheq 'printable? #f
                           'size '(0 0)
                           'draw (λ (q doc)
                                   (add-page doc)
                                   (font-size doc 10)
                                   (define str (string-append "page " (number->string page-count)))
                                   ;; page number
                                   (text doc str 10 10 (hasheq 'link "https://practicaltypography.com"))
                                   (set! page-count (add1 page-count)))) '(#\page)))
(define (page-wrap xs size [debug #f])
  (break xs size debug
         #:break-before? #t
         #:break-val pb
         #:soft-break-proc $break?
         #:finish-wrap-proc (λ (pcs) (list ($page (hasheq 'offset '(36 36)) (filter-not $break? pcs))))))

(define (typeset doc qarg)
  (define chars 65)
  (define line-width (* 7.2 chars))
  (define lines-per-page (* 40 line-height))
  (let* ([x (time-name runify (runify qarg))]
         [x (time-name quadify (map (λ (x) (quadify doc x)) x))]
         [x (time-name line-wrap (line-wrap x line-width))]
         [x (time-name page-wrap (page-wrap x lines-per-page))]
         [x (time-name position (position ($doc (hasheq) x)))])
    x))


(provide quad run qexpr->quad)
(define quad (default-tag-function 'quad))

(define (run qin [path "test.pdf"])
  (define doc (time-name make-doc
                         (make-$doc
                          (hasheq 'compress #t
                                  'autoFirstPage #f))))
  (time-name config-doc
             [font doc (path->string charter)]
             [font-size doc 12])
  (define q (typeset doc qin))
  (report draw-counter)
  (time-name draw
             (with-output-to-file path
               (λ ()
                 (start-doc doc)
                 (draw q doc)
                 (end-doc doc))
               #:exists 'replace))
  (report draw-counter))

(define-macro (mb . ARGS)
  (with-syntax ([PS (syntax-property #'ARGS 'ps)]
                [(REP . QS) #'ARGS])
    #'(#%module-begin
       (define qs (list . QS))
       (define lotsa-qs (append* (make-list (string->number (string-trim REP)) qs)))
       (run (qexpr->quad (apply quad #:fontsize "12" lotsa-qs)) PS)
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
