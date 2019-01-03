#lang debug racket
(require quad racket/class)
(require racket/promise racket/list sugar/debug pitfall/document pitfall/vector pitfall/font pitfall/annotation pitfall/color pitfall/text fontland/font racket/runtime-path pollen/tag)
(provide (rename-out [mb #%module-begin]) (except-out (all-from-out racket) #%module-begin))

(define-runtime-path charter "fonts/charter.ttf")

(define (soft-break? q)
  (and (quad? q)
       (or (memv (car (send q elems)) '(#\space #\- #\u00AD))
           (member  (car (send q elems)) (map string '(#\space #\- #\u00AD))))))

(define (draw-debug q doc)
  (save doc)
  (line-width doc 0.25)
  (apply rect doc (append (send q origin) (send q size)))
  (stroke doc "#fcc")
  (apply rect doc (append (send q origin) (send q size)))
  (clip doc)
  (circle doc (pt-x (in-point q)) (pt-y (in-point q)) 1)
  (circle doc (pt-x (out-point q)) (pt-y (out-point q)) 1)
  (fill doc "#f99")  
  (restore doc))

(define draw-counter 0)
(define quadify%
  (class quad%
    (super-new)
    (init-field doc)
    (inherit-field @size @elems @attrs @origin @in @out)
    (set! @in 'bi)
    (set! @out 'bo)
    (set! @size
          (delay
            (define fontsize (string->number (hash-ref @attrs 'fontsize "12")))
            (define str (car @elems))
            (font-size doc fontsize)
            (font doc (path->string charter))
            (list
             (string-width doc str)
             (current-line-height doc))))
                 
    (define/override (printable? [sig #f])
      (case (car @elems)
        [(" " #\u00AD) (λ (sig) (memq sig '(end)))]
        [(" " #\space) (λ (sig) (not (memq sig '(start end))))]
        [else #true]))

    (define/override (draw doc)
      (set! draw-counter (add1 draw-counter ))
      (font-size doc (string->number (hash-ref @attrs 'fontsize "12")))
      (let ([str (car @elems)])
        (cond
          [(hash-has-key? @attrs 'link)
           (save doc)
           (fill-color doc "blue")
           (text doc str (first @origin) (second @origin) (hasheq 'link (hash-ref @attrs 'link)))
           (restore doc)]
          [else
           #;(println str)
           (void)
           (apply text doc str @origin)])))))

(define (quadify doc q)
  (make-object quadify%  doc (hash-set* (send q attrs) 'font charter) (send q elems)))

(define $line (class quad% (super-new)
                (set-field! @size this (list +inf.0 line-height))
                (set-field! @out this 'sw)))
(define $page (class quad% (super-new)
                (set-field! @offset this'(36 36))))
(define $doc (class quad% (super-new)))
(define $break (class quad% (super-new)))
(define page-count 1)
(define (make-break . xs) (make-object $break (hasheq 'printable? #f 'size '(0 0)) xs))

(define (consolidate-runs pcs)
  (for/fold ([runs empty]
             [pcs pcs]
             #:result (reverse runs))
            ([i (in-naturals)]
             #:break (empty? pcs))
    (define-values (run-pcs rest) (splitf-at pcs (λ (p) (same-run? (car pcs) p))))
    (define new-run (car pcs))
    (set-field! @size new-run (list (for/sum ([pc (in-list run-pcs)])
                                             (pt-x (send pc size)))
                                    (pt-y (send (car pcs) size))))
    (set-field! @elems new-run (merge-adjacent-strings (apply append (for/list ([pc (in-list run-pcs)])
                                                                               (send pc elems)))))
    (values (cons new-run runs) rest)))

(define line-height 16)
(define consolidate-into-runs? #t)
(define (line-wrap xs size [debug #f])
  (break xs size debug
         #:break-val (make-break #\newline)
         #:soft-break-proc soft-break?
         #:finish-wrap-proc (λ (pcs) (list (make-object $line (hasheq)
                                             ;; consolidate chars into a single run (naively)
                                             ;; by taking attributes from first (including origin)
                                             ;; this only works because there's only one run per line
                                             ;; that is, it suffices to position the first letter
                                             (if consolidate-into-runs?
                                                 (consolidate-runs pcs)
                                                 pcs))))))

;; 181231 it's weird that setup work for page is in the page break,
;; which is between pages, not associated with either
(define pb (make-object (let ([pb (class $break
                                    (super-new)
                                    (define/override (printable?) #f)
                                    (inherit-field @size)
                                    (set! @size '(0 0))
                                    (define/override (draw doc)
                                      (add-page doc)
                                      (font-size doc 10)
                                      (define str (string-append "page " (number->string page-count)))
                                      ;; page number
                                      (save doc)
                                      (fill-color doc "blue")
                                      (text doc str 10 10 (hasheq 'link "https://practicaltypography.com"))
                                      (restore doc)
                                      (set! page-count (add1 page-count))))])
                          pb) '(#\page)))

(define ($break? x) (is-a? x $break))
(define (page-wrap xs size [debug #f])
  (break xs size debug
         #:break-before? #t
         #:break-val pb
         #:soft-break-proc $break?
         #:finish-wrap-proc (λ (pcs) (list (make-object $page (hasheq) (filter-not $break? pcs))))))

(define (typeset doc qarg)
  (define chars 65)
  (define line-width (* 7.2 chars))
  (define lines-per-page (* 40 line-height))
  (let* ([x (time-name runify (runify qarg))]
         [x (time-name quadify (map (λ (x) (quadify doc x)) x))]
         [x (time-name line-wrap (line-wrap x line-width))]
         [x (time-name page-wrap (page-wrap x lines-per-page))]
         [x (time-name position (position (make-object $doc (hasheq) x)))])
    x))

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
                 (send q draw doc)
                 (end-doc doc))
               #:exists 'replace))
  (report draw-counter))

(define-syntax (mb stx)
  (syntax-case stx ()
    [(_ . ARGS)
     (with-syntax ([PS (syntax-property #'ARGS 'ps)]
                   [(REP . QS) #'ARGS])
       #'(#%module-begin
          (define qs (list . QS))
          (define lotsa-qs (append* (make-list (string->number (string-trim REP)) qs)))
          (run (qexpr->quad (apply quad #:fontsize "12" lotsa-qs)) PS)
          (void)))]))

(define quad (default-tag-function 'quad))
(provide quad)

(module reader syntax/module-reader
  qtest/typewriter
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
