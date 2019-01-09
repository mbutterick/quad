#lang debug racket
(require quad racket/class)
(require racket/promise racket/list sugar/debug pitfall/pdf pitfall/vector pitfall/font pitfall/annotation pitfall/color pitfall/text fontland/font racket/runtime-path pollen/tag)
(provide (rename-out [mb #%module-begin]) (except-out (all-from-out racket) #%module-begin))

(define-runtime-path charter "fonts/charter.ttf")

(define (soft-break? q)
  (and (quad? q)
       (or (memv (car (quad-elems q)) '(#\space #\- #\u00AD))
           (member  (car (quad-elems q)) (map string '(#\space #\- #\u00AD))))))

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
(define $textish (q #:in 'bi
                    #:out 'bo
                    #:printable (λ (q [sig #f])
                                  (case (car (quad-elems q))
                                    [(" " #\u00AD) (λ (sig) (memq sig '(end)))]
                                    [(" " #\space) (λ (sig) (not (memq sig '(start end))))]
                                    [else #true]))
                    #:draw (λ (q doc)
                             (set! draw-counter (add1 draw-counter ))
                             (font-size doc (string->number (hash-ref (quad-attrs q) 'fontsize "12")))
                             (let ([str (car (quad-elems q))])
                               (cond
                                 [(hash-has-key? (quad-attrs q) 'link)
                                  (save doc)
                                  (fill-color doc "blue")
                                  (text doc str (first  (quad-origin q)) (second (quad-origin q)) (hasheq 'link (hash-ref (quad-attrs q) 'link)))
                                  (restore doc)]
                                 [else
                                  #;(println str)
                                  (void)
                                  (apply text doc str (quad-origin q))])))))
  
(define (quadify doc q)
  (struct-copy quad $textish
               [attrs (let ([h (quad-attrs q)]) (hash-set! h 'font charter) h)]
               [elems (quad-elems q)]
               [size (delay
                       (define fontsize (string->number (hash-ref (quad-attrs q) 'fontsize "12")))
                       (define str (car (quad-elems q)))
                       (font-size doc fontsize)
                       (font doc (path->string charter))
                       (pt
                        (string-width doc str)
                        (current-line-height doc)))]))

(define line-height 16)
(define $line (q #:attrs (hasheq 'type "line")
                 #:size (pt +inf.0 line-height)
                 #:out 'sw
                 #:printable #true))
(define $page (q #:attrs (hasheq 'type "page")
                 #:offset '(36 36)
                 #:pre-draw (λ (q doc)
                              (add-page doc)
                              (font-size doc 10)
                              (define str (string-append "page " (number->string page-count)))
                              ;; page number
                              (save doc)
                              (fill-color doc "blue")
                              (text doc str 10 10 (hasheq 'link "https://practicaltypography.com"))
                              (restore doc)
                              (set! page-count (add1 page-count)))))
(define $doc (q #:pre-draw (λ (q doc) (start-doc doc))
                #:post-draw (λ (q doc) (end-doc doc))))
(struct $break quad ())
(define page-count 1)
(define (make-break . xs) (q #:type $break
                             #:printable #f
                             #:size '(0 0)
                             #:elems xs))

(define (consolidate-runs pcs)
  (for/fold ([runs empty]
             [pcs pcs]
             #:result (reverse runs))
            ([i (in-naturals)]
             #:break (empty? pcs))
    (define-values (run-pcs rest) (splitf-at pcs (λ (p) (same-run? (car pcs) p))))
    (define new-run (struct-copy quad $textish
                                 [attrs (quad-attrs (car pcs))]
                                 [elems (merge-adjacent-strings (apply append (for/list ([pc (in-list run-pcs)])
                                                                                        (quad-elems pc))))]
                                 [size (delay (pt (for/sum ([pc (in-list run-pcs)])
                                                             (pt-x (size pc)))
                                                    (pt-y (size (car pcs)))))]))
    (values (cons new-run runs) rest)))


(define consolidate-into-runs? #t)
(define (line-wrap xs size [debug #f])
  (break xs size debug
         #:soft-break-proc soft-break?
         #:finish-wrap-proc (λ (pcs q idx) (list (struct-copy quad $line
                                                  [elems
                                                   ;; consolidate chars into a single run (naively)
                                                   ;; by taking attributes from first (including origin)
                                                   ;; this only works because there's only one run per line
                                                   ;; that is, it suffices to position the first letter
                                                   (if consolidate-into-runs?
                                                       (consolidate-runs pcs)
                                                       pcs)])))))

(define (page-wrap xs size [debug #f])
  (break xs size debug
         #:finish-wrap-proc (λ (pcs q idx) (list (struct-copy quad $page
                                                  [elems pcs])))))

(define (typeset pdf qarg)
  (define chars 65)
  (define line-width (* 7.2 chars))
  (define lines-per-page (* 40 line-height))
  (time-name config-pdf
             (font pdf (path->string charter))
             (font-size pdf 12))
  (let* ([x (time-name runify (runify qarg))]
         [x (time-name quadify (map (λ (x) (quadify pdf x)) x))]
         [x (time-name line-wrap (line-wrap x line-width))]
         [x (time-name page-wrap (page-wrap x lines-per-page))]
         [x (time-name position (position (struct-copy quad $doc
                                                       [elems x])))])
    x))

(define (run qin [path "test.pdf"])
  (define pdf (time-name make-pdf (make-pdf #:compress #t
                                            #:auto-first-page #f
                                            #:output-path path)))
  (define q (typeset pdf qin))
  (report draw-counter)
  (time-name draw (draw q pdf))
  (report draw-counter))

(require pollen/tag)
(define quad-tag (default-tag-function 'quad))
(provide (rename-out [quad-tag quad]))

(define-syntax (mb stx)
  (syntax-case stx ()
    [(_ . ARGS)
     (with-syntax ([PS (syntax-property #'ARGS 'ps)]
                   [(REP . QS) #'ARGS])
       #'(#%module-begin
          (define qs (list . QS))
          (define lotsa-qs (append* (make-list (string->number (string-trim REP)) qs)))
          (run (qexpr->quad (apply quad-tag #:fontsize "14" lotsa-qs)) PS)
          (void)))]))



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
