#lang debug racket/base
(require (for-syntax racket/base) racket/runtime-path racket/promise racket/match racket/list
         pitfall  quad sugar/debug markdown pollen/tag (prefix-in pt: pollen/top))
(provide (except-out (all-from-out racket/base) #%module-begin #%top)
         (rename-out [mb #%module-begin][pt:#%top #%top])
         (all-defined-out))

(define-syntax-rule (p attrs . exprs)
  (list 'q 'attrs . exprs))

(define-syntax-rule (strong attrs . exprs)
  (list 'q (cons '(font "charter-bold") 'attrs)  . exprs))

(define-syntax-rule (em attrs . exprs)
  (list 'q (cons '(font "charter-italic") 'attrs)  . exprs))

(define q:string (q #:in 'bi #:out 'bo ;; align to baseline
                    ;; printable unless single space, which is not printable at start or end
                    #:printable (λ (q [sig #f])
                                  (case (car (quad-elems q))
                                    [(" " #\space) (not (memq sig '(start end)))]
                                    [else #true]))
                    ;; draw with pdf text routine
                    #:draw (λ (q doc)
                             (font doc (path->string (hash-ref (quad-attrs q) 'font)))
                             (define str (car (quad-elems q)))
                             (apply text doc str (quad-origin q)))))

(define-runtime-path charter "fonts/charter.ttf")
(define-runtime-path charter-bold "fonts/charter-bold.ttf")
(define-runtime-path charter-italic "fonts/charter-italic.ttf")
(define-runtime-path fira "fonts/fira.ttf")

(define (->string-quad doc q)
  (struct-copy quad q:string
               [attrs (let ([attrs (quad-attrs q)])
                        ;; attrs hashes are shared between many quads.
                        ;; so the first update will change every reference to the shared hash
                        ;; hence why we ignore if val is already a path
                        ;; but this op should ideally happen earlier
                        (hash-update! attrs 'font (λ (val) (if (path? val)
                                                               val
                                                               (match (string-downcase val)
                                                                 ["charter" charter]
                                                                 ["charter-bold" charter-bold]
                                                                 ["charter-italic" charter-italic]
                                                                 ["fira" fira]))))
                        attrs)]
               [elems (quad-elems q)]
               [size (delay
                       (define fontsize (string->number (hash-ref (quad-attrs q) 'fontsize)))
                       (font-size doc fontsize)
                       (font doc (path->string (hash-ref (quad-attrs q) 'font)))
                       (define str (car (quad-elems q)))
                       (pt (string-width doc str) (current-line-height doc)))]))

(define line-height 20)
(define q:line (q #:size (pt +inf.0 line-height)
                  #:out 'sw
                  #:printable #true))

(define softies (map string '(#\space #\- #\u00AD)))
(define (soft-break-for-line? q)
  (member (car (quad-elems q)) softies))

(define (consolidate-runs pcs)
  (for/fold ([runs empty]
             [pcs pcs]
             #:result (reverse runs))
            ([i (in-naturals)]
             #:break (empty? pcs))
    (define-values (run-pcs rest) (splitf-at pcs (λ (p) (same-run? (car pcs) p))))
    (define new-run (struct-copy quad q:string
                                 [attrs (quad-attrs (car pcs))]
                                 [elems (merge-adjacent-strings (apply append (for/list ([pc (in-list run-pcs)])
                                                                                (quad-elems pc))))]
                                 [size (delay (pt (for/sum ([pc (in-list run-pcs)])
                                                    (pt-x (size pc)))
                                                  (pt-y (size (car pcs)))))]))
    (values (cons new-run runs) rest)))

;; 190108 feels like this needs separate finish-wraps for soft and hard.
;; and maybe pass in the quad that triggered the wrap.
;; otherwise within line-wrap, the finish-wrap-proc can't tell the difference between
;; 1) ordinary word space (permits wrap to next line)
;; 2) hard line break (forces wrap to next line)
;; 3) paragraph break (forces wrap to next line and also triggers first-line behavior,
;; e.g., more space above or first-line indent.)
(define (line-wrap xs size)
  (break xs size
         #:hard-break-proc (λ (q) (equal? "¶" (car (quad-elems q))))
         #:soft-break-proc soft-break-for-line?
         #:finish-wrap-proc (λ (pcs) (list (struct-copy quad q:line
                                                        [elems (consolidate-runs pcs)])))))

(define q:page (q #:offset '(36 36)
                  #:pre-draw (λ (q doc) (add-page doc))))

(define q:doc (q #:pre-draw (λ (q doc) (start-doc doc))
                #:post-draw (λ (q doc) (end-doc doc))))


(define (page-wrap xs vertical-height)
  (break xs vertical-height #:finish-wrap-proc (λ (pcs) (list (struct-copy quad q:page [elems pcs])))))

(define (run xs path)
  (define pdf (time-name make-pdf (make-pdf #:compress #t
                                            #:auto-first-page #f
                                            #:output-path path)))
  (define line-width 200)
  (define vertical-height 200)
  (let* ([x (time-name runify (runify (qexpr->quad xs)))]
         [x (time-name ->string-quad (map (λ (x) (->string-quad pdf x)) x))]
         [x (time-name line-wrap (line-wrap x line-width))]
         [x (time-name page-wrap (page-wrap x vertical-height))]
         [x (time-name position (position (struct-copy quad q:doc [elems x])))])
      (time-name draw (draw x pdf))))

(define-syntax (mb stx)
  (syntax-case stx ()
    [(_ . STRS)
     (with-syntax ([PS (syntax-property #'STRS 'ps)])
       #'(#%module-begin
          (define qx `(q ((font "Charter") (fontsize "12")) ,@(list . STRS)))
          (run qx PS)))]))

(module reader syntax/module-reader
  qtest/markdown
  #:read quad-read
  #:read-syntax quad-read-syntax
  #:whole-body-readers? #t ;; need this to make at-reader work
  (require scribble/reader markdown pollen/private/splice racket/list quad)
  
  (define (quad-read p) (syntax->datum (quad-read-syntax (object-name p) p)))
  
  (define (quad-read-syntax path-string p)
    (define quad-at-reader (make-at-reader
                            #:syntax? #t 
                            #:inside? #t
                            #:command-char #\◊))
    (define stx (quad-at-reader path-string p))
    (define parsed-stx (datum->syntax stx (add-between (parse-markdown (apply string-append (syntax->datum stx))) '(quad "¶"))))
    (syntax-property parsed-stx 'ps (path-replace-extension path-string #".pdf"))))