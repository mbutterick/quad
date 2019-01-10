#lang debug racket/base
(require (for-syntax racket/base) txexpr racket/runtime-path racket/string racket/promise racket/match racket/list
         pitfall quad sugar/debug pollen/tag)
(provide (except-out (all-from-out racket/base) #%module-begin)
         (rename-out [mb #%module-begin] [q-tag q])
         p id strong em attr-list h1 h2 code pre a)

(define-tag-function (p attrs exprs)
  (txexpr 'q attrs exprs))

(define id (default-tag-function 'id))
(define class (default-tag-function 'class))

(define q-tag (default-tag-function 'q))

(define-tag-function (strong attrs exprs)
  (txexpr 'q (cons '(font "charter-bold") attrs) exprs))

(define-tag-function (a attrs exprs)
  (txexpr 'q `((link ,(cadr (assoc 'href attrs)))(color "MediumVioletRed")) exprs))

(define-tag-function (em attrs exprs)
  (txexpr 'q (cons '(font "charter-italic") attrs) exprs))

(define-syntax-rule (attr-list . attrs) 'attrs)

(define-tag-function (h1 attrs exprs)
  (txexpr 'q (append  '((font "fira")(fontsize "36")(line-height "48")) attrs) exprs))

(define-tag-function (h2 attrs exprs)
  (txexpr 'q (append '((font "fira")(fontsize "24")(line-height "36")) attrs)  exprs))

(define-tag-function (code attrs exprs)
  (txexpr 'q (append '((font "fira-mono")(fontsize "11")(bg "aliceblue")) attrs)  exprs))

(define-tag-function (pre attrs exprs)
  (txexpr 'q attrs exprs))

(define q:string (q #:in 'bi
                    #:out 'bo ;; align to baseline
                    ;; printable unless single space, which is not printable at start or end
                    #:printable (λ (q [sig #f])
                                  (case (car (quad-elems q))
                                    [(" " #\space) (not (memq sig '(start end)))]
                                    [else #true]))
                    ;; draw with pdf text routine
                    #:draw (λ (q doc)
                             (font doc (path->string (hash-ref (quad-attrs q) 'font)))
                             (font-size doc (string->number (hash-ref (quad-attrs q) 'fontsize "12")))
                             (fill-color doc (hash-ref (quad-attrs q) 'color "black"))
                             (match-define (list str) (quad-elems q))
                             (match-define (list x y) (quad-origin q))
                             (text doc str x y #:bg (hash-ref (quad-attrs q) 'bg #f)
                                   #:link (hash-ref (quad-attrs q) 'link #f)))))

(define-runtime-path charter "fonts/charter.ttf")
(define-runtime-path charter-bold "fonts/charter-bold.ttf")
(define-runtime-path charter-italic "fonts/charter-italic.ttf")
(define-runtime-path fira "fonts/fira-light.ttf")
(define-runtime-path fira-mono "fonts/fira-mono.ttf")

(define (->string-quad doc q)
  (struct-copy
   quad q:string
   [attrs (let ([attrs (quad-attrs q)])
            ;; attrs hashes are shared between many quads.
            ;; so the first update will change every reference to the shared hash
            ;; hence why we ignore if val is already a path
            ;; but this op should ideally happen earlier
            (hash-update! attrs 'font
                          (λ (val) (if (path? val)
                                       val
                                       (match (string-downcase (string-replace val " " "-"))
                                         ["charter" charter]
                                         ["charter-bold" charter-bold]
                                         ["charter-italic" charter-italic]
                                         ["fira" fira]
                                         ["fira-mono" fira-mono]))))
            attrs)]
   [elems (quad-elems q)]
   [size (delay
           (define fontsize (string->number (hash-ref (quad-attrs q) 'fontsize)))
           (font-size doc fontsize)
           (font doc (path->string (hash-ref (quad-attrs q) 'font)))
           (define str (car (quad-elems q)))
           (pt (string-width doc str) (current-line-height doc)))]))

(define (draw-debug q doc)
  (save doc)
  (line-width doc 0.5)
  (apply rect doc (append (quad-origin q) (size q)))
  (stroke doc "#fcc")
  (apply rect doc (append (quad-origin q) (size q)))
  (clip doc)
  (circle doc (pt-x (in-point q)) (pt-y (in-point q)) 3)
  (circle doc (pt-x (out-point q)) (pt-y (out-point q)) 3)
  (fill doc "#f99")  
  (restore doc))

(define line-height 20)
(define q:line (q #:size (pt 380 line-height)
                  #:in 'nw
                  #:inner 'sw ; puts baseline at bottom of line box
                  #:out 'sw
                  #:printable #true
                  #:draw (λ (q doc)
                           #;(draw-debug q doc)
                           (default-draw q doc))))
(struct line-spacer quad () #:transparent)
(define q:line-spacer (q #:type line-spacer
                         #:size (pt 380 (* line-height 0.6))
                         #:out 'sw
                         #:printable (λ (q sig)
                                       (not (memq sig '(start end))))))

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


(define (line-wrap xs size)
  (break xs size
         #:hard-break (λ (q) (equal? "¶" (car (quad-elems q))))
         #:soft-break soft-break-for-line?
         #:finish-wrap (λ (pcs q idx)
                         (append
                          (if (= idx 1) (list q:line-spacer) null)
                          (list (struct-copy quad q:line
                                             [size (let ()
                                                     (define line-heights
                                                       (filter-map
                                                        (λ (q) (string->number (hash-ref (quad-attrs q) 'line-height "NaN")))
                                                        pcs))
                                                     (match-define (list w h) (quad-size q:line))
                                                     ;; when `line-heights` is empty, this is just h
                                                     (pt w (apply max (cons h line-heights))))]
                                             [elems (consolidate-runs pcs)]))))))

(define vert-margin 48)
(define side-margin 120)
(define page-offset (pt side-margin vert-margin))
(define q:page (q #:offset page-offset
                  #:pre-draw (λ (q doc) (add-page doc))))

(define q:doc (q #:pre-draw (λ (q doc) (start-doc doc))
                 #:post-draw (λ (q doc) (end-doc doc))))


(define (page-wrap xs vertical-height)
  (break xs vertical-height
         #:soft-break line-spacer?
         #:finish-wrap (λ (pcs q idx) (list (struct-copy quad q:page [elems pcs])))))

(define (run xs path)
  (define pdf (time-name make-pdf (make-pdf #:compress #t
                                            #:auto-first-page #f
                                            #:output-path path
                                            #:size "legal")))
  (define line-width (- (pdf-width pdf) (* 2 side-margin)))
  (define vertical-height (- (pdf-height pdf) (* 2 vert-margin)))
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
  (require scribble/reader  (only-in markdown parse-markdown) racket/list quad)
  
  (define (quad-read p) (syntax->datum (quad-read-syntax (object-name p) p)))

  (require racket/match txexpr)
  (define (xexpr->parse-tree x)
    ;; an ordinary txexpr can't serve as a parse tree because of the attrs list fails when passed to #%app.
    ;; so stick an `attr-list` identifier on it which can hook into the expander.
    ;; sort of SXML-ish.
    (let loop ([x x])
      (match x
        [(txexpr tag attrs elems) (list* tag (cons 'attr-list attrs) (map loop elems))]
        [(? list? xs) (map loop xs)]
        [_ x])))
  
  (define (quad-read-syntax path-string p)
    (define quad-at-reader (make-at-reader
                            #:syntax? #t 
                            #:inside? #t
                            #:command-char #\◊))
    (define stx (quad-at-reader path-string p))
    (define parsed-stx (datum->syntax stx (xexpr->parse-tree (add-between (parse-markdown (apply string-append (syntax->datum stx))) '(q "¶")))))
    (syntax-property parsed-stx 'ps (path-replace-extension path-string #".pdf"))))