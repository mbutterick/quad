#lang quad/dev
(require racket/class racket/contract racket/string sugar/debug sugar/cache racket/list racket/file racket/draw data/gvector)
(provide (all-defined-out))

(define (world:paper-width-default) 612)
(define (world:paper-height-default) 792)

(define renderable-quads '(word box))

(define (render-pdf [qs #f] [path-string "test.pdf"])
  (send* (current-ps-setup) (set-margin 0 0) (set-scaling 1.0 1.0))
  (define dc-output-port (open-output-bytes))
  (define dc (new pdf-dc% [interactive #f][use-paper-bbox #f][as-eps #f]
                  [output dc-output-port]
                  [width (world:paper-width-default)][height (world:paper-height-default)]))
  (send dc start-doc "boing")
  (send dc set-pen "black" 1 'solid)
  (send dc set-brush "black" 'transparent) ; no fill by default
  
  
  #;(for ([q (in-vector qs)] #:when (member (quad-name q) renderable-quads))
         (define p (quad-attr-ref q world:page-key))
         (gvector-set! page-quad-vector p (cons q (gvector-ref page-quad-vector p null))))
  
  #;(for/list ([pq (in-gvector page-quad-vector)])
              (send dc start-page)
              (map/send render-element (filter-not whitespace/nbsp? elements))
              (send dc end-page))
  
  (define (print-status)
    (send dc draw-text (format "quad pdf test @ ~a" (current-milliseconds)) 0 0))
  
  (send dc set-font (make-font #:face "Source Code Pro" #:size 10))
  
  (send dc start-page)
  (print-status)
  
  (define default-x 40)
  (define default-y 40)
  
  (when qs
    (for/fold ([page-pos 0]
               [x-pos default-x]
               [y-pos default-y])
              ([q (in-vector qs)])
      (let ([font-attr (hash-ref (quad-attrs q) 'font #f)])
        (when font-attr
          (send dc set-font (make-font #:face (string-trim font-attr ".ttf") #:size 10))))
      (cond
        [(eq? (quad-dim q) 'page-break)
         (send dc end-page)
         (send dc start-page)
         (print-status)
         (values page-pos default-x default-y)]
        [(eq? (quad-dim q) 'line-break)
         (values page-pos default-x (+ y-pos 12))]
        [(eq? (quad-dim q) 'column-break)
         (values page-pos x-pos y-pos)] ; ignore for now
        [(quad-printable? q)
         (send dc draw-text (format "~a" (quad-val q)) x-pos y-pos)
         (values page-pos (+ x-pos (quad-dim q)) y-pos)]
        [else (values page-pos x-pos y-pos)])))
  
  (send dc end-page)
  (send dc end-doc)
  
  (define result-bytes (get-output-bytes dc-output-port))
  (display-to-file result-bytes path-string #:exists 'replace #:mode 'binary))


#;(define (render-element q)
    (cond 
      [(word? q) (render-word q)]
      [else q]))


(define/caching (make-font/caching font size style weight)
  (make-font #:face font #:size size #:style style #:weight weight))

#;(define (render-word w)
    (define word-font (quad-attr-ref/parameter w world:font-name-key))
    (define word-size (quad-attr-ref/parameter w world:font-size-key))
    (define word-style (quad-attr-ref/parameter w world:font-style-key))
    (define word-weight (quad-attr-ref/parameter w world:font-weight-key))
    (define word-color (quad-attr-ref/parameter w world:font-color-key))
    (define word-background (quad-attr-ref/parameter w world:font-background-key))
    (send dc set-font (make-font/caching word-font word-size word-style word-weight))
    (send dc set-text-foreground (send the-color-database find-color word-color))
    (define background-color (send the-color-database find-color word-background))
    (if background-color ; all invalid color-string values will return #f
        (send* dc (set-text-mode 'solid) (set-text-background background-color))
        (send dc set-text-mode 'transparent))
    
    (define word-text (quad-car w))
    (send dc draw-text word-text (quad-attr-ref w world:x-position-key) 
          ;; we want to align by baseline rather than top of box
          ;; thus, subtract ascent from y to put baseline at the y coordinate
          (- (quad-attr-ref w world:y-position-key) (quad-attr-ref w world:ascent-key 0)) #t))

(module+ test
  (render-pdf))