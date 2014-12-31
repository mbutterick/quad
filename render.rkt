#lang racket/base
(require racket/class racket/contract sugar/debug sugar/cache racket/list racket/file racket/draw data/gvector)
(require "utils.rkt" "quads.rkt" "world.rkt")
(provide (all-defined-out))

(define/contract abstract-renderer%
  (class/c [render (quad? . ->m . any/c)]
           [render-element (quad? . ->m . quad?)])
  
  (class object% 
    (super-new)    
    
    (define renderable-quads '(word box))
    
    ;; hash implementation
    (define/public (render0 doc-quad)
      (finalize
       (let ([rendering-input (flatten-quad (setup doc-quad))])
         (define page-quad-hash (make-hash))
         (for ([q (in-list rendering-input)])
           (when (member (quad-name q) renderable-quads)
             (hash-update! page-quad-hash (quad-attr-ref q world:page-key) (λ(v) (cons q v)) null)))
         (map (λ(k) (render-page (hash-ref page-quad-hash k))) (sort (hash-keys page-quad-hash) <)))))
    
    
    ;; gvector implementation
    (define/public (render doc-quad)
      (finalize
       (let ([rendering-input (flatten-quad (setup doc-quad))])
         (define page-quad-vector (make-gvector))
         (for ([q (in-list rendering-input)] #:when (member (quad-name q) renderable-quads))
           (define p (quad-attr-ref q world:page-key))
           (gvector-set! page-quad-vector p (cons q (gvector-ref page-quad-vector p null))))
         (for/list ([pq (in-gvector page-quad-vector)])
           (render-page pq)))))

    
    (define/public (render-element q)
      (cond 
        [(word? q) (render-word q)]
        [else q]))
    
    (define/public (setup q) q)
    (abstract render-page)
    (abstract render-word)
    (define/public (finalize q) q)))


(define-syntax-rule (map/send method xs)
  (map (λ(x) (method x)) xs))

(define pdf-renderer%
  (class abstract-renderer%
    (super-new)
    
    (send* (current-ps-setup) (set-margin 0 0) (set-scaling 1.0 1.0))
    (define dc-output-port (open-output-bytes))
    (define dc (new pdf-dc% [interactive #f][use-paper-bbox #f][as-eps #f]
                     [output dc-output-port]
                     [width (world:paper-width-default)][height (world:paper-height-default)]))
    
    (define/override (setup tx)
      (send* dc 
        (start-doc "boing")
        (set-pen "black" 1 'solid)
        (set-brush "black" 'transparent)) ; no fill by default
      tx)
    
    (inherit render-element)
    
    (define/caching (make-font/caching font size style weight)
      (make-font #:face font #:size size #:style style #:weight weight))
    
    (define/override-final (render-word w)
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
    
    (define/override-final (render-page elements)
      (send dc start-page)
      (map/send render-element (filter-not whitespace/nbsp? elements))
      (send dc end-page))
    
    (define/override-final (finalize xs)
      (send dc end-doc)
      (get-output-bytes dc-output-port))
    
    (define/public (render-to-file doc-quad path)
      (define result-bytes (send this render doc-quad))
      (display-to-file result-bytes path #:exists 'replace #:mode 'binary))))
