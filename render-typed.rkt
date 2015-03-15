#lang typed/racket/base
(require typed/racket/class racket/file racket/list)
(require/typed racket/draw
               [current-ps-setup (Parameterof (Instance (Class (init-field)
                                                               [set-margin (Number Number . -> . Void)]
                                                               [set-scaling (Number Number . -> . Void)])))]
               [the-color-database (Instance (Class 
                                              (find-color (String . -> . (Option (Instance (Class)))))))]
               [pdf-dc% (Class (init [interactive Boolean][use-paper-bbox Boolean][as-eps Boolean]
                                     [output Output-Port]
                                     [width Flonum][height Flonum])
                               (start-doc (String . -> . Void))
                               (set-pen (String Real Symbol . -> . Void))
                               (set-brush (String Symbol . -> . Void))
                               (set-font ((Instance (Class)) . -> . Void))
                               (set-text-foreground ((Instance (Class)) . -> . Void))
                               (set-text-background ((Instance (Class)) . -> . Void))
                               (set-text-mode (Symbol . -> . Void))
                               (draw-text (String Flonum Flonum Boolean . -> . Void))
                               (start-page (-> Void))
                               (end-page (-> Void))
                               (end-doc (-> Void)))]
               [make-font ((#:size Nonnegative-Flonum) (#:style Symbol) (#:weight Symbol) (#:face String) . -> . (Instance (Class (init-field))))])
(require/typed sugar/cache [make-caching-proc ((String Nonnegative-Flonum Symbol Symbol -> (Instance (Class))) . -> . (String Nonnegative-Flonum Symbol Symbol -> (Instance (Class))))])
(require "utils-typed.rkt" "quads-typed.rkt" "world-typed.rkt")

(define abstract-renderer%
  
  (class object% 
    (super-new)    
    
    (define renderable-quads '(word box))
    
    ;; hash implementation
    (: render (Quad . -> . Any))
    (define/public (render doc-quad)
      (finalize
       (let ([rendering-input (flatten-quad (setup doc-quad))])
         (define page-quad-hash ((inst make-hash Nonnegative-Integer (Listof Quad))))
         (for ([q (in-list rendering-input)])
           (when (member (quad-name q) renderable-quads)
             ((inst hash-update! Nonnegative-Integer (Listof Quad)) page-quad-hash (cast (quad-attr-ref q world:page-key) Nonnegative-Integer) (λ(v) ((inst cons Quad (Listof Quad)) q v)) (λ() (cast null (Listof Quad))))))
         (map (λ([k : Nonnegative-Integer]) (render-page ((inst hash-ref Nonnegative-Integer (Listof Quad) (Listof Quad)) page-quad-hash k))) (sort (hash-keys page-quad-hash) <)))))
    
    (: render-element (Quad . -> . Any))
    (define/public (render-element q)
      (cond 
        [(word? q) (render-word q)]
        [else q]))
    
    (: setup (Quad . -> . Quad))
    (define/public (setup q) q)
    
    ;; use in lieu of 'abstract' definition
    (: render-page ((Listof Quad) . -> . Void))
    (define/public (render-page qs) (void)) 
    
    ;; use in lieu of 'abstract' definition
    (: render-word (Quad . -> . Any))
    (define/public (render-word x) (word)) 
    
    (: finalize (Any . -> . Any))
    (define/public (finalize x) x)))

(define-syntax-rule (map/send method xs)
  (map (λ([x : Quad]) (method x)) xs))

;; this is outside class def'n because if inside,
;; (define dc ...) can't see it and type it correctly.
;; there may be a better way, but for now this is OK
(: dc-output-port Output-Port)
(define dc-output-port (open-output-bytes))


(provide pdf-renderer%) 
(define pdf-renderer%
  (class abstract-renderer%
    (super-new)
    
    (send* (current-ps-setup) (set-margin 0 0) (set-scaling 1.0 1.0))
    
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
    
    (define make-font/caching
      (make-caching-proc (λ (font size style weight)
                           (make-font #:face font #:size size #:style style #:weight weight))))
    
    
    (define/override (render-word w)
      (define word-font (cast (quad-attr-ref/parameter w world:font-name-key) String))
      (define word-size (cast (quad-attr-ref/parameter w world:font-size-key) Nonnegative-Flonum))
      (define word-style (cast (quad-attr-ref/parameter w world:font-style-key) Symbol))
      (define word-weight (cast (quad-attr-ref/parameter w world:font-weight-key) Symbol))
      (define word-color (cast (quad-attr-ref/parameter w world:font-color-key) String))
      (define word-background (cast (quad-attr-ref/parameter w world:font-background-key) String))
      (send dc set-font (make-font/caching word-font word-size word-style word-weight))
      (define foreground-color (send the-color-database find-color word-color))
      (when foreground-color
        (send dc set-text-foreground foreground-color))
      (define background-color (send the-color-database find-color word-background))
      (if background-color ; all invalid color-string values will return #f
          (send* dc (set-text-mode 'solid) (set-text-background background-color))
          (send dc set-text-mode 'transparent))
      
      (define word-text (cast (quad-car w) String))
      (send dc draw-text word-text (cast (quad-attr-ref w world:x-position-key) Flonum) 
            ;; we want to align by baseline rather than top of box
            ;; thus, subtract ascent from y to put baseline at the y coordinate
            (- (cast (quad-attr-ref w world:y-position-key) Flonum) (cast (quad-attr-ref w world:ascent-key 0) Flonum)) #t))
    
    (define/override (render-page elements)
      (send dc start-page)
      (map/send render-element (filter-not whitespace/nbsp? elements))
      (send dc end-page))
    
    (define/override (finalize xs)
      (send dc end-doc)
      (get-output-bytes dc-output-port))
    
    (: render-to-file (Quad Path-String . -> . Void))
    (define/public (render-to-file doc-quad path)
      (define result-bytes (send this render doc-quad))
      (display-to-file result-bytes path #:exists 'replace #:mode 'binary))
    
    
    ))
