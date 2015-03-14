#lang typed/racket/base
(require typed/racket/class racket/file racket/list)
(require/typed racket/draw
               [current-ps-setup (Parameterof (Instance (Class (init-field)
                                                  [set-margin (Number Number . -> . Void)]
                                                  [set-scaling (Number Number . -> . Void)])))]
               [the-color-database Any]
               [pdf-dc% (Class (init [interactive Boolean][use-paper-bbox Boolean][as-eps Boolean]
                    [output Output-Port]
                    [width Flonum][height Flonum]))]
               [make-font ((#:size Nonnegative-Flonum) (#:style Symbol) (#:weight Symbol) (#:face String) . -> . (Instance (Class (init-field))))])
(require/typed sugar/cache [make-caching-proc (Procedure . -> . Procedure)])
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
    
    (: render-element (Quad . -> . Quad))
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
    (: render-word (Quad . -> . Quad))
    (define/public (render-word x) (word)) 
    
    (: finalize (Any . -> . Any))
    (define/public (finalize x) x)))

(define-syntax-rule (map/send method xs)
  (map (λ(x) (method x)) xs))


(define pdf-renderer%
  (class abstract-renderer%
    (super-new)
    
    (send* (current-ps-setup) (set-margin 0 0) (set-scaling 1.0 1.0))
    
    (: dc-output-port Output-Port)
    (define dc-output-port (open-output-bytes))
    
    (define dc (new pdf-dc% [interactive #f][use-paper-bbox #f][as-eps #f]
                    [output dc-output-port]
                    [width (world:paper-width-default)][height (world:paper-height-default)]))
    
    #| restart here with error:
Type Checker: missing type for identifier;
 consider adding a type annotation with `:'
  identifier: self160170 in: dc-output-port

|#
    
    #;(define/override (setup tx)
      (send* dc 
        (start-doc "boing")
        (set-pen "black" 1 'solid)
        (set-brush "black" 'transparent)) ; no fill by default
      tx)
    
    (inherit render-element)
    
    #;(define make-font/caching
      (make-caching-proc (λ (font size style weight)
                           (make-font #:face font #:size size #:style style #:weight weight))))
    
    
    #;(define/override (render-word w)
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
      
      #;(define word-text (quad-car w))
      #;(send dc draw-text word-text (quad-attr-ref w world:x-position-key) 
            ;; we want to align by baseline rather than top of box
            ;; thus, subtract ascent from y to put baseline at the y coordinate
            (- (quad-attr-ref w world:y-position-key) (quad-attr-ref w world:ascent-key 0)) #t))
    
    #;(define/override (render-page elements)
      (send dc start-page)
      (map/send render-element (filter-not whitespace/nbsp? elements))
      (send dc end-page))
    
    #;(define/override (finalize xs)
      (send dc end-doc)
      (get-output-bytes dc-output-port))
    
  #|  (: render-to-file (Quad . -> . Path-String))
    (define/public (render-to-file doc-quad path)
      (define result-bytes (send this render doc-quad))
      (display-to-file result-bytes path #:exists 'replace #:mode 'binary))
  |#
  
  ))
