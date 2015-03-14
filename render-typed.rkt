#lang typed/racket/base
(require typed/racket/class)
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