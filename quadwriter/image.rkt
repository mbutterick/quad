#lang debug racket
(require "struct.rkt"
         "attrs.rkt"
         "param.rkt"
         "debug.rkt"
         quad/position
         pitfall
         quad/quad)
(provide (all-defined-out))

 

(define (convert-image-quad q)
  (define path-string (quad-ref q :image-file))
  (unless (file-exists? path-string)
    (raise-argument-error 'create-image-quad "image path that exists" path-string))
  (define img-obj (open-image (current-pdf) path-string))
  (define img-width ($img-width img-obj))
  (define img-height ($img-height img-obj))
  (match-define (list layout-width layout-height)
    (match (list (quad-ref q :image-width) (quad-ref q :image-height)) 
      [(list (? number? w) (? number? h)) (list w h)]
      [(list #false (? number? h))
       (define ratio (/ h img-height))
       (list (* ratio img-width) h)]
      [(list (? number? w) #false)
       (define ratio (/ w img-width))
       (list w (* ratio img-height))]
      [(list #false #false) (list img-width img-height)]))
  (quad-copy image-quad q:image
             [attrs (let ([h (hash-copy (quad-attrs q))])
                      ;; defeat 'bi 'bo positioning by removing font reference
                      (hash-set! h font-path-key #false)
                      ;; save the img-obj for later
                      (hash-set! h :image-object img-obj)
                      h)]
             [size (pt layout-width layout-height)]))

(define (q:image-draw q doc)
  (define img (quad-ref q :image-object))
  (match-define (list x y) (quad-origin q))
  (match-define (list w h) (size q))
  (image doc img x y
         #:width w
         #:height h))

(define (q:image-draw-end q doc)
  (when (draw-debug-image?)
    (draw-debug q doc "orange" "orange")))

(define q:image (q #:type image-quad
                   #:from 'bo
                   #:to 'bi
                   #:id 'image
                   #:printable #true
                   #:draw q:image-draw
                   #:draw-end q:image-draw-end))