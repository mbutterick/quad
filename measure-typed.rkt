#lang typed/racket/base
(require (for-syntax typed/racket/base))
(require typed/racket/class math/flonum racket/list racket/file typed/racket/draw "core-types.rkt" typed/sugar/define)
(require/typed racket/serialize [serialize (Any -> Any)]
               [deserialize (Any -> (HashTable (List String String Symbol Symbol) Measurement-Result-Type))])
(provide measure-text measure-ascent round-float update-text-cache-file load-text-cache-file)


(define precision 4.0)
(define base (expt 10.0 precision))
(define max-size 1024.0)
(define dc (new record-dc%))
(define-type Measurement-Result-Type (List Float Float Float Float))
(define mrt? (make-predicate Measurement-Result-Type))
(define current-text-cache (make-parameter ((inst make-hash (List String String Symbol Symbol) Measurement-Result-Type) '())))
(define current-font-cache (make-parameter ((inst make-hash (List Font-Name Font-Weight Font-Style) (Instance Font%)) '())))

(define/typed (round-float x)
  (Float -> Float)
  (/ (round (* base x)) base))


(define/typed (get-cache-file-path)
  (-> Path)
  (build-path "font.cache"))


(define/typed (update-text-cache-file)
  (-> Void)
  (write-to-file (serialize (current-text-cache)) (get-cache-file-path) #:exists 'replace))


(define/typed (load-text-cache-file)
  (-> Void)
  (define cache-file-path (get-cache-file-path))
  (current-text-cache (if (file-exists? cache-file-path)
                          (deserialize (file->value cache-file-path))
                          ((inst make-hash (List String String Symbol Symbol) Measurement-Result-Type) '()))))


(define/typed (get-cached-font font weight style)
  (Font-Name Font-Weight Font-Style -> (Instance Font%))
  (hash-ref! (current-font-cache) (list font weight style) (λ() (make-font #:size max-size #:style style #:weight weight #:face font))))


(define/typed (measure-max-size text font [weight 'normal] [style 'normal])
  ((String Font-Name) (Font-Weight Font-Style) . ->* . Measurement-Result-Type)
  (: hash-updater (-> Measurement-Result-Type))
  (define (hash-updater) 
    #;(current-text-cache-changed? #t)
    (define font-instance (get-cached-font font weight style))
    ;; 'combine' boolean only makes a difference for two or more chars, so use (>= (string-length text) 1) for speed
    (define-values (width height descent extra) (send dc get-text-extent text font-instance (>= (string-length text) 1)))
    ;; avoid `map` here because it requires a cast to ensure the type
    ;; this seems like a bug in TR: doesn't recognize (List Float Float Float Float) as subtype of (Listof Float)?
    (list (fl width) (fl height) (fl descent) (fl extra)))
  ((inst hash-ref! (List String String Symbol Symbol) Measurement-Result-Type) (current-text-cache) (list text font weight style) hash-updater))

(define-syntax-rule (width x) (first x))
(define-syntax-rule (height x) (second x))
(define-syntax-rule (descent x) (third x))
#;(define-syntax-rule (extra x) (fourth x)) 


;; works by taking max size and scaling it down. Allows caching of results.
(define/typed (measure-text text size font weight style)
  (String Font-Size Font-Name Font-Weight Font-Style -> Float)
  (define raw-width (width (measure-max-size text font weight style)))
  (round-float (/ (* raw-width size) max-size)))


;; works by taking max size and scaling it down. Allows caching of results.
(define/typed (measure-ascent text size font [weight 'normal] [style 'normal])
  ((String Font-Size Font-Name) (Font-Weight Font-Style) . ->* . Float)
  (define result-list : Measurement-Result-Type (measure-max-size text font weight style))
  (define raw-baseline-distance (- (height result-list) (descent result-list)))
  (round-float (/ (* raw-baseline-distance size) max-size)))