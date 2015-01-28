#lang typed/racket/base
(require typed/racket/class)
(require/typed racket/draw [record-dc%  (Class (init-field))]
               [make-font ((#:size Nonnegative-Flonum) (#:style Symbol) (#:weight Symbol) (#:face String) . -> . (Instance (Class (init-field))))])
(require/typed sugar/cache [make-caching-proc (Procedure . -> . Procedure)])
(require/typed racket/serialize [serialize (Any . -> . Any)]
               [deserialize (Any . -> . (HashTable Any Any))])
(require math/flonum racket/list (only-in sugar/list values->list) racket/file)
(provide measure-text measure-ascent round-float update-text-cache-file load-text-cache-file)

(define precision 4.0)
(define base (flexpt 10.0 precision))

(define-syntax-rule (round-float x)
  (fl/ (flround (fl* base (fl x))) base))


(define dc (new record-dc%))

(define max-size 1024.0) ; use fixnum to trigger faster bitshift division

;; changing max-size invalidates font cache (because it's based on max size, duh)


(define make-font/caching
  (make-caching-proc (λ (font weight style)
                       (make-font #:size max-size #:style style #:weight weight #:face font))))


(define (get-cache-file-path)
  (build-path "font.cache"))


(define current-text-cache (make-parameter (make-hash '())))
(define current-text-cache-changed? (make-parameter #f))
(define current-font-cache (make-parameter (make-hash '())))


(define (update-text-cache-file)
  (when (current-text-cache-changed?)
    (write-to-file (serialize (current-text-cache)) (get-cache-file-path) #:exists 'replace)
    (current-text-cache-changed? #f)))

(define (load-text-cache-file) 
  (define cache-file-path (get-cache-file-path))
  (current-text-cache (if (file-exists? cache-file-path)
                          (deserialize (file->value cache-file-path))
                          (make-hash '()))))


(define-type mms-type ((String String) (Symbol Symbol) . ->* . (Listof Nonnegative-Flonum)))
(: measure-max-size mms-type)
(define measure-max-size 
  (cast (make-caching-proc (λ(text font [weight 'normal] [style 'normal])
                       ;((string? string?) (symbol? symbol?) . ->* . number?)
                       (define font-instance (hash-ref! (current-font-cache) (list font weight style) (λ() (make-font #:size max-size #:style style #:weight weight #:face font))))
                       ;; 'combine' boolean only makes a difference for two or more chars
                       (hash-ref! (current-text-cache) (list text font weight style) (λ() (current-text-cache-changed? #t)
                                                                                       (values->list (send dc get-text-extent text font-instance (>= (string-length text) 1))))))) mms-type))




(define-syntax-rule (width x) (first x))
(define-syntax-rule (height x) (second x))
(define-syntax-rule (descent x) (third x))
(define-syntax-rule (extra x) (fourth x)) 


(define-syntax-rule (measure-text-max-size text font weight style)
  (width (measure-max-size text font weight style)))

(: measure-text ((String Nonnegative-Flonum String) (Symbol Symbol) . ->* . Nonnegative-Flonum))
(define (measure-text text size font [weight 'normal] [style 'normal])
  ;; Native function only accepts integers, so get max-size and scale down to size needed.
  (define raw-measure (measure-text-max-size text font weight style))
  (cast (round-float (/ (* (exact->inexact raw-measure) (exact->inexact size)) max-size)) Nonnegative-Flonum))


(define-syntax-rule (measure-ascent-max-size text font weight style)
  (let ([result-list (measure-max-size text font weight style)])
    (- (height result-list) (descent result-list))))

(: measure-ascent ((String Nonnegative-Flonum String) (Symbol Symbol) . ->* . Nonnegative-Flonum))
(define (measure-ascent text size font [weight 'normal] [style 'normal])
  ;  ((string? flonum? string?) (symbol? symbol?) . ->* . flonum?)
  ;; Native function only accepts integers, so get max-size and scale down to size needed.
  (define raw-baseline-distance (measure-ascent-max-size text font weight style))
  (cast (round-float (/ (* (exact->inexact raw-baseline-distance) (exact->inexact size)) max-size)) Nonnegative-Flonum))
