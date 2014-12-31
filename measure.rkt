#lang racket/base
(require math/flonum racket/draw racket/class racket/contract sugar/debug sugar/list racket/list sugar/cache racket/serialize racket/file)
(provide measure-text measure-ascent round-float update-text-cache-file load-text-cache-file)

(define precision 4.0)
(define base (flexpt 10.0 precision))

(define-syntax-rule (round-float x)
  (fl/ (flround (fl* base (fl x))) base))

(define dc (new record-dc%))

(define max-size 1000.0)

(define/caching (make-font/caching font weight style)
  (make-font #:size max-size #:style style #:weight weight #:face font))

(define (get-cache-file-path)
  (build-path "font.cache"))

(define (update-text-cache-file)
  (when (current-text-cache-changed?)
    (write-to-file (serialize (current-text-cache)) (get-cache-file-path) #:exists 'replace)
    (current-text-cache-changed? #f)))

(define (load-text-cache-file) 
  (define cache-file-path (get-cache-file-path))
  (current-text-cache (if (file-exists? cache-file-path)
      (deserialize (file->value cache-file-path))
      (make-hash))))

(define current-text-cache (make-parameter (make-hash)))
(define current-text-cache-changed? (make-parameter #f))
(define current-font-cache (make-parameter (make-hash)))

(define/caching (measure-max-size text font [weight 'normal] [style 'normal])
  ;((string? string?) (symbol? symbol?) . ->* . number?)
  (define font-instance (hash-ref! (current-font-cache) (list font weight style) (λ() (make-font #:size max-size #:style style #:weight weight #:face font))))
  ;; combine boolean only makes a difference for two or more chars
  (hash-ref! (current-text-cache) (list text font weight style) (λ() (current-text-cache-changed? #t)
                                                                  (values->list (send dc get-text-extent text font-instance (>= (string-length text) 1))))))

(define (width x) (first x))
(define (height x) (second x))
(define (descent x) (third x))
(define (extra x) (fourth x)) 

(define (measure-text-max-size text font [weight 'normal] [style 'normal])
  (width (measure-max-size text font weight style)))

(define/contract (measure-text text size font [weight 'normal] [style 'normal])
  ((string? flonum? string?) (symbol? symbol?) . ->* . flonum?)
  ;; Native function only accepts integers, so get max-size and scale down to size needed.
  (define raw-measure (measure-text-max-size text font weight style))
  (round-float (fl/ (fl* (fl raw-measure) size) max-size)))


(define (measure-ascent-max-size text font [weight 'normal] [style 'normal])
  (define result-list (measure-max-size text font weight style))
  (- (height result-list) (descent result-list)))


(define/contract (measure-ascent text size font [weight 'normal] [style 'normal])
  ((string? flonum? string?) (symbol? symbol?) . ->* . flonum?)
  ;; Native function only accepts integers, so get max-size and scale down to size needed.
  (define raw-baseline-distance (measure-ascent-max-size text font weight style))
  (round-float (fl/ (fl* (fl raw-baseline-distance) size) max-size)))
