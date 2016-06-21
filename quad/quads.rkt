#lang racket/base
(provide (all-defined-out))
(require racket/string racket/list (for-syntax  racket/base racket/syntax))

(struct $quad (attrs dim val) #:transparent #:mutable)
(struct $black $quad () #:transparent)
(struct $soft $quad () #:transparent)
(struct $hard $quad () #:transparent)

(define quad? $quad?)

(define quad-attrs $quad-attrs)
(define quad-val $quad-val)

(define (quad-attrs? x) (list? x))


#|
Attrs needed to specify rendered appearance:
(font) family
(font) style 
(font) size
color
background
position
measure (line width)

|#
(define default-attrs (hasheq 'size 10 'font "sc.otf")) ; Source Code Pro, 12 pt, chars are 6pt wide

(define (munge-whitespace str)
  ;; reduce multiple whitespace to single
  ;; trim remaining
  (string-trim (regexp-replace* #px"\\s+" str " ")))

(define (merge-strings xs)
  ;; merge consecutive strings
  (let loop ([xs xs])
    (cond
      [(empty? xs) empty]
      [else
       (define-values (strs rest) (splitf-at xs string?))
       (define-values (nonstrs restrest) (splitf-at rest (λ(x) (not (string? x)))))
       (append (if (empty? strs)
                   empty
                   (list (munge-whitespace (string-append* strs)))) nonstrs (loop restrest))])))

#;(define (merge-strings1 xs)
    ;; merge consecutive strings
    (define-values (last-list list-of-lists last-negating)
      (for/fold ([current-list empty]
                 [list-of-lists empty]
                 [negating? #f])
                ([x (in-list xs)])
        (define current-pred (if negating? (λ (x) (not (string? x))) string?))
        (if (current-pred x)
            (values (cons x current-list) list-of-lists negating?)
            (values (cons x null) (if (not (empty? current-list))
                                      (cons (reverse current-list) list-of-lists)
                                      list-of-lists) (not negating?)))))
    (append-map (λ(xs) (if (string? (car xs))
                           (list (munge-whitespace (string-append* xs)))
                           xs))
                (reverse (cons (reverse last-list) list-of-lists))))

#;(require sugar/list)
#;(define (merge-strings xs)
    (append-map (λ(xis) (if (string? (car xis))
                            (list (munge-whitespace (string-append* xis)))
                            xis)) (slicef xs string?)))


(struct $attrs (size font) #:transparent)
(define (make-attrs #:size [size #f]
                    #:font [font #f])
  (hasheq 'size size 'font font))


(define (quad attr . xs)
  ;; squeeze excess whitespace out of quad args
  ;; todo: find way to do this with less allocation
  ($quad (or attr (make-attrs)) 0 (merge-strings xs)))


(define (quad-dim q)
  ($quad-dim q))

(define (quad-font q)
  (hash-ref (quad-attrs q) 'font))
(define (quad-font-size q)
  (hash-ref (quad-attrs q) 'size))

(define (quad-dim-set! q val)
  (set-$quad-dim! q val))

(define (override-with dest source)
  ;; replace missing values in dest with values from source
  (for/hasheq ([k (in-hash-keys source)])
              (values k (or (hash-ref dest k) (hash-ref source k)))))

(require (for-syntax sugar/debug))
(define-syntax-rule (define-break name)
  (define (name) (quad #f 'name)))

(define-break page-break)
(define-break column-break)
(define-break block-break)
(define-break line-break)


(define-syntax (caseq stx)
  ;; like case but strictly uses `eq?` comparison (as opposed to `equal?`)
  (syntax-case stx ()
    [(_ test-val [(match-val ...) . result] ... [else . else-result])
     #'(cond
         [(memq test-val '(match-val ...)) . result] ...
         [else . else-result])]
    [(_ test-val [(match-val ...) . result] ...)
     #'(caseq test-val
              [(match-val ...) . result] ...
              [else (error 'caseq "no match")])]))

(module+ test
  (require rackunit)
  (define q (quad #f "bar"))
  (check-true (quad? q))
  (check-false (quad? 42))
  (check-equal? (quad-attrs q) (make-attrs))
  (check-equal? (quad-val q) '("bar"))
  (check-equal? (merge-strings '(50 " foo   " "   bar  " 42 "  zam")) '(50 "foo bar" 42 "zam")))