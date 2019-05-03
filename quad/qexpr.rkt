#lang debug racket/base
(require xml
         racket/dict
         racket/string
         racket/match
         racket/list
         txexpr 
         "quad.rkt")
(provide (all-defined-out))

(module+ test (require rackunit))

;; should we allow quads within a qexpr? I say yes
(define permissive-qexprs (make-parameter #t))

(define (valid-tag? tag) (and (memq tag '(q quad)) #t))

(define (qexpr? x)
  ;; a qexpr is like an xexpr, but more lenient in some ways (possibly allows quads)
  ;; and less in others (only allows 'q or 'quad as tag names, only allows strings or qexprs as elements)
  ;; attrs are open-ended
  (match x
    [(cons (? valid-tag?) rest)
     (match rest
       [(list (? txexpr-attrs?) (? qexpr?) ...) #t]
       [(list (? qexpr?) ...) #t]
       [_ #f])]
    [(? string?) #t]
    [(? quad?) (permissive-qexprs)]
    [_ #f]))


(module+ test
  (check-true (qexpr? "Hello world"))
  (check-true (qexpr? '(q "Hello world")))
  (check-true (qexpr? '(quad "Hello world")))
  (check-false (qexpr? '(div "Hello world")))
  (check-false (qexpr? '(q #\H)))
  (check-false (qexpr? '(quad #\H)))
  (check-false (qexpr? '(span #\H)))
  (check-true (qexpr? '(quad "Hello world")))
  (check-true (qexpr? `(quad "Hello " ,(q "world")))))

(define (quad-name q) (string->symbol (string-trim (symbol->string (object-name q)) "$")))

(define (qexpr #:clean-attrs? [clean-attrs? #f]
               #:name [name 'q]
               attrs . elems)
  (define new-attrs (if clean-attrs? (remove-duplicates attrs #:key car) attrs))
  (define new-elems (match elems
                      [(list (? char? c)) (list (string c))]
                      [(list (? list? xs)) xs]
                      [else elems]))
  (cond
    [(empty? new-attrs) (list* name new-elems)]
    [else (list* name new-attrs new-elems)]))

(module+ test
  (check-equal? (qexpr null "foo") '(q "foo"))
  (check-equal? (qexpr '((k "v")) "foo") '(q ((k "v")) "foo"))
  (check-equal? (qexpr '((k "v2")(k "v1")) "foo") '(q ((k "v2")(k "v1")) "foo"))
  (check-equal? (qexpr #:clean-attrs? #t '((k "v2")(k "v1")) "foo") '(q ((k "v2")) "foo")))

(define (hash->qattrs attr-hash)
  (for/list ([(k v) (in-dict (hash->list attr-hash))])
    (list k (format "~a" v))))

(define (quad->qexpr q)
  (let loop ([x q])
    (cond
      [(quad? x) (apply qexpr #:name (quad-name x) #:clean-attrs? #t (hash->qattrs (quad-attrs x)) (map loop (quad-elems x)))]
      [else x])))

(define (qexpr->quad x)
  (unless (qexpr? x)
    (raise-argument-error 'qexpr->quad "qexpr" x))
  (let loop ([x x])
    (match x
      [(cons (? valid-tag?) rest)
       (match rest
         [(list (? txexpr-attrs? attrs) (? qexpr? elems) ...)
          (define mheq (make-hasheq)) ; want mutable hash
          (for ([kv (in-list attrs)])
            (match-define (list k v) kv)
            ;; coerce number strings to actual numbers
            ;; this misbehaves on a list index like "1." which becomes 1.0
            (hash-set! mheq k (or (string->number v) (string-downcase v))))
          (q #:attrs mheq #:elems (map loop elems))]
         [(list (? qexpr? elems) ...)
          (q #:elems (map loop elems))])]
      [_ x])))

(module+ test
  (check-true
   (quad=?
    (qexpr->quad  `(q ((font "charter") (fontsize "12")) (q "Foo bar") ,(make-quad "zzz") (q "Zim Zam")))
    (q (hasheq 'font "charter" 'fontsize 12) (q "Foo bar") (q "zzz") (q "Zim Zam")))))

(define (qml->qexpr x)
  (parameterize ([permissive-xexprs #t]
                 [xexpr-drop-empty-attributes #t])
    (string->xexpr x)))

(define (qexpr->qml x)
  (xexpr->string x))

(module+ test
  (check-equal? (qml->qexpr (qexpr->qml '(q "hi"))) '(q "hi"))
  (check-equal? (qml->qexpr (qexpr->qml '(q () "hi"))) '(q "hi"))
  (check-equal? (qml->qexpr (qexpr->qml '(q ((foo "bar")) "hi"))) '(q ((foo "bar")) "hi")))