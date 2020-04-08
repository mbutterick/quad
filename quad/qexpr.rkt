#lang debug racket/base
(require xml
         racket/dict
         racket/string
         racket/match
         racket/list
         racket/path
         txexpr 
         "quad.rkt")
(provide (all-defined-out))

(module+ test (require rackunit))

;; should we allow quads within a qexpr? I say yes
(define permissive-qexprs (make-parameter #t))

(define (valid-tag? tag) (symbol? tag))

(define (qexpr? x)
  ;; a qexpr is like an xexpr, but more lenient in some ways (possibly allows quads)
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
  (check-true (qexpr? '(div "Hello world")))
  (check-false (qexpr? '(q #\H)))
  (check-false (qexpr? '(quad #\H)))
  (check-false (qexpr? '(span #\H)))
  (check-true (qexpr? '(quad "Hello world")))
  (check-true (qexpr? `(quad "Hello " ,(q "world")))))

(define (quad-qexpr-name q) (string->symbol (string-trim (symbol->string (object-name q)) "$")))

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
      [(quad? x) (apply qexpr #:name (quad-qexpr-name x) #:clean-attrs? #t (hash->qattrs (quad-attrs x)) (map loop (quad-elems x)))]
      [else x])))

(define (qexpr->quad x)
  (unless (qexpr? x)
    (raise-argument-error 'qexpr->quad "qexpr" x))
  (let loop ([x x])
    (match x
      [(cons (? valid-tag? tag) rest)
       (match rest
         [(list (? txexpr-attrs? attrs) (? qexpr? elems) ...)
          (define mheq (make-hasheq)) ; want mutable hash
          (for ([kv (in-list attrs)])
               (match-define (list k v) kv)
               ;; coerce number strings to actual numbers
               ;; this misbehaves on a list index like "1." which becomes 1.0
               (hash-set! mheq k (cond
                                   [(equal? v "true") #true]
                                   [(equal? v "false") #false]
                                   [(string->number v)]
                                   [else v])))
          (make-quad #:tag tag
                     #:attrs mheq
                     #:elems (map loop elems))]
         [(list (? qexpr? elems) ...)
          (make-quad #:tag tag
                     #:elems (map loop elems))])]
      [_ x])))

(module+ test
  (check-true
   (quad=?
    (qexpr->quad  `(q ((font "charter") (fontsize "12")) (q "Foo bar") ,(make-quad "zzz") (q "Zim Zam")))
    (q (hasheq 'font "charter" 'fontsize 12) (q "Foo bar") (q "zzz") (q "Zim Zam")))))

(define qml-extension #".qml")
(define (qml-path? x)
  (and (or (path-string? x) (path-for-some-system? x))
       (for/or ([ext (in-list (list qml-extension
                                    (string->bytes/utf-8
                                     (string-upcase
                                      (bytes->string/utf-8 qml-extension)))))])
               (path-has-extension? x ext))))

(module+ test
  (check-true (qml-path? "foo.qml"))
  (check-true (qml-path? "foo.QML"))
  (check-false (qml-path? "foo.QmL"))
  (check-false (qml-path? "foo.qmla")))

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