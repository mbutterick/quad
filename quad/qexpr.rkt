#lang debug racket/base
(require xml
         racket/contract
         racket/dict
         racket/string
         racket/match
         racket/list
         txexpr 
         "quad.rkt" "generic.rkt" sugar/debug)
(provide (all-defined-out))

(module+ test (require rackunit))

(define/contract (qexpr? x)
  ;; a qexpr is like an xexpr, but more lenient in some ways (allows single char as body element)
  ;; and less in others (only allows 'q or 'quad as tag names)
  (any/c . -> . boolean?)
  (define (valid-tag? tag) (and (memq tag '(q quad)) #t))
  (match x
    [(? txexpr?) #t]
    [(list (? symbol? tag) (? char? c)) #t]
    [(? string?) #t]
    [else #f]))


(module+ test
  (check-true (qexpr? "Hello world"))
  (check-true (qexpr? '(q "Hello world")))
  (check-true (qexpr? '(quad "Hello world")))
  #;(check-false (qexpr? '(div "Hello world")))
  (check-true (qexpr? '(q #\H)))
  (check-true (qexpr? '(quad #\H)))
  #;(check-false (qexpr? '(span #\H)))
  (check-true (qexpr? '(quad "Hello world")))
  (check-false (qexpr? 'q)))

(define (quad-name q) (string->symbol (string-trim (symbol->string (object-name q)) "$")))

(define/contract (qexpr #:clean-attrs? [clean-attrs? #f]
                        #:name [name 'q]
                        attrs . elems)
  ((txexpr-attrs?) (#:clean-attrs? any/c #:name txexpr-tag?) #:rest (or/c txexpr-elements? (list/c char?)) . ->* . qexpr?)
  (txexpr name (if clean-attrs? (remove-duplicates attrs #:key car) attrs) (match elems
                                                                             [(list (? char? c)) (list (string c))]
                                                                             [else elems])))

(module+ test
  (check-equal? (qexpr null "foo") '(q "foo"))
  (check-equal? (qexpr '((k "v")) "foo") '(q ((k "v")) "foo"))
  (check-equal? (qexpr '((k "v2")(k "v1")) "foo") '(q ((k "v2")(k "v1")) "foo"))
  (check-equal? (qexpr #:clean-attrs? #t '((k "v2")(k "v1")) "foo") '(q ((k "v2")) "foo")))

(define (hash->qattrs attr-hash)
  (for/list ([(k v) (in-dict (hash->list attr-hash))])
    (list k (format "~a" v))))

(define/contract (quad->qexpr q)
  (quad? . -> . qexpr?)
  (let loop ([x q])
    (cond
      [(quad? x) (apply qexpr #:name (quad-name x) #:clean-attrs? #t (hash->qattrs (attrs x)) (map loop (elems x)))]
      [else x])))

(define/contract (qexpr->quad x)
  (qexpr? . -> . quad?)
  (if (txexpr? x)
      ($quad (attrs->hash (get-attrs x)) (map qexpr->quad (get-elements x)))
      x))

(define/contract (qml->qexpr x)
  (string? . -> . qexpr?)
  (parameterize ([permissive-xexprs #t]
                 [xexpr-drop-empty-attributes #t])
    (string->xexpr x)))

(define/contract (qexpr->qml x)
  (qexpr? . -> . string?)
  (xexpr->string x))

(module+ test
  (check-equal? (qml->qexpr (qexpr->qml '(q "hi"))) '(q "hi"))
  (check-equal? (qml->qexpr (qexpr->qml '(q () "hi"))) '(q "hi"))
  (check-equal? (qml->qexpr (qexpr->qml '(q ((foo "bar")) "hi"))) '(q ((foo "bar")) "hi")))