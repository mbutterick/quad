#lang br
(require xml racket/contract txexpr)
(provide (all-defined-out))

(module+ test (require rackunit))

(define/contract (qexpr? x)
  ;; a qexpr is like an xexpr, but more lenient in some ways (allows single char as body element)
  ;; and less in others (only allows 'q or 'quad as tag names)
  (any/c . -> . boolean?)
  (define (valid-tag? tag) (and (memq tag '(q quad)) #t))
  (match x
    [(? txexpr?) (valid-tag? (get-tag x))]
    [(list (? symbol? tag) (? char? c)) (valid-tag? tag)]
    [(? string?) #t]
    [else #f]))

(module+ test
  (check-true (qexpr? "Hello world"))
  (check-true (qexpr? '(q "Hello world")))
  (check-true (qexpr? '(quad "Hello world")))
  (check-false (qexpr? '(div "Hello world")))
  (check-true (qexpr? '(q #\H)))
  (check-true (qexpr? '(quad #\H)))
  (check-false (qexpr? '(span #\H)))
  (check-true (qexpr? '(quad "Hello world")))
  (check-false (qexpr? 'q)))

(define/contract (qexpr attrs . elems)
  ((txexpr-attrs?) #:rest txexpr-elements? . ->* . qexpr?)
  (txexpr 'q (remove-duplicates attrs #:key car) elems))

(module+ test
  (check-equal? (qexpr null "foo") '(q "foo"))
  (check-equal? (qexpr '((k "v")) "foo") '(q ((k "v")) "foo"))
  (check-equal? (qexpr '((k "v2")(k "v1")) "foo") '(q ((k "v2")) "foo")))

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