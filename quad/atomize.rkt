#lang racket/base
(require racket/contract racket/match racket/list txexpr sugar/debug
         "qexpr.rkt")
(provide (all-defined-out))

(define/contract (atomize qx)
  ;; normalize a qexpr by reducing it to one-character quads.
  ;; propagate attrs downward by appending to front of attrs list.
  ;; at then end, duplicates are removed, with frontmost attrs (= added later) given preference
  (qexpr? . -> . (listof qexpr?))
  (let loop ([x qx][attrs null])
    (match x
      [(? string?) (for/list ([c (in-string x)]) ;; strings are exploded
                             (qexpr attrs (string c)))]
      [(list (? symbol?) (? txexpr-attrs? new-attrs) xs ...) ;; qexprs with attributes are recursed
       (append* (for/list ([x (in-list xs)])
                          (loop x (append new-attrs attrs))))]
      [(list (? symbol? tag) xs ...) (loop (list* tag null xs) attrs)] ;; qexprs without attributes get null attrs
      [else (raise-argument-error 'atomize "valid item" x)])))

(module+ test
  (require rackunit)
  (check-equal? (atomize "Hi") '((q "H") (q "i")))
  (check-equal? (atomize '(q "Hi " (q "You"))) '((q "H") (q "i") (q " ") (q "Y") (q "o") (q "u")))
  (check-exn exn:fail? (λ () (atomize #t)))

  ;; with attributes
  (check-equal? (atomize '(q ((k "v")) "Hi")) '((q ((k "v")) "H") (q ((k "v")) "i")))
  (check-equal? (atomize '(q ((k "v")) "Hi " (q "You")))
                '((q ((k "v")) "H")
                  (q ((k "v")) "i")
                  (q ((k "v")) " ")
                  (q ((k "v")) "Y")
                  (q ((k "v")) "o")
                  (q ((k "v")) "u")))
  (check-equal? (atomize '(q ((k1 "v1")(k2 "42")) "Hi " (q ((k1 "v2")(k3 "foo")) "You")))
                '((q ((k1 "v1")(k2 "42")) "H")
                  (q ((k1 "v1")(k2 "42")) "i")
                  (q ((k1 "v1")(k2 "42")) " ")
                  (q ((k1 "v2")(k3 "foo")(k2 "42")) "Y")
                  (q ((k1 "v2")(k3 "foo")(k2 "42")) "o")
                  (q ((k1 "v2")(k3 "foo")(k2 "42")) "u"))))