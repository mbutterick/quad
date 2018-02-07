#lang sugar/debug racket/base
(require racket/contract racket/match racket/list txexpr racket/dict
         "quad.rkt" "qexpr.rkt" "param.rkt")
(provide (all-defined-out))

(define (update-with base-hash . update-hashes)
  ;; starting with base-hash, add or update keys found in update-hashes
  (apply hasheq (flatten (map hash->list (list* base-hash update-hashes)))))

(module+ test
  (check-equal?
   ((hasheq 'foo "bar" 'zim "zam") . update-with .  (hasheq 'zim "BANG") (hasheq 'toe "jam") (hasheq 'foo "zay"))
   '#hasheq((zim . "BANG") (foo . "zay") (toe . "jam"))))

(define/contract (atomize qx)
  ;; normalize a quad by reducing it to one-character quads.
  ;; propagate attrs downward.
  (quad? . -> . atomic-quads?)
  (let loop ([x qx][attrs (current-default-attrs)])
    (match x
      [(? char? c) (list (q attrs c))]
      [(? string?) (append* (for/list ([c (in-string x)]) ;; strings are exploded
                                      (loop c attrs)))]
      [($quad this-attrs elems) ;; qexprs with attributes are recursed
       (define merged-attrs (attrs . update-with . this-attrs))
       (append* (for/list ([elem (in-list elems)])
                          (loop elem merged-attrs)))]
      [else (raise-argument-error 'atomize "valid item" x)])))

(module+ test
  (require rackunit)
  (check-equal? (atomize (q "Hi")) (list (q #\H) (q #\i)))
  (check-equal? (atomize (q "Hi " (q "You"))) (list (q #\H) (q #\i) (q #\space) (q #\Y) (q #\o) (q #\u)))
  (check-exn exn:fail:contract? (λ () (atomize #t)))

  ;; with attributes
  (check-equal? (atomize (q (hasheq 'k "v") "Hi")) (list (q (hasheq 'k "v") #\H) (q (hasheq 'k "v") #\i)))
  (check-equal? (atomize (q (hasheq 'k "v") "Hi " (q "You")))
                (list
                 ($quad '#hasheq((k . "v")) '(#\H))
                 ($quad '#hasheq((k . "v")) '(#\i))
                 ($quad '#hasheq((k . "v")) '(#\space))
                 ($quad '#hasheq((k . "v")) '(#\Y))
                 ($quad '#hasheq((k . "v")) '(#\o))
                 ($quad '#hasheq((k . "v")) '(#\u))))
  (check-equal? (atomize (q (hasheq 'k1 "v1" 'k2 42) "Hi " (q (hasheq 'k1 "v2" 'k3 "foo") "You")))
                (list
                 ($quad '#hasheq((k1 . "v1") (k2 . 42)) '(#\H))
                 ($quad '#hasheq((k1 . "v1") (k2 . 42)) '(#\i))
                 ($quad '#hasheq((k1 . "v1") (k2 . 42)) '(#\space))
                 ($quad '#hasheq((k1 . "v2") (k2 . 42) (k3 . "foo")) '(#\Y))
                 ($quad '#hasheq((k1 . "v2") (k2 . 42) (k3 . "foo")) '(#\o))
                 ($quad '#hasheq((k1 . "v2") (k2 . 42) (k3 . "foo")) '(#\u)))))