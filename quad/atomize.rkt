#lang debug racket/base
(require racket/contract racket/match racket/list txexpr racket/dict sugar/list racket/function
         "quad.rkt" "qexpr.rkt" "param.rkt")
(provide (all-defined-out))
(module+ test (require rackunit))

(define (update-with base-hash . update-hashes)
  ;; starting with base-hash, add or update keys found in update-hashes
  (apply hasheq (flatten (map hash->list (list* base-hash update-hashes)))))

(module+ test
  (check-equal?
   ((hasheq 'foo "bar" 'zim "zam") . update-with .  (hasheq 'zim "BANG") (hasheq 'toe "jam") (hasheq 'foo "zay"))
   '#hasheq((zim . "BANG") (foo . "zay") (toe . "jam"))))

(define (merge-whitespace aqs)
  ;; collapse each sequence of whitespace aqs to the first one, and make it a space
  ;; also drop leading & trailing whitespaces
  ;; (same behavior as web browsers)
  (define (white-aq? aq) (char-whitespace? (car (qe aq))))
  (let loop ([acc null][aqs aqs])
    (if (null? aqs)
        (flatten acc)
        (let*-values ([(bs rest) (splitf-at aqs (negate white-aq?))]
                      [(ws rest) (splitf-at rest white-aq?)])
          (loop (list acc bs (if (and (pair? rest) ;; we precede bs (only #t if rest starts with bs, because we took the ws)
                                      (pair? bs) ;; we follow bs
                                      (pair? ws)) ;; we have ws
                                 (break (qa (car ws)) #\space)
                                 null)) rest)))))

(module+ test
  (check-equal? (merge-whitespace (list (q #\space) (q #\newline) (q #\H) (q #\space) (q #\newline) (q #\space) (q #\i) (q #\newline)))
                (list (q #\H) (b #\space) (q #\i))))

(define/contract (atomize qx)
  ;; normalize a quad by reducing it to one-character quads.
  ;; propagate attrs downward.
  ((or/c quad? string?) . -> . (listof atomic-quad?))
  (define atomic-quads
    (let loop ([x (if (string? qx) (q qx) qx)][attrs (current-default-attrs)])
      (match x
        [(? char? c) (list (q attrs c))]
        [(? string?) (append* (for/list ([c (in-string x)]) ;; strings are exploded
                                        (loop c attrs)))]
        [($quad this-attrs elems) ;; qexprs with attributes are recursed
         (define merged-attrs (attrs . update-with . this-attrs))
         (append* (for/list ([elem (in-list elems)])
                            (loop elem merged-attrs)))]
        [else (raise-argument-error 'atomize "valid item" x)])))
  (merge-whitespace atomic-quads))

(module+ test
  (require rackunit)
  (check-equal? (atomize (q "Hi")) (list (q #\H) (q #\i)))
  (check-equal? (atomize (q "Hi " (q "You"))) (list (q #\H) (q #\i) (b #\space) (q #\Y) (q #\o) (q #\u)))
  (check-exn exn:fail:contract? (λ () (atomize #t)))
  (check-equal? (atomize (q "H i")) (list (q #\H) (b #\space) (q #\i)))
  (check-equal? (atomize (q "H \n\n i")) (list (q #\H) (b #\space) (q #\i))) ;; collapse whitespace to single

  ;; with attributes
  (check-equal? (atomize (q (hasheq 'k "v") "Hi")) (list (q (hasheq 'k "v") #\H) (q (hasheq 'k "v") #\i)))
  (check-equal? (atomize (q (hasheq 'k "v") "Hi " (q "You")))
                (list
                 ($quad '#hasheq((k . "v")) '(#\H))
                 ($quad '#hasheq((k . "v")) '(#\i))
                 ($break '#hasheq((k . "v")) '(#\space))
                 ($quad '#hasheq((k . "v")) '(#\Y))
                 ($quad '#hasheq((k . "v")) '(#\o))
                 ($quad '#hasheq((k . "v")) '(#\u))))
  (check-equal? (atomize (q (hasheq 'k1 "v1" 'k2 42) "Hi \n\n" (q (hasheq 'k1 "v2" 'k3 "foo") "\n \nYou")))
                (list
                 ($quad '#hasheq((k1 . "v1") (k2 . 42)) '(#\H))
                 ($quad '#hasheq((k1 . "v1") (k2 . 42)) '(#\i))
                 ($break '#hasheq((k1 . "v1") (k2 . 42)) '(#\space))
                 ($quad '#hasheq((k1 . "v2") (k2 . 42) (k3 . "foo")) '(#\Y))
                 ($quad '#hasheq((k1 . "v2") (k2 . 42) (k3 . "foo")) '(#\o))
                 ($quad '#hasheq((k1 . "v2") (k2 . 42) (k3 . "foo")) '(#\u)))))