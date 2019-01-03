#lang debug racket/base
(require racket/string racket/class racket/match racket/list txexpr racket/dict racket/function
         "quad.rkt" "param.rkt")
(provide (all-defined-out))
(module+ test (require rackunit))

(define (update-with base-hash . update-hashes)
  ;; starting with base-hash, add or update keys found in update-hashes
  (define h (make-hasheq))
  (for ([(k v) (in-dict (append-map hash->list (list* base-hash update-hashes)))])
       (hash-set! h k v))
  h)

(module+ test
  (check-equal?
   ((hasheq 'foo "bar" 'zim "zam") . update-with .  (hasheq 'zim "BANG") (hasheq 'toe "jam") (hasheq 'foo "zay"))
   (make-hasheq '((zim . "BANG") (foo . "zay") (toe . "jam")))))

(define (merge-whitespace qs [white-q? (λ (aq) (char-whitespace? (car (get-field elems aq))))])
  ;; collapse each sequence of whitespace qs to the first one, and make it a space
  ;; also drop leading & trailing whitespaces
  ;; (same behavior as web browsers)
  (let loop ([acc null][qs qs])
    (if (null? qs)
        (flatten acc)
        (let*-values ([(bs rest) (splitf-at qs (negate white-q?))]
                      [(ws rest) (splitf-at rest white-q?)])
          (loop (list acc bs (if (and (pair? rest) ;; we precede bs (only #t if rest starts with bs, because we took the ws)
                                      (pair? bs) ;; we follow bs
                                      (pair? ws)) ;; we have ws
                                 (quad (get-field attrs (car ws)) #\space)
                                 null)) rest)))))

#;(module+ test
    (check-equal? (merge-whitespace (list (q #\space) (q #\newline) (q #\H) (q #\space) (q #\newline) (q #\space) (q #\i) (q #\newline)))
                  (list (q #\H) (q #\space) (q #\i))))

(define (atomize qx)
  ;; normalize a quad by reducing it to one-character quads.
  ;; propagate attrs downward.
  (define atomic-quads
    (let loop ([x (if (string? qx) (q qx) qx)][attrs (current-default-attrs)])
      (match x
        [(? char? c) (list (q attrs c))]
        [(? string?) (append* (for/list ([c (in-string x)]) ;; strings are exploded
                                        (loop c attrs)))]
        [(? quad?) ;; qexprs with attributes are recursed
         (define this-attrs (get-field attrs x))
         (define elems (get-field elems x))
         (define merged-attrs (attrs . update-with . this-attrs))
         (append* (for/list ([elem (in-list elems)])
                            (loop elem merged-attrs)))]
        [else (raise-argument-error 'atomize "valid item" x)])))
  (merge-whitespace atomic-quads))

#;(module+ test
    (require rackunit)
    (check-equal? (atomize (q "Hi")) (list (q #\H) (q #\i)))
    (check-equal? (atomize (q "Hi " (q "You"))) (list (q #\H) (q #\i) (q #\space) (q #\Y) (q #\o) (q #\u)))
    (check-exn exn:fail:contract? (λ () (atomize #t)))
    (check-equal? (atomize (q "H i")) (list (q #\H) (q #\space) (q #\i)))
    (check-equal? (atomize (q "H \n\n i")) (list (q #\H) (q #\space) (q #\i))) ;; collapse whitespace to single

    ;; with attributes
    (check-equal? (atomize (q (hasheq 'k "v") "Hi")) (list (q (hasheq 'k "v") #\H) (q (hasheq 'k "v") #\i)))
    (check-equal? (atomize (q (hasheq 'k "v") "Hi " (q "You")))
                  (list
                   (quad (hasheq 'k "v") #\H)
                   (quad (hasheq 'k "v") #\i)
                   (quad (hasheq 'k "v") #\space)
                   (quad (hasheq 'k "v") #\Y)
                   (quad (hasheq 'k "v") #\o)
                   (quad (hasheq 'k "v") #\u)))
    (check-equal? (atomize (q (hasheq 'k1 "v1" 'k2 42) "Hi \n\n" (q (hasheq 'k1 "v2" 'k3 "foo") "\n \nYou")))
                  (list
                   (quad (hasheq 'k1 "v1" 'k2 42) #\H)
                   (quad (hasheq 'k1 "v1" 'k2 42) #\i)
                   (quad (hasheq 'k1 "v1" 'k2 42) #\space)
                   (quad (hasheq 'k1 "v2" 'k2 42 'k3 "foo") #\Y)
                   (quad (hasheq 'k1 "v2" 'k2 42 'k3 "foo") #\o)
                   (quad (hasheq 'k1 "v2" 'k2 42 'k3 "foo") #\u))))

(define whitespace-pat #px"\\s+")
(define (merge-and-isolate-white str)
  (for/list ([(m idx) (in-indexed (regexp-match* whitespace-pat str #:gap-select? #t))]
             #:when (non-empty-string? m))
            (if (even? idx) m " ")))

(define (merge-adjacent-strings xs [isolate-white? #false])
  (let loop ([xs xs][acc null])
    (match xs
      [(list) (reverse acc)]
      [(list (? string? strs) ..1 others ...)
       (loop others (append (reverse ((if isolate-white?
                                          merge-and-isolate-white
                                          list) (apply string-append strs))) acc))]
      [(cons x others) (loop others (cons x acc))])))

(define run-key 'run)

(define (same-run? qa qb)
  (eq? (hash-ref (get-field attrs qa) run-key) (hash-ref (get-field attrs qb) run-key)))

(define (runify qx)
  ;; runify a quad by reducing it to a series of "runs",
  ;; which are multi-character quads with the same formatting.
  (define first-key (gensym))
  (define first-attrs (hash-copy (current-default-attrs)))
  (hash-set! first-attrs 'idx first-key)
  (dropf
   (let loop ([x (if (string? qx) (q qx) qx)]
              [attrs first-attrs]
              [key first-key])
     (match x
       [(? quad?) ;; qexprs with attributes are recursed
        (define this-attrs (get-field attrs x))
        (define elems (get-field elems x))
        (define next-key (if (hash-empty? this-attrs) key (gensym)))
        (define next-attrs (if (hash-empty? this-attrs) attrs (attrs . update-with . this-attrs)))
        (unless (hash-empty? this-attrs) (hash-set! next-attrs run-key next-key))
        (append* (for/list ([elem (in-list (merge-adjacent-strings elems 'merge-white))])
                           (if (string? elem)
                               (list (q next-attrs elem))
                               (loop elem next-attrs next-key))))]))
   (λ (q) (string=? " " (car (get-field elems q))))))

#;(module+ test
    (check-equal?
     (runify  (quad (hasheq 'foo 42) (quad "Hi" "    idiot" (quad (hasheq 'bar 84) "There") "Eve" "ry" "one")))
     (list (quad (hasheq 'foo 42) "Hi") (quad (hasheq 'foo 42) " ") (quad (hasheq 'foo 42) "idiot") (quad (hasheq 'foo 42 'bar 84) "There") (quad (hasheq 'foo 42) "Everyone"))))
   
