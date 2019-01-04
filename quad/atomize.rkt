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

(define (merge-whitespace qs [white-q? (λ (aq) (char-whitespace? (car (quad-elems aq))))])
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
                                 (make-quad (quad-attrs (car ws)) '(#\space))
                                 null)) rest)))))


(module+ test
  (define (qq . xs) (q #f xs))
  (define (qqa attrs . xs) (q attrs xs))
  (check-equal? (merge-whitespace (list (qq #\space) (qq #\newline) (qq #\H) (qq #\space) (qq #\newline) (qq #\space) (qq #\i) (qq #\newline)))
                (list (qq #\H) (qq #\space) (qq #\i))))

(define (atomize qx)
  ;; normalize a quad by reducing it to one-character quads.
  ;; propagate attrs downward.
  (define atomic-quads
    (let loop ([x (if (string? qx) (q #f (list qx)) qx)][attrs (current-default-attrs)])
      (match x
        [(? char? c) (list (q attrs (list c)))]
        [(? string?) (append* (for/list ([c (in-string x)]) ;; strings are exploded
                                        (loop c attrs)))]
        [(? quad?) ;; qexprs with attributes are recursed
         (define this-attrs (quad-attrs x))
         (define elems (quad-elems x))
         (define merged-attrs (attrs . update-with . this-attrs))
         (append* (for/list ([elem (in-list elems)])
                            (loop elem merged-attrs)))]
        [else (raise-argument-error 'atomize "valid item" x)])))
  (merge-whitespace atomic-quads))

(module+ test
  (require rackunit)
  (check-equal? (atomize (qq "Hi")) (list (qq #\H) (qq #\i)))
  (check-equal? (atomize (qq "Hi " (qq "You"))) (list (qq #\H) (qq #\i) (qq #\space) (qq #\Y) (qq #\o) (qq #\u)))
  (check-exn exn:fail:contract? (λ () (atomize #t)))
  (check-equal? (atomize (qq "H i")) (list (qq #\H) (qq #\space) (qq #\i)))
  (check-equal? (atomize (qq "H \n\n i")) (list (qq #\H) (qq #\space) (qq #\i))) ;; collapse whitespace to single

  ;; with attributes
  (check-equal? (atomize (qqa (hasheq 'k "v") "Hi")) (list (qqa (hasheq 'k "v") #\H) (qqa (hasheq 'k "v") #\i)))
  (check-equal? (atomize (qqa (hasheq 'k "v") "Hi " (qq "You")))
                (list
                 (qqa (hasheq 'k "v") #\H)
                 (qqa (hasheq 'k "v") #\i)
                 (qqa (hasheq 'k "v") #\space)
                 (qqa (hasheq 'k "v") #\Y)
                 (qqa (hasheq 'k "v") #\o)
                 (qqa (hasheq 'k "v") #\u)))
  (check-equal? (atomize (qqa (hasheq 'k1 "v1" 'k2 42) "Hi \n\n" (qqa (hasheq 'k1 "v2" 'k3 "foo") "\n \nYou")))
                (list
                 (qqa (hasheq 'k1 "v1" 'k2 42) #\H)
                 (qqa (hasheq 'k1 "v1" 'k2 42) #\i)
                 (qqa (hasheq 'k1 "v1" 'k2 42) #\space)
                 (qqa (hasheq 'k1 "v2" 'k2 42 'k3 "foo") #\Y)
                 (qqa (hasheq 'k1 "v2" 'k2 42 'k3 "foo") #\o)
                 (qqa (hasheq 'k1 "v2" 'k2 42 'k3 "foo") #\u))))

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
        (define this-attrs (quad-attrs x))
        (define elems (quad-elems x))
        (define next-key (if (hash-empty? this-attrs) key (gensym)))
        (define next-attrs (if (hash-empty? this-attrs) attrs (attrs . update-with . this-attrs)))
        (unless (hash-empty? this-attrs) (hash-set! next-attrs run-key next-key))
        (append* (for/list ([elem (in-list (merge-adjacent-strings elems 'merge-white))])
                           (if (string? elem)
                               (list (make-quad next-attrs (list elem)))
                               (loop elem next-attrs next-key))))]))
   (λ (q) (string=? " " (car (quad-elems q))))))

#;(module+ test
    (check-equal?
     (runify (qqa (hasheq 'foo 42) (qq "Hi" "    idiot" (qqa (hasheq 'bar 84) "There") "Eve" "ry" "one")))
     (list (qqa (hasheq 'foo 42) "Hi") (qqa (hasheq 'foo 42) " ") (qqa (hasheq 'foo 42) "idiot") (qqa (hasheq 'foo 42 'bar 84) "There") (qqa (hasheq 'foo 42) "Everyone"))))
   
