#lang debug racket/base
(require racket/string
         racket/hash
         racket/match
         racket/list
         racket/struct
         txexpr
         sugar/list
         racket/function
         "quad.rkt"
         "qexpr.rkt"
         "param.rkt")
(provide (all-defined-out))

(module+ test
  (require rackunit))

(define (update-with! h . update-hashes)
  (apply hash-union! #:combine (λ (v1 v2) v2) h update-hashes))

(define (update-with base-hash . update-hashes)
  ;; starting with base-hash, add or update keys found in update-hashes
  (define h (make-hasheq))
  (apply update-with! h base-hash update-hashes)
  h)

(module+ test
  (check-equal?
   ((hasheq 'foo "bar" 'zim "zam") . update-with .  (hasheq 'zim "BANG") (hasheq 'toe "jam") (hasheq 'foo "zay"))
   (make-hasheq '((zim . "BANG") (foo . "zay") (toe . "jam")))))

(define whitespace-pat #px"\\s+")

(define (merge-and-isolate-white str)
  (for/list ([(m idx) (in-indexed (regexp-match* whitespace-pat str #:gap-select? #t))]
             #:when (non-empty-string? m))
    (if (even? idx) m " ")))

(define (merge-adjacent-strings xs [isolate-white? #false])
  (let loop ([xs xs][acc null])
    (match xs
      [(== empty) (reverse acc)]
      [(list (? string? strs) ..1 others ...)
       (loop others (append (reverse ((if isolate-white?
                                          merge-and-isolate-white
                                          list) (apply string-append strs))) acc))]
      [(cons x others) (loop others (cons x acc))])))

(define run-key 'run)

(define (same-run? qa qb)
  (eq? (quad-ref qa run-key) (quad-ref qb run-key)))

(define (atomize qx #:attrs-proc [attrs-proc values])
  ;; atomize a quad by reducing it to the smallest indivisible formatting units.
  ;; which are multi-character quads with the same formatting.
  (define atomized-qs
    (let loop ([x (make-quad qx)]
               [attrs (hash-copy (current-default-attrs))]
               [key (eq-hash-code (current-default-attrs))])
      (match-define-values (next-key next-attrs)
        ;; make a new run when we encounter non-empty attrs
        (match (quad-attrs x)
          [(? hash-empty?) (values key attrs)]
          [this-attrs (define next-key (eq-hash-code this-attrs))
                      (define next-attrs (attrs . update-with . this-attrs))
                      (hash-set! next-attrs run-key next-key)
                      (attrs-proc next-attrs)
                      (values next-key next-attrs)]))
      (match (quad-elems x)
        [(? null?) ((quad-attrs x) . update-with! . next-attrs) (list x)]
        [_
         ;; we don't use `struct-copy` here because it needs to have the structure id at compile time.
         ;; whereas with this technique, we can extract a constructor for any structure type.
         ;; notice that the technique depends on
         ;; 1) we only need to update attrs and elems
         ;; 2) we make them the first two fields, so we know to drop the first two fields of x-tail
         (define x-constructor (derive-quad-constructor x))
         (define x-tail (drop (struct->list x) 2))
         (match (merge-adjacent-strings (quad-elems x) 'isolate-white)
           [(? pair? merged-elems)
            (append* 
             (for/list ([elem (in-list merged-elems)])
               (match elem
                 [(? string? str) (list (apply x-constructor next-attrs (list str) x-tail))]
                 [_ (loop elem next-attrs next-key)])))]
           ;; if merged elements are empty (for instance, series of empty strings)
           ;; then zero out the elements in the quad.
           [_ (list (apply x-constructor next-attrs null x-tail))])])))
  #;(trimf atomized-qs (λ (q) (equal? (quad-elems q) '(" "))))
  atomized-qs)

(module+ test
  (define (filter-private-keys qs)
    (for-each (λ (q) (when (hash-has-key? (quad-attrs q) 'run)
                       (hash-remove! (quad-attrs q) 'run))) qs)
    qs)
  (struct $br quad ())
  (define br (q #:type $br (make-hasheq '((br . "time")))))
  (check-equal? (filter-private-keys (atomize (q (q "a b") br (q "x y"))))
                (list (q "a") (q " ") (q "b") br (q "x") (q " ") (q "y")))  
  (check-equal?
   (filter-private-keys (atomize (q (hasheq 'foo 42) (q "Hi" "    idiot" (q (hasheq 'bar 84) "There") "Eve" "ry" "one"))))
   (list (q (hasheq 'foo 42) "Hi")
         (q (hasheq 'foo 42) " ")
         (q (hasheq 'foo 42) "idiot")
         (q (hasheq 'foo 42 'bar 84) "There")
         (q (hasheq 'foo 42) "Everyone")))

  (check-true (andmap quad=?  (atomize (qexpr->quad '(q))) (atomize (qexpr->quad '(q ""))))))
