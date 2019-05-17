#lang debug racket/base
(require racket/string
         racket/hash
         racket/match
         racket/list
         racket/struct
         txexpr
         sugar/list
         racket/function
         "unicode/emoji.rkt"
         "unicode/math.rkt"
         fontland
         "quad.rkt"
         "qexpr.rkt"
         "param.rkt"
         "util.rkt")
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

(define handle-fallback
  (let ([font-cache (make-hash)]
        [gid-cache (make-hash)])
    (λ (missing-glyph-action str attrs fallback-font-family emoji-font-family math-font-family font-path-resolver)
      (match missing-glyph-action
        ;; #false = no op
        [#false (list (cons attrs str))]
        [action
         (define font-path (hash-ref attrs 'font-path))
         (define f (hash-ref! font-cache font-path (λ () (open-font font-path))))
         (define glyph-ids+chars
           (for/list ([c (in-string str)])
             (define gid
               (hash-ref! gid-cache (cons c font-path)
                          (λ () (glyph-id (vector-ref (glyphrun-glyphs (layout f (string c))) 0)))))
             (define fallback-result (and (zero? gid) (cond
                                                        [(emoji? c) 'emoji]
                                                        [(math? c) 'math]
                                                        [else 'fallback])))
             (cons fallback-result c)))
         (for*/list ([cprs (in-list (contiguous-group-by car glyph-ids+chars eq?))]
                     [fallback-val (in-value (car (car cprs)))]
                     #:unless (and fallback-val (eq? action 'omit)))
           (define str (list->string (map cdr cprs)))
           (define maybe-fallback-attrs
             (cond 
               [(not fallback-val) attrs]
               [(eq? action 'warning) 
                (displayln (format "warning: glyph ~a is not available in font ~a" str (path->string font-path)))
                attrs]
               [(eq? action 'error)
                (raise-argument-error 'quad (format "glyph that exists in font ~a" (path->string font-path)) str)]
               [else (define new-attrs (hash-copy attrs))
                     (hash-set! new-attrs 'font-family (match fallback-val
                                                         ['emoji  emoji-font-family]
                                                         ['math math-font-family]
                                                         [_ fallback-font-family]))
                     (font-path-resolver new-attrs)
                     new-attrs]))
           (cons maybe-fallback-attrs str))]))))


(define (atomize qx #:attrs-proc [attrs-proc values]
                 #:fallback [fallback-font-family #f]
                 #:emoji [emoji-font-family #f]
                 #:math [math-font-family #f]
                 #:font-path-resolver [font-path-resolver values])
  ;; atomize a quad by reducing it to the smallest indivisible formatting units.
  ;; which are multi-character quads with the same formatting.
  (define missing-glyph-action (current-missing-glyph-action))
  
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
               [(? string? str)
                (for/list ([attrstr (in-list
                                     (handle-fallback missing-glyph-action str next-attrs fallback-font-family emoji-font-family math-font-family font-path-resolver))])
                  (match-define (cons attrs str) attrstr)
                  (apply x-constructor attrs (list str) x-tail))]
               [_ (loop elem next-attrs next-key)])))]
         ;; if merged elements are empty (for instance, series of empty strings)
         ;; then zero out the elements in the quad.
         [_ (list (apply x-constructor next-attrs null x-tail))])])))

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
