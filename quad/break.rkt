#lang debug racket
(require racket/list racket/match sugar/debug 
         "param.rkt" "quad.rkt" "atomize.rkt" "position.rkt")
(provide break)

(define-syntax (debug-report stx)
  (syntax-case stx ()
    [(_ EXPR ...) (with-syntax ([debug (datum->syntax stx 'debug)])
                    #'(when debug (report EXPR ...)))]))

(define (break xs
               [target-size (current-wrap-distance)]
               [debug #f]
               #:hard-break-proc [hard-break? (λ (x) #f)]
               #:soft-break-proc [soft-break? (λ (x) #f)]
               #:finish-wrap-proc [finish-wrap-proc list])
  #;((listof quad?)
     (real?
      any/c
      #:hard-break-proc (any/c . -> . any/c)
      #:soft-break-proc (any/c . -> . any/c)
      #:finish-wrap-proc ((listof any/c) . -> . (listof any/c))) . ->* . (listof any/c))
  (break-hards xs target-size debug hard-break? soft-break? finish-wrap-proc))

;; the hard breaks are used to divide the wrap territory into smaller chunks
;; that can be cached, parallelized, etc.
(define (break-hards qs target-size debug hard-break? soft-break? finish-wrap-proc)
  (let loop ([wraps null][qs qs])
    (match qs
      [(? null?) (append* (reverse wraps))]
      [(or (cons (? hard-break?) rest) rest)
       (define-values (head tail) (splitf-at rest (λ (x) (not (hard-break? x)))))
       ;; head will be empty (intentionally) if qs starts with two hard breaks
       (define next-wrap (break-softs head target-size debug soft-break? finish-wrap-proc))
       (loop (cons next-wrap wraps) tail)])))

(define (nonprinting-at-start? x)
  (not (printable? x 'start)))
(define (nonprinting-at-end? x)
  (not (printable? x 'end)))
(define (nonprinting-in-middle-soft-break? x)
  (and (not (printable? x)) (soft-break? x)))

(define (wrap-append partial wrap)
  ;; pieces will have been accumulated in reverse order
  ;; thus beginning of list represents the end of the wrap
  (append partial (dropf wrap nonprinting-in-middle-soft-break?)))

(define (break-softs qs
                     target-size
                     debug
                     soft-break?
                     finish-wrap-proc)
  (let loop ([wraps null] ; list of (list of quads)
             [next-wrap-head null] ; list of quads ending in previous `soft-break?`
             [next-wrap-tail null] ; list of unbreakable quads
             [current-dist #false] ; #false (to indicate start) or integer
             [qs qs]) ; list of quads
    (match qs
      [(== empty) (define last-wrap (wrap-append next-wrap-tail next-wrap-head))
                  (append* (reverse
                            (for/list ([wrap-qs (in-list (cons last-wrap wraps))])
                              (finish-wrap-proc (reverse (dropf wrap-qs nonprinting-at-end?))))))]
      [(cons q other-qs)
       (debug-report q 'next-q)
       (debug-report (quad-elems q) 'next-q-elems)
       (define at-start? (not current-dist))
       (define dist (if (printable? q) (distance q) 0))
       (define would-overflow? (and current-dist (> (+ dist current-dist) target-size)))
       (cond
         [at-start?
          (match q
            [(and (? soft-break?) (? nonprinting-at-start?))
             (debug-report q 'skipping-soft-break-at-beginning)
             (loop wraps
                   next-wrap-head
                   next-wrap-tail
                   current-dist
                   other-qs)]
            [_ (debug-report 'hard-quad-at-start)
               (loop wraps
                     next-wrap-head
                     (list q)
                     (distance q)
                     other-qs)])]
         [would-overflow?
          (match q
            [(and (? soft-break?) (? nonprinting-at-end?))
             (debug-report 'would-overflow-soft-nonprinting)
             ;; a break is inevitable but we want to wait to finish the wrap until we see a hard quad
             ;; but we can move the current-partial into the current-wrap
             (loop wraps
                   (wrap-append (cons q next-wrap-tail) next-wrap-head)
                   null
                   (+ dist current-dist)
                   other-qs)]
            [_ #:when (empty? next-wrap-head)
               (debug-report 'would-overflow-hard-without-captured-break)
               (loop (cons next-wrap-tail wraps)
                     null
                     null
                     #false
                     qs)]
            [_ ; finish the wrap & reset the line without consuming a quad
             (loop (cons next-wrap-head wraps)
                   null
                   next-wrap-tail
                   (apply + (map distance next-wrap-tail))
                   qs)])]         
         [(soft-break? q) ; printing soft break, like a hyphen
          (debug-report 'would-not-overflow-soft)
          ;; a soft break that fits, so move it on top of the next-wrap-head with the next-wrap-tail
          (loop wraps
                (wrap-append (cons q next-wrap-tail) next-wrap-head)
                null
                (+ dist current-dist)
                other-qs)]
         [else
          (debug-report 'would-not-overflow)
          ;; add to partial
          (loop wraps
                next-wrap-head
                (cons q next-wrap-tail)
                (+ dist current-dist)
                other-qs)])])))


(define q-zero (q #:size (pt 0 0)))
(define q-one (q #:size (pt 1 1) #:printable #t))

(define x (struct-copy quad q-one [elems '(#\x)]))
(define zwx (struct-copy quad q-zero [elems '(#\z)]))
(define hyph (struct-copy quad q-one [elems '(#\-)]))
(define shy (struct-copy quad q-one
                         [printable (λ (q [sig #f])
                                      (case sig
                                        [(end) #t]
                                        [else #f]))]
                         [elems '(#\-)]))
(define a (struct-copy quad q-one [elems '(#\a)]))
(define b (struct-copy quad q-one [elems '(#\b)]))
(define c (struct-copy quad q-one [elems '(#\c)]))
(define d (struct-copy quad q-one [elems '(#\d)]))
(define sp (struct-copy quad q-one
                        [printable (λ (q [sig #f])
                                     (case sig
                                       [(start end) #f]
                                       [else #t]))]
                        [elems '(#\space)]))
(define br (struct-copy quad q-one
                        [printable (λ (q [sig #f]) #f)]
                        [elems '(#\newline)]))
(define soft-break? (λ (q)  (memv (car (quad-elems q)) '(#\space #\-))))

(define (linewrap xs size [debug #f])
  (break xs size debug
         #:finish-wrap-proc (λ (xs) (list (length xs)))
         #:hard-break-proc (λ (q) (char=? (car (quad-elems q)) #\newline))
         #:soft-break-proc soft-break?))

(module+ test
  (require rackunit))

(module+ test
  (require rackunit)
  (test-case
   "chars"
   (check-equal? (linewrap (list) 1) null)  
   (check-equal? (linewrap (list a) 1) '(1))
   (check-equal? (linewrap (list a b) 1) '(1 1))
   (check-equal? (linewrap (list a b c) 1) '(1 1 1))
   (check-equal? (linewrap (list a b c) 2) '(2 1))
   (check-equal? (linewrap (list x x x x) 2) '(2 2))
   (check-equal? (linewrap (list x x x x x) 3) '(3 2))
   (check-equal? (linewrap (list x x x x x) 1) '(1 1 1 1 1))
   (check-equal? (linewrap (list x x x x x) 10)  '(5))))

(module+ test
  (test-case
   "chars and spaces"
   (check-equal? (linewrap (list a sp b) 1) '(1 1))
   (check-equal? (linewrap (list a b sp c) 2) '(2 1))
   (check-equal? (linewrap (list a sp b) 3) '(3))
   (check-equal? (linewrap (list a sp b c) 3) '(1 2))))

(module+ test
  (test-case
   "leading & trailing spaces"
   (check-equal? (linewrap (list sp x) 2) '(1))
   (check-equal? (linewrap (list x sp) 2) '(1))
   (check-equal? (linewrap (list sp x sp) 2) '(1))
   (check-equal? (linewrap (list sp sp x sp sp) 2) '(1))
   (check-equal? (linewrap (list sp sp x sp sp x sp) 1) '(1 1))))

(module+ test
  (test-case
   "hard hyphens"
   (check-equal? (linewrap (list hyph) 1) '(1))
   (check-equal? (linewrap (list hyph hyph) 1) '(1 1))
   (check-equal? (linewrap (list hyph hyph) 2) '(2))
   (check-equal? (linewrap (list hyph hyph hyph) 2) '(2 1))
   (check-equal? (linewrap (list x hyph) 1) '(1 1))
   (check-equal? (linewrap (list a b hyph c d) 1) '(1 1 1 1 1))
   (check-equal? (linewrap (list a b hyph c d) 2) '(2 2 1))
   (check-equal? (linewrap (list a b hyph c d) 3) '(3 2))
   (check-equal? (linewrap (list x x hyph x x) 4) '(3 2))
   (check-equal? (linewrap (list x x hyph x x) 5) '(5))))

(module+ test
  (test-case
   "soft hyphens"
   (check-equal? (linewrap (list shy) 1) '(0))
   (check-equal? (linewrap (list shy shy) 2) '(0))
   (check-equal? (linewrap (list shy shy shy) 2) '(0))
   (check-equal? (linewrap (list x shy) 1) '(1))
   (check-equal? (linewrap (list x shy shy shy shy) 1) '(1))
   ;; todo: degenerate cases that don't work without continuations
   ;(check-equal? (linewrap (list x x shy x x) 1) (list x 'lb x 'lb x 'lb x))
   ;(check-equal? (linewrap (list x x shy x x) 2) (list x x 'lb x x))
   (check-equal? (linewrap (list x x shy x x) 3) '(3 2))
   (check-equal? (linewrap (list x x shy x x) 4) '(4))
   (check-equal? (linewrap (list x x shy x x) 5) '(4))
   (check-equal? (linewrap (list x x shy x sp x) 4) '(3 1))))

#|
(module+ test
  (test-case
   "zero width nonbreakers"
   ;; todo: fix
   (check-equal? (linewrap (list sp zwx) 2) '(1))
   (check-equal? (linewrap (list zwx sp) 2) '(1))
   (check-equal? (linewrap (list sp zwx sp) 2) '(1))
   (check-equal? (linewrap (list sp sp zwx sp sp) 2) '(1))
   (check-equal? (linewrap (list sp sp zwx sp sp zwx sp) 2) '(4))))


(module+ test
  (test-case
   "hard breaks"
   (check-equal? (linewrap (list br) 2) (list)) ;; only insert a break if it's between things
   (check-equal? (linewrap (list a br b) 2) (list (list a) (list b)))
   (check-equal? (linewrap (list a b br) 2) (list (list a b)))
   (check-equal? (linewrap (list a b br br) 2) (list (list a b)))
   (check-equal? (linewrap (list x br x x) 3) (list (list x) (list x x)))
   (check-equal? (linewrap (list x x br x) 3) (list (list x x) (list x)))
   (check-equal? (linewrap (list x x x x) 3) (list (list x x x) (list x)))
   (check-equal? (linewrap (list x x x sp x x) 2) (list (list x x) (list x) (list x x)))
   (check-equal? (linewrap (list x x x sp x x) 3) (list (list x x x) (list x x)))))

(module+ test
  (test-case
   "hard breaks and spurious spaces"
   (check-equal? (linewrap (list a sp sp sp br b) 2) (list (list a) (list b)))
   (check-equal? (linewrap (list x sp br sp sp x x sp) 3) (list (list x) (list x x)))
   (check-equal? (linewrap (list sp sp x x sp sp br sp sp sp x) 3) (list (list x x) (list x)))
   (check-equal? (linewrap (list a sp b sp sp br sp c) 3) (list (list a sp b) (list c)))
   (check-equal? (linewrap (list x x x x) 3) (list (list x x x) (list x)))
   (check-equal? (linewrap (list x x x sp x x) 2) (list (list x x) (list x) (list x x)))
   (check-equal? (linewrap (list x x x sp x x) 3) (list (list x x x) (list x x)))))

(define (visual-wrap str int [debug #f])
  (string-join
   (for/list ([qs (in-list (linewrap (for/list ([atom (atomize str)])
                                       (if (equal? (quad-elems atom) '(#\space))
                                           (struct-copy quad sp)
                                           (struct-copy quad q-one
                                                        [attrs (quad-attrs atom)]
                                                        [elems (quad-elems atom)]))) int debug))])
     (list->string (map (λ (q) (car (quad-elems q))) qs))) "|"))

(module+ test
  (test-case
   "visual breaks"
   (check-equal? (visual-wrap "My dog has fleas" 1) "M|y|d|o|g|h|a|s|f|l|e|a|s")
   (check-equal? (visual-wrap "My dog has fleas" 2) "My|do|g|ha|s|fl|ea|s")
   (check-equal? (visual-wrap "My dog has fleas" 3) "My|dog|has|fle|as")
   (check-equal? (visual-wrap "My dog has fleas" 4) "My|dog|has|flea|s")
   (check-equal? (visual-wrap "My dog has fleas" 5) "My|dog|has|fleas")
   (check-equal? (visual-wrap "My dog has fleas" 6) "My dog|has|fleas")
   (check-equal? (visual-wrap "My dog has fleas" 7) "My dog|has|fleas")
   (check-equal? (visual-wrap "My dog has fleas" 8) "My dog|has|fleas")
   (check-equal? (visual-wrap "My dog has fleas" 9) "My dog|has fleas")
   (check-equal? (visual-wrap "My dog has fleas" 10) "My dog has|fleas")
   (check-equal? (visual-wrap "My dog has fleas" 11) "My dog has|fleas")
   (check-equal? (visual-wrap "My dog has fleas" 12) "My dog has|fleas")
   (check-equal? (visual-wrap "My dog has fleas" 13) "My dog has|fleas")
   (check-equal? (visual-wrap "My dog has fleas" 14) "My dog has|fleas")
   (check-equal? (visual-wrap "My dog has fleas" 15) "My dog has|fleas")
   (check-equal? (visual-wrap "My dog has fleas" 16) "My dog has fleas")))

(define (pagewrap xs size [debug #f])
  (break xs size debug
         #:hard-break-proc (λ (x) (and (quad? x) (memv (car (quad-elems x)) '(#\page))))
         #:soft-break-proc (λ (x) (eq? x 'lb))))
(define pbr (q #:size #false #:elems '(#\page)))

(module+ test
  (require rackunit)
  (test-case
   "soft page breaks"
   (check-equal? (pagewrap null 2) (list))
   (check-equal? (pagewrap (list x) 2) (list (list x)))
   (check-equal? (pagewrap (list x x) 2) (list (list x x)))
   (check-equal? (pagewrap (list x x x) 1) (list (list x) (list x) (list x)))
   (check-equal? (pagewrap (list x x x) 2) (list (list x x) (list x)))
   (check-equal? (pagewrap (list x x x) 3) (list (list x x x)))
   (check-equal? (pagewrap (list x x x) 4) (list (list x x x)))
   (check-equal? (pagewrap (list x 'lb x x) 2) (list (list x) (list x x)))))

(module+ test
  (test-case
   "hard page breaks"
   (check-equal? (pagewrap (list x pbr x x) 2) (list (list x) (list x x)))
   (check-equal? (pagewrap (list x pbr x x) 1) (list (list x) (list x) (list x)))
   ; todo: fix double breaks
   #;(check-equal? (pagewrap (list x pbr pbr x x) 1) (list (list x) (list) (list x) (list x)))
   #;(check-equal? (pagewrap (list x pbr pbr x x) 2) (list (list x) (list) (list x x)))
   (check-equal? (pagewrap (list 'lb x 'lb 'lb pbr 'lb x x 'lb) 2) (list (list x) (list x x)))))


(module+ test
  (test-case
   "composed line breaks and page breaks"
   ; todo: fix empty test
   #;(check-equal? (pagewrap (linewrap null 1) 2) (list (list (list))))
   (check-equal? (pagewrap (linewrap (list x) 1) 2) (list (list (list x))))
   (check-equal? (pagewrap (linewrap (list x x x) 1) 2) (list (list (list x) (list x)) (list (list x))))
   (check-equal? (pagewrap (linewrap (list x x x) 2) 2) (list (list (list x x)) (list (list x))))
   (check-equal? (pagewrap (linewrap (list x x x) 2) 1) (list (list (list x)) (list (list x)) (list (list x))))))


(define (slug . xs) (q #:attrs (hasheq) #:elems xs))
(define (linewrap2 xs size [debug #f])
  (break xs size debug
         #:break-val 'lb
         #:hard-break-proc (λ (q) (and (quad? q) (memv (car (quad-elems q)) '(#\newline))))
         #:soft-break-proc soft-break?
         #:finish-wrap-proc (λ (pcs) (list (apply slug pcs)))))

(module+ test
  (test-case
   "hard breaks and spurious spaces with slugs"
   (check-equal? (linewrap2 (list a sp sp sp br b) 2) (list (slug a) 'lb (slug b)))
   (check-equal? (linewrap2 (list x sp br sp sp x x sp) 3) (list (slug x) 'lb (slug x x)))
   (check-equal? (linewrap2 (list sp sp x x sp sp br sp sp sp x) 3) (list (slug x x) 'lb (slug x)))
   (check-equal? (linewrap2 (list a sp b sp sp br sp c) 3) (list (slug a sp b) 'lb (slug c)))
   (check-equal? (linewrap2 (list x x x x) 3) (list (slug x x x) 'lb (slug x)))
   (check-equal? (linewrap2 (list x x x sp x x) 2) (list (slug x x) 'lb (slug x) 'lb (slug x x)))
   (check-equal? (linewrap2 (list x x x sp x x) 3) (list (slug x x x) 'lb (slug x x)))))

|#