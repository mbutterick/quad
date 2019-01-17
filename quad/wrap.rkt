#lang debug racket
(require racket/list racket/match sugar/debug 
         "param.rkt" "quad.rkt" "atomize.rkt" "position.rkt")
(provide wrap)

(define-syntax (debug-report stx)
  (syntax-case stx ()
    [(_ EXPR ...) (with-syntax ([debug (datum->syntax stx 'debug)])
                    #'(when debug (report EXPR ...)))]))

(define (nonprinting-at-start? x) (not (printable? x 'start)))
(define (nonprinting-at-end? x) (not (printable? x 'end)))
(define (nonprinting-soft-break-in-middle? x) (and (not (printable? x)) (soft-break? x)))

(define (wrap-append partial wrap)
  ;; pieces will have been accumulated in reverse order
  ;; thus beginning of list represents the end of the wrap
  (append partial (dropf wrap nonprinting-soft-break-in-middle?)))

(define (wrap qs
              [target-size (current-wrap-distance)]
              [debug #f]
              #:hard-break [hard-break? (λ (x) #f)]
              #:soft-break [soft-break? (λ (x) #f)]
              #:wrap-anywhere? [wrap-anywhere? #f]
              #:finish-wrap [finish-wrap-proc (λ (xs q idx) (list xs))])
  ; takes quads in wrap, triggering quad, and wrap idx; returns list containing wrap (and maybe other things)
  (define (finish-wrap qs wrap-idx [wrap-triggering-q (car qs)])
    ;; reverse because quads accumulated in reverse
    ;; wrap-triggering-q is ordinarily the last accumulated q
    ;; unless it's the last wrap, in which case it's #f
    ;; but we capture it separately because it's likely to get trimmed away by `nonprinting-at-end?`
    ;; note: we don't trim `soft-break?` or `hard-break?` because that's an orthogonal consideration
    ;; for instance, a hyphen is `soft-break?` but shouldn't be trimmed.
    (finish-wrap-proc (reverse (dropf qs nonprinting-at-end?)) wrap-triggering-q wrap-idx))
  (let loop ([wraps null] ; list of (list of quads)
             [wrap-idx 1] ; wrap count (could be (length wraps) but we'd rather avoid `length`)
             [next-wrap-head null] ; list of quads ending in previous `soft-break?` or `hard-break?`
             [next-wrap-tail null] ; list of unbreakable quads
             [current-dist #false] ; #false (to indicate start) or integer
             [qs qs]) ; list of quads
    (match qs
      [(or (== empty) (list (? hard-break?))) ; ignore single trailing hard break
       (define last-wrap (finish-wrap (wrap-append next-wrap-tail next-wrap-head) wrap-idx #f))
       ; append* because `finish-wrap-proc` returns a spliceable list
       ; reverse because wraps accumulated in reverse
       ; as a special case, '(()) is returned as just '()
       (match (append* (reverse (cons last-wrap wraps)))
         [(list (list)) (list)]
         [wraps wraps])]
      [(cons q other-qs)
       (debug-report q 'next-q)
       (debug-report (quad-elems q) 'next-q-elems)
       (cond
         [(hard-break? q)
          (debug-report 'found-hard-break)
          ;; put hard break onto next-wrap-tail, and finish the wrap
          (define wrap-qs (wrap-append (cons q next-wrap-tail) next-wrap-head))
          (loop (cons (finish-wrap wrap-qs wrap-idx) wraps)
                (add1 wrap-idx)
                null
                null
                #false
                other-qs)]
         [(let ([at-start? (not current-dist)]) at-start?)
          (match q
            [(and (? soft-break?) (? nonprinting-at-start?))
             (debug-report q 'skipping-soft-break-at-beginning)
             (loop wraps
                   wrap-idx
                   next-wrap-head
                   next-wrap-tail
                   current-dist
                   other-qs)]
            [_ (debug-report 'hard-quad-at-start)
               (loop wraps
                     wrap-idx
                     next-wrap-head
                     (list q)
                     (distance q)
                     other-qs)])]
         [else ; cases that require computing distance
          (define dist (if (printable? q) (distance q) 0))
          (define would-overflow? (and current-dist (> (+ dist current-dist) target-size)))
          (cond
            [would-overflow?
             (cond
               [wrap-anywhere?
                (debug-report 'we-can-wrap-anywhere-so-why-not-here)
                (loop (cons (finish-wrap (wrap-append next-wrap-tail next-wrap-head) wrap-idx) wraps)
                      (add1 wrap-idx)
                      null
                      null
                      #false
                      qs)]
               [(and (soft-break? q) (nonprinting-at-end? q))
                (debug-report 'would-overflow-soft-nonprinting)
                ;; a break is inevitable but we want to wait to finish the wrap until we see a hard quad
                ;; but we can move the current-partial into the current-wrap
                (loop wraps
                      wrap-idx
                      (wrap-append (cons q next-wrap-tail) next-wrap-head)
                      null
                      (+ dist current-dist)
                      other-qs)]
               [(empty? next-wrap-head)
                (debug-report 'would-overflow-hard-without-captured-break)
                (loop (cons (finish-wrap next-wrap-tail wrap-idx) wraps)
                      (add1 wrap-idx)
                      null
                      null
                      #false
                      qs)]
               [else ; finish the wrap & reset the line without consuming a quad
                (loop (cons (finish-wrap next-wrap-head wrap-idx) wraps)
                      (add1 wrap-idx)
                      null
                      next-wrap-tail
                      (apply + (map distance next-wrap-tail))
                      qs)])]         
            [(soft-break? q)
             (debug-report 'would-not-overflow-soft)
             ;; a soft break that fits, so move it on top of the next-wrap-head with the next-wrap-tail
             (loop wraps
                   wrap-idx
                   (wrap-append (cons q next-wrap-tail) next-wrap-head)
                   null
                   (+ dist current-dist)
                   other-qs)]
            [else
             (debug-report 'would-not-overflow)
             ;; add to partial
             (loop wraps
                   wrap-idx
                   next-wrap-head
                   (cons q next-wrap-tail)
                   (+ dist current-dist)
                   other-qs)])])])))

(define q-zero (q #:size (pt 0 0)))

(define q-one (q #:size (pt 1 1) #:printable #t))

(define x (struct-copy quad q-one [elems '(#\x)]))

(define zwx (struct-copy quad q-zero
                         [printable (λ _ #t)]
                         [elems '(#\z)]))

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

(define lbr (struct-copy quad q-one
                         [printable (λ _ #f)]
                         [elems '(#\newline)]))

(define soft-break? (λ (q)  (memv (car (quad-elems q)) '(#\space #\-))))

(define (linewrap xs size [debug #f])
  (add-between (wrap xs size debug
                     #:finish-wrap (λ (xs . _) (list xs))
                     #:hard-break (λ (q) (char=? (car (quad-elems q)) #\newline))
                     #:soft-break soft-break?) lbr))

(module+ test
  (require rackunit))

(module+ test
  (test-case
   "chars"
   (check-equal? (linewrap (list) 1) (list))  
   (check-equal? (linewrap (list a) 1) (list (list a)))
   (check-equal? (linewrap (list a b) 1) (list (list a) lbr (list b)))
   (check-equal? (linewrap (list a b c) 1) (list (list a) lbr (list b) lbr (list c)))
   (check-equal? (linewrap (list a b c) 2) (list (list a b) lbr (list c)))
   (check-equal? (linewrap (list x x x x) 2) (list (list x x) lbr (list x x)))
   (check-equal? (linewrap (list x x x x x) 3) (list (list x x x) lbr (list x x)))
   (check-equal? (linewrap (list x x x x x) 1)
                 (list (list x) lbr (list x) lbr (list x) lbr (list x) lbr (list x)))
   (check-equal? (linewrap (list x x x x x) 10) (list (list x x x x x)))))

(module+ test
  (test-case
   "chars and spaces"
   (check-equal? (linewrap (list a sp b) 1) (list (list a) lbr (list b)))
   (check-equal? (linewrap (list a b sp c) 2) (list (list a b) lbr (list c)))
   (check-equal? (linewrap (list a sp b) 3) (list (list a sp b)))
   (check-equal? (linewrap (list a sp b c) 3) (list (list a) lbr (list b c)))))

(module+ test
  (test-case
   "leading & trailing spaces"
   (check-equal? (linewrap (list sp x) 2) (list (list x)))
   (check-equal? (linewrap (list x sp) 2) (list (list x)))
   (check-equal? (linewrap (list sp x sp) 2) (list (list x)))
   (check-equal? (linewrap (list sp sp x sp sp) 2) (list (list x)))
   (check-equal? (linewrap (list sp sp x sp sp x sp) 1) (list (list x) lbr (list x)))))

(module+ test
  (test-case
   "hard hyphens"
   (check-equal? (linewrap (list hyph) 1) (list (list hyph)))
   (check-equal? (linewrap (list hyph hyph) 1) (list (list hyph) lbr (list hyph)))
   (check-equal? (linewrap (list hyph hyph) 2) (list (list hyph hyph)))
   (check-equal? (linewrap (list hyph hyph hyph) 2) (list (list hyph hyph) lbr (list hyph)))
   (check-equal? (linewrap (list x hyph) 1) (list (list x) lbr (list hyph)))
   (check-equal? (linewrap (list a b hyph c d) 1)
                 (list (list a) lbr (list b) lbr (list hyph) lbr (list c) lbr (list d)))
   (check-equal? (linewrap (list a b hyph c d) 2) (list (list a b) lbr (list hyph c) lbr (list d)))
   (check-equal? (linewrap (list a b hyph c d) 3) (list (list a b hyph) lbr (list c d)))
   (check-equal? (linewrap (list x x hyph x x) 4) (list (list x x hyph) lbr (list x x)))
   (check-equal? (linewrap (list x x hyph x x) 5) (list (list x x hyph x x)))))

(module+ test
  (test-case
   "soft hyphens"
   (check-equal? (linewrap (list shy) 1) (list))
   (check-equal? (linewrap (list shy shy) 2) (list))
   (check-equal? (linewrap (list shy shy shy) 2) (list))
   (check-equal? (linewrap (list x shy) 1) (list (list x)))
   (check-equal? (linewrap (list x shy shy shy shy) 1) (list (list x)))
   ;; todo: degenerate cases that don't work without continuations
   ;(check-equal? (linewrap (list x x shy x x) 1) (list x br x br x br x))
   ;(check-equal? (linewrap (list x x shy x x) 2) (list x x br x x))
   (check-equal? (linewrap (list x x shy x x) 3) (list (list x x shy) lbr (list x x)))
   (check-equal? (linewrap (list x x shy x x) 4) (list (list x x x x)))
   (check-equal? (linewrap (list x x shy x x) 5) (list (list x x x x)))
   (check-equal? (linewrap (list x x shy x sp x) 4) (list (list x x x) lbr (list x)))))

(module+ test
  (test-case
   "zero width nonbreakers"
   (check-equal? (linewrap (list sp zwx) 2) (list (list zwx)))
   (check-equal? (linewrap (list zwx sp) 2) (list (list zwx)))
   (check-equal? (linewrap (list sp zwx sp) 2) (list (list zwx)))
   (check-equal? (linewrap (list sp sp zwx sp sp) 2) (list (list zwx)))
   (check-equal? (linewrap (list sp sp zwx sp sp zwx sp) 2) (list (list zwx sp sp zwx)))))

(module+ test
  (test-case
   "hard breaks"
   (check-equal? (linewrap (list lbr) 2) (list)) ;; only insert a break if it's between things
   (check-equal? (linewrap (list a lbr b) 2) (list (list a) lbr (list b)))
   (check-equal? (linewrap (list a b lbr) 2) (list (list a b)))
   (check-equal? (linewrap (list a b lbr lbr) 2) (list (list a b) lbr (list)))
   (check-equal? (linewrap (list x lbr x x) 3) (list (list x) lbr (list x x)))
   (check-equal? (linewrap (list x x lbr x) 3) (list (list x x) lbr (list x)))
   (check-equal? (linewrap (list x x x x) 3) (list (list x x x) lbr (list x)))
   (check-equal? (linewrap (list x x x sp x x) 2) (list (list x x) lbr (list x) lbr (list x x)))
   (check-equal? (linewrap (list x x x sp x x) 3) (list (list x x x) lbr (list x x)))))

(module+ test
  (test-case
   "hard breaks and spurious spaces"
   (check-equal? (linewrap (list a sp sp sp lbr b) 2) (list (list a) lbr (list b)))
   (check-equal? (linewrap (list a sp lbr sp sp b c sp) 3) (list (list a) lbr (list b c)))
   (check-equal? (linewrap (list sp sp x x sp sp lbr sp sp sp x) 3) (list (list x x) lbr (list x)))
   (check-equal? (linewrap (list a sp b sp sp lbr sp c) 3) (list (list a sp b) lbr (list c)))
   (check-equal? (linewrap (list x x x x) 3) (list (list x x x) lbr (list x)))
   (check-equal? (linewrap (list x x x sp x x) 2) (list (list x x) lbr (list x) lbr (list x x)))
   (check-equal? (linewrap (list x x x sp x x) 3) (list (list x x x) lbr (list x x)))))

(define (visual-wrap str int [debug #f])
  (string-join
   (for/list ([x (in-list (linewrap (for/list ([c (in-string str)])
                                              (define atom (q c))
                                              (if (equal? (quad-elems atom) '(#\space))
                                                  (struct-copy quad sp)
                                                  (struct-copy quad q-one
                                                               [attrs (quad-attrs atom)]
                                                               [elems (quad-elems atom)]))) int debug))]
              #:when (and (list? x) (andmap quad? x)))
             (list->string (map car (map quad-elems x))))
   "|"))

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
  (add-between
   (wrap (flatten xs) size debug
         #:hard-break (λ (x) (and (quad? x) (memv (car (quad-elems x)) '(#\page))))
         #:soft-break (λ (x) (and (quad? x) (eq? x lbr)))) pbr))
(define pbr (q #:size #false
               #:printable #false
               #:elems '(#\page)))

(module+ test
  (require rackunit)
  (test-case
   "soft page breaks"
   (check-equal? (pagewrap null 2) (list))
   (check-equal? (pagewrap (list x) 2) (list (list x)))
   (check-equal? (pagewrap (list x x) 2) (list (list x x)))
   (check-equal? (pagewrap (list x x x) 1) (list (list x) pbr (list x) pbr (list x)))
   (check-equal? (pagewrap (list x x x) 2) (list (list x x) pbr (list x)))
   (check-equal? (pagewrap (list x x x) 3) (list (list x x x)))
   (check-equal? (pagewrap (list x x x) 4) (list (list x x x)))
   (check-equal? (pagewrap (list x lbr x x) 2) (list (list x) pbr (list x x)))))

(module+ test
  (test-case
   "hard page breaks"
   (check-equal? (pagewrap (list a pbr b c) 2) (list (list a) pbr (list b c)))
   (check-equal? (pagewrap (list x pbr x x) 1) (list (list x) pbr (list x) pbr (list x)))
   (check-equal? (pagewrap (list x pbr pbr x x) 1) (list (list x) pbr (list) pbr (list x) pbr (list x)))
   (check-equal? (pagewrap (list x pbr pbr x x) 2) (list (list x) pbr (list) pbr (list x x)))
   (check-equal? (pagewrap (list lbr x lbr lbr pbr lbr x x lbr) 2) (list (list x) pbr (list x x)))))

(module+ test
  (test-case
   "composed line breaks and page breaks"
   (check-equal? (pagewrap (linewrap null 1) 2) (list))
   (check-equal? (pagewrap (linewrap (list x) 1) 2) (list (list x)))
   (check-equal? (pagewrap (linewrap (list x x x) 1) 2) (list (list x lbr x) pbr (list x)))
   (check-equal? (pagewrap (linewrap (list x x x) 2) 2) (list (list x x) pbr (list x)))
   (check-equal? (pagewrap (linewrap (list x x x) 2) 1) (list (list x) pbr (list x) pbr (list x)))))

(define (linewrap2 xs size [debug #f])
  (add-between
   (wrap xs size debug
         #:hard-break (λ (q) (memv (car (quad-elems q)) '(#\newline)))
         #:soft-break soft-break?
         #:finish-wrap (λ (pcs . _) (list (apply q pcs))))
   lbr))

(module+ test
  (test-case
   "hard breaks and spurious spaces with slugs"
   (check-equal? (linewrap2 (list a sp sp sp lbr b) 2) (list (q a) lbr (q b)))
   (check-equal? (linewrap2 (list x sp lbr sp sp x x sp) 3) (list (q x) lbr (q x x)))
   (check-equal? (linewrap2 (list sp sp x x sp sp lbr sp sp sp x) 3) (list (q x x) lbr (q x)))
   (check-equal? (linewrap2 (list a sp b sp sp lbr sp c) 3) (list (q a sp b) lbr (q c)))
   (check-equal? (linewrap2 (list x x x x) 3) (list (q x x x) lbr (q x)))
   (check-equal? (linewrap2 (list x x x sp x x) 2) (list (q x x) lbr (q x) lbr (q x x)))
   (check-equal? (linewrap2 (list x x x sp x x) 3) (list (q x x x) lbr (q x x)))))

(module+ test
  (test-case
   "wrap anywhere behavior"
   (struct sp quad ())
   (define (qsoft)
     (q #:type sp
        #:printable (λ (q sig) (not (memq sig '(start end))))
        #:size (pt 1 1)))
   (define (qhard) (q #:attrs (hasheq 'q 1) #:size (pt 1 1)))
   (define qs (list (qhard) (qsoft) (qhard) (qhard)))
   ;; only wraps on soft break, so two qhards go in second wrap
   (check-equal? (wrap qs 3 #:soft-break sp?) (list (list (qhard)) (list (qhard) (qhard))))
   ;; wraps anywhere, so two qhards fit onto first wrap with space
   (check-equal? (wrap qs 3 #:soft-break sp? #:wrap-anywhere? #t) (list (list (qhard) (qsoft) (qhard)) (list (qhard))))))

