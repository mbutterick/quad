#lang debug racket
(require racket/list racket/match sugar/debug sugar/list
         "param.rkt" "quad.rkt" "atomize.rkt" "position.rkt" "ocm.rkt")
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
  ;; drop any soft breaks that wouldn't print (e.g., unused soft hyphens)
  (append partial (dropf wrap nonprinting-soft-break-in-middle?)))

(define (default-finish-wrap-func wrap-qs q0 q idx) (list wrap-qs))
(define ((make-finish-wrap finish-wrap-func) qs previous-wrap-ender wrap-idx [wrap-triggering-q (car qs)])
  ;; reverse because quads accumulated in reverse
  ;; wrap-triggering-q is ordinarily the last accumulated q
  ;; unless it's the last wrap, in which case it's #f
  ;; but we capture it separately because it's likely to get trimmed away by `nonprinting-at-end?`
  ;; note: we don't trim `soft-break?` or `hard-break?` because that's an orthogonal consideration
  ;; for instance, a hyphen is `soft-break?` but shouldn't be trimmed.
  (finish-wrap-func (reverse (dropf qs nonprinting-at-end?)) previous-wrap-ender wrap-triggering-q wrap-idx))

(define (make-max-distance-proc max-distance-proc-arg)
  (match max-distance-proc-arg
    [(? procedure? proc) proc]
    [val (λ (q idx) val)]))

(define (finalize-reversed-wraps wraps)
  (match (append* (reverse wraps))
    [(list (list)) (list)]
    [wraps wraps]))

(define ((make-hard-break-pred hard-break-func no-break-func) x)
  (and (hard-break-func x) (or (not no-break-func) (not (no-break-func x)))))

(define ((make-soft-break-pred soft-break-func no-break-func) x)
  (and (soft-break-func x) (or (not no-break-func) (not (no-break-func x)))))

(define (wrap qs
              [max-distance-proc-arg (current-wrap-distance)]
              [debug #f]
              ;; hard break: must wrap
              #:hard-break [hard-break-func (λ (x) #f)]
              ;; soft break: can wrap
              #:soft-break [soft-break-func (λ (x) #f)]
              ;; no break: must not wrap (exception to hard / soft predicates)
              #:no-break [no-break-func #f]
              ;; size of potential wrap.
              ;; simple: measure q and add it to last-dist
              ;; sophisticated: process all wrap-qs and measure resulting 
              #:distance [distance-func (λ (q last-dist wrap-qs)
                                          (+ last-dist (if (printable? q) (distance q) 0)))]
              ;; called when wrap counter increments.
              ;; perhaps should reset after paragraph breaks, etc.
              #:wrap-count [wrap-count (λ (idx wrap-triggering-q) (add1 idx))]
              ;; starting value when wrap counter resets.
              ;; could use an arbitrary data structure (then incremented with `wrap-count`
              #:initial-wrap-count [initial-wrap-idx 1]
              ;; called when wrap is done.
              ;; takes as input list of qs in wrap,
              ;; q0 that caused the previous wrap, or #f at beginning.
              ;; q that caused this one, or #f at end.
              ;; (q0 is not part of this wrap, but q is)
              ;; idx is current wrap-count value.
              #:finish-wrap [finish-wrap-func default-finish-wrap-func])
  (define hard-break? (make-hard-break-pred hard-break-func no-break-func))
  (define soft-break? (make-soft-break-pred soft-break-func no-break-func))
  (define max-distance-proc (make-max-distance-proc max-distance-proc-arg))
  ; takes quads in wrap, triggering quad, and wrap idx; returns list containing wrap (and maybe other things)
  (define finish-wrap (make-finish-wrap finish-wrap-func))
  (let loop ([wraps null] ; list of (list of quads)
             [wrap-idx initial-wrap-idx] ; wrap count (could be (length wraps) but we'd rather avoid `length`)
             [next-wrap-head null] ; list of quads ending in previous `soft-break?` or `hard-break?`
             [next-wrap-tail null] ; list of unbreakable quads
             [current-dist #false] ; #false (to indicate start) or integer
             [previous-wrap-ender #f]
             [qs qs]) ; list of quads
    (match qs
      [(or (== empty) (list (? hard-break?))) ; ignore single trailing hard break
       (define last-wrap (finish-wrap (wrap-append next-wrap-tail next-wrap-head) previous-wrap-ender wrap-idx #f))
       ; append* because `finish-wrap-proc` returns a spliceable list
       ; reverse because wraps accumulated in reverse
       ; as a special case, '(()) is returned as just '()
       (finalize-reversed-wraps (cons last-wrap wraps))]
      [(cons q other-qs)
       (debug-report q 'next-q)
       (debug-report (quad-elems q) 'next-q-elems)
       (define would-be-wrap-qs (wrap-append (cons q next-wrap-tail) next-wrap-head))
       (cond
         [(hard-break? q)
          (debug-report 'found-hard-break)
          ;; put hard break onto next-wrap-tail, and finish the wrap
          (loop (cons (finish-wrap would-be-wrap-qs previous-wrap-ender wrap-idx) wraps)
                (wrap-count wrap-idx q)
                null
                null
                #false
                q
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
                   previous-wrap-ender
                   other-qs)]
            [_ (debug-report 'hard-quad-at-start)
               (loop wraps
                     wrap-idx
                     next-wrap-head
                     (list q)
                     (distance-func q 0 would-be-wrap-qs)
                     previous-wrap-ender
                     other-qs)])]
         [else ; cases that require computing distance
          (define wrap-distance (distance-func q current-dist would-be-wrap-qs))
          (define max-distance (max-distance-proc q wrap-idx))
          (define would-overflow? (> wrap-distance max-distance))
          (cond
            [would-overflow?
             (cond
               [(and (soft-break? q) (nonprinting-at-end? q))
                (debug-report 'would-overflow-soft-nonprinting)
                ;; a break is inevitable but we want to wait to finish the wrap until we see a hard quad
                ;; but we can move the current-partial into the current-wrap
                (loop wraps
                      wrap-idx
                      (wrap-append (cons q next-wrap-tail) next-wrap-head)
                      null
                      wrap-distance
                      previous-wrap-ender
                      other-qs)]
               [(empty? next-wrap-head)
                (define-values (next-wrap-qs other-qs)
                  (cond
                    [(empty? next-wrap-tail)
                     ;; degenerate case where q is big enough to trigger a wrap on its own,
                     ;; but nothing left in next-wrap-head or next-wrap-tail.
                     ;; so we put it in its own wrap and recur, because otherwise we can't proceed
                     ;; though it will look screwy
                     (debug-report 'making-the-best-of-a-bad-situation)
                     (values (list q) (cdr qs))]
                    [else
                     (debug-report 'would-overflow-hard-without-captured-break)
                     (values next-wrap-tail qs)]))
                (loop (cons (finish-wrap next-wrap-qs previous-wrap-ender wrap-idx) wraps)
                      (wrap-count wrap-idx q)
                      null
                      null
                      #false
                      (car next-wrap-qs)
                      other-qs)]
               [else ; finish the wrap & reset the line without consuming a quad
                (loop (cons (finish-wrap next-wrap-head previous-wrap-ender wrap-idx) wraps)
                      (wrap-count wrap-idx q)
                      null
                      next-wrap-tail
                      (for/sum ([item (in-list next-wrap-tail)]) (distance item))
                      (car next-wrap-head)
                      qs)])]         
            [(soft-break? q)
             (debug-report 'would-not-overflow-soft)
             ;; a soft break that fits, so move it on top of the next-wrap-head with the next-wrap-tail
             (loop wraps
                   wrap-idx
                   (wrap-append (cons q next-wrap-tail) next-wrap-head)
                   null
                   wrap-distance
                   previous-wrap-ender
                   other-qs)]
            [else
             (debug-report 'would-not-overflow)
             ;; add to partial
             (loop wraps
                   wrap-idx
                   next-wrap-head
                   (cons q next-wrap-tail)
                   wrap-distance
                   previous-wrap-ender
                   other-qs)])])])))

(define (wrap-best qs
                   [max-distance-proc-arg (current-wrap-distance)]
                   [debug #f]
                   ;; hard break: must wrap
                   #:hard-break [hard-break-func (λ (x) #f)]
                   ;; soft break: can wrap
                   #:soft-break [soft-break-func (λ (x) #f)]
                   ;; no break: must not wrap (exception to hard / soft predicates)
                   #:no-break [no-break-func #f]
                   ;; size of potential wrap.
                   ;; simple: measure q and add it to last-dist
                   ;; sophisticated: process all wrap-qs and measure resulting 
                   #:distance [distance-func (λ (q last-dist wrap-qs)
                                               (+ last-dist (if (printable? q) (distance q) 0)))]
                   ;; called when wrap counter increments.
                   ;; perhaps should reset after paragraph breaks, etc.
                   #:wrap-count [wrap-count (λ (idx q) (add1 idx))]
                   ;; starting value when wrap counter resets.
                   ;; could use an arbitrary data structure (then incremented with `wrap-count`
                   #:initial-wrap-count [initial-wrap-idx 1]
                   ;; called when wrap is done.
                   ;; takes as input list of qs in wrap,
                   ;; q0 that caused the previous wrap, or #f at beginning.
                   ;; q that caused this one, or #f at end.
                   ;; (q0 is not part of this wrap, but q is)
                   ;; idx is current wrap-count value.
                   #:finish-wrap [finish-wrap-func default-finish-wrap-func])
  (define hard-break? (make-hard-break-pred hard-break-func no-break-func))
  (define soft-break? (make-soft-break-pred soft-break-func no-break-func))
  (define max-distance-proc (make-max-distance-proc max-distance-proc-arg))
  (define finish-wrap (make-finish-wrap finish-wrap-func))
  
  (struct $penalty (val idx) #:transparent)
  (define (penalty i j)
    (match-define ($penalty last-val last-idx) (ocm-min-value ocm i))
    (cond
      [(> j (vector-length pieces))
       (define out-of-bounds-signal (- i))
       ($penalty out-of-bounds-signal last-idx)]
      [else
       (define would-be-wrap-qs (pieces-sublist i j))
       (define last-q (vector-ref pieces (sub1 j)))
       (define wrap-idx (wrap-count last-idx last-q))
       (define wrap-distance (for/fold ([last-dist 0])
                                       ([q (in-list would-be-wrap-qs)])
                               (distance-func q last-dist would-be-wrap-qs)))
       (define measure (max-distance-proc (car would-be-wrap-qs) wrap-idx))
       (define underflow (- measure wrap-distance))
       ($penalty
        (+ last-val ; include penalty so far
           (if (negative? underflow)
               ;; overfull line: huge penalty prevents break; multiplier is essential for monotonicity.
               (* 1e8 (- underflow))
               ;; standard penalty
               (expt underflow 2)))
        wrap-idx)]))

  (define ocm (make-ocm penalty ($penalty 0 (sub1 initial-wrap-idx)) $penalty-val))
  ;; starting from last position, ask ocm for position of row minimum (= new-pos)
  ;; collect this value, and use it as the input next time
  ;; until you reach first position.
  (define pieces (list->vector (slicef-after qs (λ (q) (or (hard-break? q) (soft-break? q)) ))))
  (define (pieces-sublist i j)
    ;; nonprinting-soft-break-in-middle? is soft hyphen.
    ;; if a soft hyphen is in the middle of a pieces-sublist, it's superfluous.
    ;; the ones that will end up in the middle are the ones at the end of every piece except the last.
    ;; and the last can drop the nonprinting-at-end?
    (apply append (for*/list ([n (in-range i j)]
                              [pcs (in-value (vector-ref pieces n))])
                    (dropf-right pcs (if (= n j) nonprinting-at-end? nonprinting-soft-break-in-middle?)))))
  (define last-j (vector-length pieces))
  (define bps
    (let loop ([j last-j][bps (list last-j)]) ; start from end
      (define i (ocm-min-index ocm j)) ; look to the previous line
      (if (zero? i) ; zero means we're at first position, and therefore done
          (cons i bps)
          (loop i (cons i bps)))))
  (for/fold ([wraps null]
             [wrap-idx initial-wrap-idx]
             #:result (finalize-reversed-wraps wraps))
            ([i (in-list bps)]
             [j (in-list (cdr bps))])
    (define wrap-qs (reverse (pieces-sublist i j))) ; first-fit gets wrap-qs in reverse, so be consistent
    (define previous-wrap-ender (and (not (zero? i)) (vector-ref pieces (sub1 i))))
    (define wrap-triggering-q (and (not (= j (vector-length pieces))) (car wrap-qs)))
    (values (cons (finish-wrap wrap-qs previous-wrap-ender wrap-idx wrap-triggering-q) wraps)
            (wrap-count wrap-idx (car wrap-qs)))))

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

(define (soft-break? q) (memv (car (quad-elems q)) '(#\space #\-)))

(define (linewrap xs size [debug #f] #:wrapper [wrap-proc wrap])
  (add-between (wrap-proc xs size debug
                          #:finish-wrap (λ (xs . _) (list xs))
                          #:hard-break (λ (q) (char=? (car (quad-elems q)) #\newline))
                          #:soft-break soft-break?) lbr))


(define (visual-wrap str int [debug #f] #:wrapper [wrap-proc wrap])
  (string-join
   (for/list ([x (in-list (linewrap (for/list ([c (in-string str)])
                                      (define atom (q c))
                                      (if (equal? (quad-elems atom) '(#\space))
                                          (struct-copy quad sp)
                                          (struct-copy quad q-one
                                                       [attrs (quad-attrs atom)]
                                                       [elems (quad-elems atom)]))) int debug
                                                                                    #:wrapper wrap-proc))]
              #:when (and (list? x) (andmap quad? x)))
     (list->string (map car (map quad-elems x))))
   "|"))

(define (pagewrap xs size [debug #f])
  (add-between
   (wrap (flatten xs) size debug
         #:hard-break (λ (x) (and (quad? x) (memv (car (quad-elems x)) '(#\page))))
         #:soft-break (λ (x) (and (quad? x) (eq? x lbr)))) pbr))
(define pbr (q #:size #false
               #:printable #false
               #:elems '(#\page)))

(define (linewrap2 xs size [debug #f])
  (add-between
   (wrap xs size debug
         #:hard-break (λ (q) (memv (car (quad-elems q)) '(#\newline)))
         #:soft-break soft-break?
         #:finish-wrap (λ (pcs . _) (list (apply q pcs))))
   lbr))


(module+ test (require rackunit))

(module+ test
  (test-case
   "kp linebreaking"
   (define meg-is-an-ally (list a b c sp a b sp c d sp a b c d x)) ; "Meg is an ally."
   (check-equal? (linewrap meg-is-an-ally 6)
                 ;; Meg is
                 ;; an
                 ;; ally.
                 (list (list a b c sp a b) lbr (list c d) lbr (list a b c d x)))
   (check-equal? (linewrap meg-is-an-ally 6 #:wrapper wrap-best)
                 ;; Meg
                 ;; is an
                 ;; ally.
                 (list (list a b c) lbr (list a b sp c d) lbr (list a b c d x)))))

(module+ test
  (test-begin
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
    (check-equal? (linewrap (list x x x x x) 10) (list (list x x x x x))))

   (test-case
    "chars and spaces"
    (check-equal? (linewrap (list a sp b) 1) (list (list a) lbr (list b)))
    (check-equal? (linewrap (list a b sp c) 2) (list (list a b) lbr (list c)))
    (check-equal? (linewrap (list a sp b) 3) (list (list a sp b)))
    (check-equal? (linewrap (list a sp b c) 3) (list (list a) lbr (list b c))))

   (test-case
    "leading & trailing spaces"
    (check-equal? (linewrap (list sp x) 2) (list (list x)))
    (check-equal? (linewrap (list x sp) 2) (list (list x)))
    (check-equal? (linewrap (list sp x sp) 2) (list (list x)))
    (check-equal? (linewrap (list sp sp x sp sp) 2) (list (list x)))
    (check-equal? (linewrap (list sp sp x sp sp x sp) 1) (list (list x) lbr (list x))))

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
    (check-equal? (linewrap (list x x hyph x x) 5) (list (list x x hyph x x))))

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
    (check-equal? (linewrap (list x x shy x sp x) 4) (list (list x x x) lbr (list x))))

   (test-case
    "zero width nonbreakers"
    (check-equal? (linewrap (list sp zwx) 2) (list (list zwx)))
    (check-equal? (linewrap (list zwx sp) 2) (list (list zwx)))
    (check-equal? (linewrap (list sp zwx sp) 2) (list (list zwx)))
    (check-equal? (linewrap (list sp sp zwx sp sp) 2) (list (list zwx)))
    (check-equal? (linewrap (list sp sp zwx sp sp zwx sp) 2) (list (list zwx sp sp zwx))))

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
    (check-equal? (linewrap (list x x x sp x x) 3) (list (list x x x) lbr (list x x))))


   (test-case
    "hard breaks and spurious spaces"
    (check-equal? (linewrap (list a sp sp sp lbr b) 2) (list (list a) lbr (list b)))
    (check-equal? (linewrap (list a sp lbr sp sp b c sp) 3) (list (list a) lbr (list b c)))
    (check-equal? (linewrap (list sp sp x x sp sp lbr sp sp sp x) 3) (list (list x x) lbr (list x)))
    (check-equal? (linewrap (list a sp b sp sp lbr sp c) 3) (list (list a sp b) lbr (list c)))
    (check-equal? (linewrap (list x x x x) 3) (list (list x x x) lbr (list x)))
    (check-equal? (linewrap (list x x x sp x x) 2) (list (list x x) lbr (list x) lbr (list x x)))
    (check-equal? (linewrap (list x x x sp x x) 3) (list (list x x x) lbr (list x x))))

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
    (check-equal? (visual-wrap "My dog has fleas" 16) "My dog has fleas"))

   (test-case
    "soft page breaks"
    (check-equal? (pagewrap null 2) (list))
    (check-equal? (pagewrap (list x) 2) (list (list x)))
    (check-equal? (pagewrap (list x x) 2) (list (list x x)))
    (check-equal? (pagewrap (list x x x) 1) (list (list x) pbr (list x) pbr (list x)))
    (check-equal? (pagewrap (list x x x) 2) (list (list x x) pbr (list x)))
    (check-equal? (pagewrap (list x x x) 3) (list (list x x x)))
    (check-equal? (pagewrap (list x x x) 4) (list (list x x x)))
    (check-equal? (pagewrap (list x lbr x x) 2) (list (list x) pbr (list x x))))

   (test-case
    "hard page breaks"
    (check-equal? (pagewrap (list a pbr b c) 2) (list (list a) pbr (list b c)))
    (check-equal? (pagewrap (list x pbr x x) 1) (list (list x) pbr (list x) pbr (list x)))
    (check-equal? (pagewrap (list x pbr pbr x x) 1) (list (list x) pbr (list) pbr (list x) pbr (list x)))
    (check-equal? (pagewrap (list x pbr pbr x x) 2) (list (list x) pbr (list) pbr (list x x)))
    (check-equal? (pagewrap (list lbr x lbr lbr pbr lbr x x lbr) 2) (list (list x) pbr (list x x))))

   (test-case
    "composed line breaks and page breaks"
    (check-equal? (pagewrap (linewrap null 1) 2) (list))
    (check-equal? (pagewrap (linewrap (list x) 1) 2) (list (list x)))
    (check-equal? (pagewrap (linewrap (list x x x) 1) 2) (list (list x lbr x) pbr (list x)))
    (check-equal? (pagewrap (linewrap (list x x x) 2) 2) (list (list x x) pbr (list x)))
    (check-equal? (pagewrap (linewrap (list x x x) 2) 1) (list (list x) pbr (list x) pbr (list x))))

   (test-case
    "hard breaks and spurious spaces with slugs"
    (check-equal? (linewrap2 (list a sp sp sp lbr b) 2) (list (q a) lbr (q b)))
    (check-equal? (linewrap2 (list x sp lbr sp sp x x sp) 3) (list (q x) lbr (q x x)))
    (check-equal? (linewrap2 (list sp sp x x sp sp lbr sp sp sp x) 3) (list (q x x) lbr (q x)))
    (check-equal? (linewrap2 (list a sp b sp sp lbr sp c) 3) (list (q a sp b) lbr (q c)))
    (check-equal? (linewrap2 (list x x x x) 3) (list (q x x x) lbr (q x)))
    (check-equal? (linewrap2 (list x x x sp x x) 2) (list (q x x) lbr (q x) lbr (q x x)))
    (check-equal? (linewrap2 (list x x x sp x x) 3) (list (q x x x) lbr (q x x))))))

