#lang debug racket/base
(require racket/contract racket/list racket/match txexpr sugar/debug sugar/define sugar/list racket/promise racket/function (only-in racket/control call/prompt)
         "param.rkt" "qexpr.rkt" "atomize.rkt" "quad.rkt" "generic.rkt" "position.rkt")

(define/contract (distance q [signal #f])
  ((any/c) (any/c) . ->* . real?)
  (cond
    [(quad? q)
     (match-define (list ∆x ∆y) (map - (out-point q signal) (in-point q signal)))
     (cond
       [(zero? ∆x) ∆y]
       [(zero? ∆y) ∆x]
       [else (sqrt (+ (* ∆x ∆x) (* ∆y ∆y)))])]
    [else 0]))


(define+provide/contract (wrap xs
                               [target-size (current-wrap-distance)]
                               [debug #f]
                               #:break-val [break-val 'break]
                               #:mandatory-break-proc [mandatory-break? (const #f)]
                               #:optional-break-proc [optional-break? (const #f)]
                               #:finish-wrap-proc [finish-wrap-proc values])
  ((any/c) (real? any/c
                  #:break-val any/c
                  #:mandatory-break-proc procedure?
                  #:optional-break-proc procedure?
                  #:finish-wrap-proc procedure?) . ->* . (listof any/c))
  (define start-signal (gensym))
  (define (nonprinting-at-end? x) (zero? (distance x 'end)))
  (define (finish-wrap pieces) (finish-wrap-proc (reverse (dropf pieces (λ (x) (and (optional-break? x) (nonprinting-at-end? x)))))))
  (define last-optional-break-k #f)
  (call/prompt ;; continuation boundary for last-optional-break-k
   (thunk
    (define (capture-optional-break-k!) (let/cc k (set! last-optional-break-k k) #f))
    (for/fold ([segments null]
               [pieces null]
               [dist-so-far start-signal]
               #:result (append* (reverse (cons (finish-wrap pieces) segments))))
              ([x (in-list xs)])
      (define at-start? (eq? dist-so-far start-signal))
      (define underflow? (and (not at-start?) (<= (+ dist-so-far (distance x 'end)) target-size)))
      (define (add-to-segment) (values segments (cons x pieces) (if at-start?
                                                                    (distance x 'start)
                                                                    (+ dist-so-far (distance x)))))
      (define (insert-break [before? #f])
        ;; a break can be inserted before or after the current quad.
        ;; At an ordinary break (mandatory or optional) it goes after the wrap point.
        ;; The wrap signal consumes the break if it's nonprinting (e.g., word space or hard break)
        ;; but not if it's printing (e.g., hyphen).
        ;; But if no ordinary break can be found for a line, the wrap will happen before the quad.
        ;; The wrap signal will not consume the quad (rather, it will become the first quad in the next wrap)
        ;; In both cases, the `finish-wrap` proc will strip off any trailing white breaks from the new segment.
        (define-values (pieces-for-this-wrap pieces-for-next-wrap starting-distance-for-next-wrap)
          (if before?
              (values pieces (list x) (distance x 'start))
              (values (if (nonprinting-at-end? x) pieces (cons x pieces)) null start-signal)))
        (values (list* (list break-val) (finish-wrap pieces-for-this-wrap) segments)
                pieces-for-next-wrap
                starting-distance-for-next-wrap))
      (cond
        [(mandatory-break? x) (when debug (report x 'got-mandatory-break))
                              (insert-break)]
        [(optional-break? x)
         (cond
           [at-start? (when debug (report x 'skipping-opt-break-at-beginning)) (values segments null dist-so-far)]
           [(and underflow? (capture-optional-break-k!)) (when debug (report x 'resuming-breakpoint))
                                                         (set! last-optional-break-k #f) ;; prevents continuation loop
                                                         (insert-break)]
           [else (when debug (report x 'add-optional-break))
                 (add-to-segment)])]
        [(or at-start? underflow?) (when debug (report x 'add-ordinary-char))
                                   (add-to-segment)]
        ;; overflow handlers
        [last-optional-break-k (when debug (report x 'invoking-last-breakpoint))
                               (last-optional-break-k #t)]
        ;; fallback if no last-breakpoint-k exists
        [else (when debug (report x 'falling-back))
              (insert-break 'before)])))))


(define x (q #f #\x))
(define zwx (q (list 'size (pt 0 0)) #\z))
(define hyph (q #f #\-))
(define a (q #f #\a))
(define b (q #f #\b))
(define c (q #f #\c))
(define d (q #f #\d))
(define sp (q (list 'size (λ (sig)
                            (case sig
                              [(start end) (pt 0 0)]
                              [else (pt 1 1)]))) #\space))
(define br (q (list 'size (pt 0 0)) #\newline))
(define optional-break? (λ (q) (and (quad? q) (memv (car (elems q)) '(#\space #\-)))))

(define (linewrap xs size [debug #f])
  (wrap xs size debug
        #:break-val 'lb
        #:mandatory-break-proc (λ (q) (and (quad? q) (memv (car (elems q)) '(#\newline))))
        #:optional-break-proc optional-break?))

(module+ test
  (require rackunit)

  (test-case
   "chars"
   (check-equal? (linewrap (list) 1) null)  
   (check-equal? (linewrap (list x) 1) (list x))
   (check-equal? (linewrap (list x x) 1) (list x 'lb x))
   (check-equal? (linewrap (list x x x) 1) (list x 'lb x 'lb x))
   (check-equal? (linewrap (list x x x) 2) (list x x 'lb x))
   (check-equal? (linewrap (list x x x x) 2) (list x x 'lb x x))
   (check-equal? (linewrap (list x x x x x) 3) (list x x x 'lb x x))
   (check-equal? (linewrap (list x x x x x) 1) (list x 'lb x 'lb x 'lb x 'lb x))
   (check-equal? (linewrap (list x x x x x) 10) (list x x x x x)))

  (test-case
   "chars and spaces"
   (check-equal? (linewrap (list x sp x) 1) (list x 'lb x))
   (check-equal? (linewrap (list x x sp x) 2) (list x x 'lb x))
   (check-equal? (linewrap (list a sp b) 3) (list a sp b))
   (check-equal? (linewrap (list x sp x x) 3) (list x 'lb x x)))


  (test-case
   "leading & trailing spaces"
   (check-equal? (linewrap (list sp x) 2) (list x))
   (check-equal? (linewrap (list x sp) 2) (list x))
   (check-equal? (linewrap (list sp x sp) 2) (list x))
   (check-equal? (linewrap (list sp sp x sp sp) 2) (list x))
   (check-equal? (linewrap (list sp sp x sp sp x sp) 1) (list x 'lb x)))

  
  (test-case
   "hyphens, hard and soft"
   (check-equal? (linewrap (list x hyph x) 1 'debug) (list x 'lb hyph 'lb x))
   #;(check-equal? (linewrap (list x x hyph x x) 1 'debug) (list x 'lb x 'lb hyph 'lb x 'lb x))
   #;(check-equal? (linewrap (list x x hyph x x) 2) (list x x 'lb hyph x 'lb x x))
   (check-equal? (linewrap (list x x hyph x x) 3) (list x x hyph 'lb x x))
   (check-equal? (linewrap (list x x hyph x x) 4) (list x x hyph 'lb x x))
   (check-equal? (linewrap (list x x hyph x x) 5) (list x x hyph x x)))

  (test-case
   "zero width nonbreakers"
   (check-equal? (linewrap (list sp zwx) 2) (list zwx))
   (check-equal? (linewrap (list zwx sp) 2) (list zwx))
   (check-equal? (linewrap (list sp zwx sp) 2) (list zwx))
   (check-equal? (linewrap (list sp sp zwx sp sp) 2) (list zwx))
   (check-equal? (linewrap (list sp sp zwx sp sp zwx sp) 2) (list zwx sp sp zwx)))

  (test-case
   "mandatory breaks"
   (check-equal? (linewrap (list br) 2) (list 'lb))
   (check-equal? (linewrap (list a br b) 2) (list a 'lb b))
   (check-equal? (linewrap (list a b br) 2) (list a b 'lb))
   (check-equal? (linewrap (list a b br br) 2) (list a b 'lb 'lb))
   (check-equal? (linewrap (list x br x x) 3) (list x 'lb x x))
   (check-equal? (linewrap (list x x br x) 3) (list x x 'lb x))
   (check-equal? (linewrap (list x x x x) 3) (list x x x 'lb x))
   (check-equal? (linewrap (list x x x sp x x) 2) (list x x 'lb x 'lb x x))
   (check-equal? (linewrap (list x x x sp x x) 3) (list x x x 'lb x x)))

  (test-case
   "mandatory breaks and spurious spaces"
   (check-equal? (linewrap (list a sp sp sp br b) 2) (list a 'lb b))
   (check-equal? (linewrap (list x sp br sp sp x x sp) 3) (list x 'lb x x))
   (check-equal? (linewrap (list sp sp x x sp sp br sp sp sp x) 3) (list x x 'lb x))
   (check-equal? (linewrap (list a sp b sp sp br sp c) 3) (list a sp b 'lb c))
   (check-equal? (linewrap (list x x x x) 3) (list x x x 'lb x))
   (check-equal? (linewrap (list x x x sp x x) 2) (list x x 'lb x 'lb x x))
   (check-equal? (linewrap (list x x x sp x x) 3) (list x x x 'lb x x)))

  (define (visual-wrap str int [debug #f])
    (apply string (for/list ([b (in-list (linewrap (atomize str) int debug))])
                            (cond
                              [(quad? b) (car (elems b))]
                              [else #\|]))))

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

  (define (pagewrap xs size [debug #f])
    (wrap xs size debug
          #:break-val 'pb
          #:mandatory-break-proc (λ (x) (and (quad? x) (memv (car (elems x)) '(#\page))))
          #:optional-break-proc (λ (x) (eq? x 'lb))))
  (define pbr (q '(size (0 0)) #\page))

  (test-case
   "soft page breaks"
   (check-equal? (pagewrap null 2) null)
   (check-equal? (pagewrap (list x) 2) (list x))
   (check-equal? (pagewrap (list x x) 2) (list x x))
   (check-equal? (pagewrap (list x x x) 1) (list x 'pb x 'pb x))
   (check-equal? (pagewrap (list x x x) 2) (list x x 'pb x))
   (check-equal? (pagewrap (list x x x) 3) (list x x x))
   (check-equal? (pagewrap (list x x x) 4) (list x x x))
   (check-equal? (pagewrap (list x 'lb x x) 2) (list x 'pb x x)))

  (test-case
   "hard page breaks"
   (check-equal? (pagewrap (list x pbr x x) 2) (list x 'pb x x))
   (check-equal? (pagewrap (list x pbr x x) 1) (list x 'pb x 'pb x))
   (check-equal? (pagewrap (list x pbr pbr x x) 1) (list x 'pb 'pb x 'pb x))
   (check-equal? (pagewrap (list x pbr pbr x x) 2) (list x 'pb 'pb x x))
   (check-equal? (pagewrap (list 'lb x 'lb 'lb pbr 'lb x x 'lb) 2) (list x 'pb x x)))

  (test-case
   "composed line breaks and page breaks"
   (check-equal? (pagewrap (linewrap null 1) 2) null)
   (check-equal? (pagewrap (linewrap (list x) 1) 2) (list x))
   (check-equal? (pagewrap (linewrap (list x x x) 1) 2) (list x 'lb x 'pb x))
   (check-equal? (pagewrap (linewrap (list x x x) 2) 2) (list x x 'pb x))
   (check-equal? (pagewrap (linewrap (list x x x) 2) 1) (list x 'pb x 'pb x))))

(struct $slug $quad () #:transparent)
(define (slug . xs) ($slug #f xs))
(define (linewrap2 xs size [debug #f])
  (wrap xs size debug
        #:break-val 'lb
        #:mandatory-break-proc (λ (q) (and (quad? q) (memv (car (elems q)) '(#\newline))))
        #:optional-break-proc optional-break?
        #:finish-wrap-proc (λ (pcs) (list ($slug #f pcs)))))

(module+ test
  (test-case
   "mandatory breaks and spurious spaces with slugs"
   (check-equal? (linewrap2 (list a sp sp sp br b) 2) (list (slug a) 'lb (slug b)))
   (check-equal? (linewrap2 (list x sp br sp sp x x sp) 3) (list (slug x) 'lb (slug x x)))
   (check-equal? (linewrap2 (list sp sp x x sp sp br sp sp sp x) 3) (list (slug x x) 'lb (slug x)))
   (check-equal? (linewrap2 (list a sp b sp sp br sp c) 3) (list (slug a sp b) 'lb (slug c)))
   (check-equal? (linewrap2 (list x x x x) 3) (list (slug x x x) 'lb (slug x)))
   (check-equal? (linewrap2 (list x x x sp x x) 2) (list (slug x x) 'lb (slug x) 'lb (slug x x)))
   (check-equal? (linewrap2 (list x x x sp x x) 3) (list (slug x x x) 'lb (slug x x)))))