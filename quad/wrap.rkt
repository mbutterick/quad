#lang debug racket/base
(require racket/contract racket/list racket/match txexpr sugar/debug sugar/define sugar/list racket/promise racket/function (only-in racket/control call/prompt) racket/future 
         "param.rkt" "qexpr.rkt" "atomize.rkt" "quad.rkt" "generic.rkt" "position.rkt")

(define/contract (distance q)
  (any/c . -> . real?)
  ;; linear distance from in point to out point
  (cond
    [(quad? q)
     (match-define (list ∆x ∆y) (map - (out-point q) (in-point q)))
     (cond
       [(zero? ∆x) ∆y]
       [(zero? ∆y) ∆x]
       [else (sqrt (+ (* ∆x ∆x) (* ∆y ∆y)))])]
    [else 0]))


(define+provide/contract (wrap xs
                               [target-size (current-wrap-distance)]
                               [debug #f]
                               #:break-val [break-val 'break]
                               #:break-before? [break-before? #f]
                               #:break-after? [break-after? #f]
                               #:mandatory-break-proc [mandatory-break? (const #f)]
                               #:optional-break-proc [optional-break? (const #f)]
                               #:finish-wrap-proc [finish-wrap-proc values])
  ((any/c) (real? any/c
                  #:break-val any/c
                  #:break-before? boolean?
                  #:break-after? boolean?
                  #:mandatory-break-proc procedure?
                  #:optional-break-proc procedure?
                  #:finish-wrap-proc procedure?) . ->* . (listof any/c))
  (wrap-private xs
                target-size
                debug
                break-val
                break-before?
                break-after?
                mandatory-break?
                optional-break?
                finish-wrap-proc))

;; the mandatory breaks are used to divide the wrap territory into smaller chunks
;; that can be cached, parallelized, etc.
(define (wrap-private xs
                      target-size
                      debug
                      break-val
                      break-before?
                      break-after?
                      mandatory-break?
                      optional-break?
                      finish-wrap-proc)
  (define break-val-equal? (if (symbol? break-val) eq? equal?))
  (define (cleanup-wraplist xs) (dropf-right (append* (reverse xs)) (λ (x) (break-val-equal? break-val x))))
  (define wraps
    (for/fold ([wraps null]
               [xs (dropf xs mandatory-break?)]
               #:result (map touch wraps))
              ([i (in-naturals)]
               #:break (null? xs))
      (cond
        [(mandatory-break? (car xs))
         (when debug (report x 'mandatory-break))
         (values (cons (future (λ () (list break-val))) wraps) (cdr xs))]
        [else
         (define-values (head tail) (splitf-at xs (λ (x) (not (mandatory-break? x)))))
         (values (cons (future (λ () (cleanup-wraplist (wrap-optionals head
                                                                       target-size
                                                                       debug
                                                                       break-val
                                                                       optional-break?
                                                                       finish-wrap-proc)))) wraps) tail)])))
  (append (if break-before? (list break-val) empty) (cleanup-wraplist wraps) (if break-after? (list break-val) empty)))


(define (nonprinting-at-start? x) (if (quad? x) (not (printable? x 'start)) #t))
(define (nonprinting-at-end? x) (if (quad? x) (not (printable? x 'end)) #t))
(define (wrap-optionals xs
                        target-size
                        debug
                        break-val
                        optional-break?
                        finish-wrap-proc)
  (define start-signal (gensym))
  (define last-optional-break-k #f)
  (define (capture-optional-break-k!)
    (when debug (report 'capturing-break))
    (let/cc k (set! last-optional-break-k k) #f))
  (call/prompt ;; continuation boundary for last-optional-break-k
   (thunk
    (let loop ([wraps null][wrap-pieces null][dist-so-far start-signal][xs xs])
      (cond
        [(null? xs)
         ;; combine the segments into a flat list, and drop any trailing breaks
         ;; (on the idea that breaks should separate things, and there's nothing left to separate)
         ;; wraps alternate with breaks
         (for/list ([pcs (in-list (cons wrap-pieces wraps))]
                    [proc (in-cycle (list 
                                     ;; pieces will have been accumulated in reverse order
                                     ;; dropf drops from beginning of list (representing the end of the wrap)
                  
                                     (λ (pcs) (finish-wrap-proc (reverse (dropf pcs (λ (x) (and (optional-break? x) (nonprinting-at-end? x)))))))
                                     values))])
           (proc pcs))]
        [else
         (define x (car xs))
         (define at-start? (eq? dist-so-far start-signal))
         (define underflow? (and (not at-start?) (<= (+ dist-so-far (if (and (quad? x) (printable? x 'end)) (distance x) 0)) target-size)))
         (define (values-for-insert-break [before? #f])
           ;; a break can be inserted before or after the current quad.
           ;; At an ordinary break (mandatory or optional) it goes after the wrap point.
           ;; The wrap signal consumes the break if it's nonprinting (e.g., word space or hard break)
           ;; but not if it's printing (e.g., hyphen).
           ;; But if no ordinary break can be found for a line, the wrap will happen before the quad.
           ;; The wrap signal will not consume the quad (rather, it will become the first quad in the next wrap)
           ;; (we do this by resetting next-xs to the whole xs list)
           ;; In both cases, the `finish-wrap` proc will strip off any trailing white breaks from the new segment.
           (set! last-optional-break-k #f) ;; prevents continuation loop
           (if before?
               (values wrap-pieces xs)
               ; omit nonprinting quad
               (values (if (and (quad? x) (nonprinting-at-end? x)) wrap-pieces (cons x wrap-pieces)) (cdr xs))))
         (cond
           [(and at-start? (optional-break? x) (nonprinting-at-start? x))
            (when debug (report x 'skipping-optional-break-at-beginning))
            ;; skip it
            (loop wraps null dist-so-far (cdr xs))]
           [(and underflow? (optional-break? x) (capture-optional-break-k!))
            (when debug (report x 'resuming-break-from-continuation))
            (define-values (pieces-for-this-wrap next-xs) (values-for-insert-break))
            (loop (list* (list break-val) pieces-for-this-wrap wraps)
                  null
                  start-signal
                  next-xs)]
           ;; the easy case of accumulating quads in the middle of a wrap
           [(or (and underflow? (when debug (report x 'add-underflow)) #t) 
                ;; assume printing (nonprinting were handled in first case)
                ;; this branch reached if the first quad on the line causes an overflow
                ;; That sounds weird, but maybe it's just really big.
                (and at-start? (when debug (report x 'add-at-start)) #t)
                ;; we do want to accumulate nonprinting optional breaks (like wordspaces and soft hyphens) in the middle.
                ;; in case we eventually encounter a printing quad that fits on the line.
                ;; if we don't (ie. the line overflows) then they will get stripped by `finish-wrap`
                (and (optional-break? x) (nonprinting-at-end? x) (when debug (report x 'add-nonprinting-optional-break)) #t))
            (define printable (and (quad? x) (printable? x (and at-start? 'start)))) 
            (define dist (and printable (distance x)))
            (loop wraps
                  (if (and (quad? x) (not printable)) wrap-pieces (cons x wrap-pieces)) ; omit nonprinting quad
                  (if at-start? (or dist start-signal) (+ dist-so-far (or dist 0)))
                  (cdr xs))]
           ;; the previous branch will catch all `underflow?` cases
           ;; therefore, in these last two cases, we have overflow
           [last-optional-break-k ;; overflow implied
            ;; if we have an optional break stored, we jump back and use it
            ;; now that we know we need it.
            (when debug (report x 'invoking-last-breakpoint))
            (last-optional-break-k #t)]
           [else ;; overflow implied
            ;; if we don't have an optional break stored, we need to just end the wrap and move on
            ;; we insert the break `before` so that the current quad is moved to the next wrap
            ;; no, it's not going to look good, but if we reach this point, we are in weird conditions
            (when debug (report x 'falling-back))
            (define-values (pieces-for-this-wrap next-xs) (values-for-insert-break 'before))
            (loop (list* (list break-val) pieces-for-this-wrap wraps)
                  null
                  start-signal
                  next-xs)])])))))


(define x (q (list 'size (pt 1 1)) #\x))
(define zwx (q (list 'size (pt 0 0)) #\z))
(define hyph (q (list 'size (pt 1 1)) #\-))
(define shy (q (list 'size (pt 1 1) 'printable? (λ (sig)
                                                  (case sig
                                                    [(end) #t]
                                                    [else #f]))) #\-))
(define a (q (list 'size (pt 1 1)) #\a))
(define b (q (list 'size (pt 1 1)) #\b))
(define c (q (list 'size (pt 1 1)) #\c))
(define d (q (list 'size (pt 1 1)) #\d))
(define sp (q (list 'size (pt 1 1) 'printable? (λ (sig)
                                                 (case sig
                                                   [(start end) #f]
                                                   [else #t]))) #\space))
(define br (q (list 'size (pt 0 0) 'printable? #f) #\newline))
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
   "hard hyphens"
   (check-equal? (linewrap (list hyph) 1) (list hyph))
   (check-equal? (linewrap (list hyph hyph) 1) (list hyph 'lb hyph))
   (check-equal? (linewrap (list hyph hyph) 2) (list hyph hyph))
   (check-equal? (linewrap (list hyph hyph hyph) 2) (list hyph hyph 'lb hyph))
   (check-equal? (linewrap (list x hyph) 1) (list x 'lb hyph))
   (check-equal? (linewrap (list x x hyph x x) 1) (list x 'lb x 'lb hyph 'lb x 'lb x))
   (check-equal? (linewrap (list x x hyph x x) 2) (list x x 'lb hyph x 'lb x))
   (check-equal? (linewrap (list x x hyph x x) 3) (list x x hyph 'lb x x))
   (check-equal? (linewrap (list x x hyph x x) 4) (list x x hyph 'lb x x))
   (check-equal? (linewrap (list x x hyph x x) 5) (list x x hyph x x)))

  (test-case
   "soft hyphens"
   (check-equal? (linewrap (list shy) 1) (list))
   (check-equal? (linewrap (list shy shy) 2) (list))
   (check-equal? (linewrap (list shy shy shy) 2) (list))
   (check-equal? (linewrap (list x shy) 1) (list x))
   (check-equal? (linewrap (list x shy shy shy shy) 1) (list x))
   (check-equal? (linewrap (list x x shy x x) 1) (list x 'lb x 'lb x 'lb x))
   (check-equal? (linewrap (list x x shy x x) 2) (list x x 'lb x x))
   (check-equal? (linewrap (list x x shy x x) 3) (list x x shy 'lb x x))
   (check-equal? (linewrap (list x x shy x x) 4) (list x x x x))
   (check-equal? (linewrap (list x x shy x x) 5) (list x x x x))
   (check-equal? (linewrap (list x x shy x sp x) 4) (list x x x 'lb x)))

  (test-case
   "zero width nonbreakers"
   (check-equal? (linewrap (list sp zwx) 2) (list zwx))
   (check-equal? (linewrap (list zwx sp) 2) (list zwx))
   (check-equal? (linewrap (list sp zwx sp) 2) (list zwx))
   (check-equal? (linewrap (list sp sp zwx sp sp) 2) (list zwx))
   (check-equal? (linewrap (list sp sp zwx sp sp zwx sp) 2) (list zwx sp sp zwx)))

  (test-case
   "mandatory breaks"
   (check-equal? (linewrap (list br) 2) (list)) ;; only insert a break if it's between things
   (check-equal? (linewrap (list a br b) 2) (list a 'lb b))
   (check-equal? (linewrap (list a b br) 2) (list a b))
   (check-equal? (linewrap (list a b br br) 2) (list a b))
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
   (check-equal? (linewrap (list x x x sp x x) 3) (list x x x 'lb x x))))

(define (visual-wrap str int [debug #f])
  (apply string (for/list ([b (in-list (linewrap (for/list ([atom (atomize str)])
                                                   ($quad (hash-set (attrs atom) 'size '(1 1))
                                                          (elems atom))) int debug))])
                  (cond
                    [(quad? b) (car (elems b))]
                    [else #\|]))))
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
  (wrap xs size debug
        #:break-val 'pb
        #:break-before? #t
        #:mandatory-break-proc (λ (x) (and (quad? x) (memv (car (elems x)) '(#\page))))
        #:optional-break-proc (λ (x) (eq? x 'lb))))
(define pbr (q '(size #f) #\page))

(module+ test
  (test-case
   "soft page breaks"
   (check-equal? (pagewrap null 2) '(pb))
   (check-equal? (pagewrap (list x) 2) (list 'pb x))
   (check-equal? (pagewrap (list x x) 2) (list 'pb x x))
   (check-equal? (pagewrap (list x x x) 1) (list 'pb x 'pb x 'pb x))
   (check-equal? (pagewrap (list x x x) 2) (list 'pb x x 'pb x))
   (check-equal? (pagewrap (list x x x) 3) (list 'pb x x x))
   (check-equal? (pagewrap (list x x x) 4) (list 'pb x x x))
   (check-equal? (pagewrap (list x 'lb x x) 2) (list 'pb x 'pb x x)))

  (test-case
   "hard page breaks"
   (check-equal? (pagewrap (list x pbr x x) 2) (list 'pb x 'pb x x))
   (check-equal? (pagewrap (list x pbr x x) 1) (list 'pb x 'pb x 'pb x))
   (check-equal? (pagewrap (list x pbr pbr x x) 1) (list 'pb x 'pb 'pb x 'pb x))
   (check-equal? (pagewrap (list x pbr pbr x x) 2) (list 'pb x 'pb 'pb x x))
   (check-equal? (pagewrap (list 'lb x 'lb 'lb pbr 'lb x x 'lb) 2) (list 'pb x 'pb x x)))

  (test-case
   "composed line breaks and page breaks"
   (check-equal? (pagewrap (linewrap null 1) 2) '(pb) )
   (check-equal? (pagewrap (linewrap (list x) 1) 2) (list 'pb x))
   (check-equal? (pagewrap (linewrap (list x x x) 1) 2) (list 'pb x 'lb x 'pb x))
   (check-equal? (pagewrap (linewrap (list x x x) 2) 2) (list 'pb x x 'pb x))
   (check-equal? (pagewrap (linewrap (list x x x) 2) 1) (list 'pb x 'pb x 'pb x))))

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