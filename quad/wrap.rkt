#lang debug racket/base
(require racket/contract racket/list racket/match txexpr sugar/debug sugar/define sugar/list racket/promise racket/function (only-in racket/control call/prompt)
         "param.rkt" "qexpr.rkt" "atomize.rkt" "quad.rkt" "generic.rkt")

(define+provide/contract (wrap xs
                                [target-size (current-line-width)]
                                [debug #f]
                                #:break-val [break-val 'break]
                                #:mandatory-break-proc [mandatory-break? (const #f)]
                                #:optional-break-proc [optional-break? (const #f)]
                                #:finish-segment-proc [finish-segment-proc values]
                                #:size-proc [size-proc (const 1)])
  ((any/c) (integer? any/c
                     #:break-val any/c
                     #:mandatory-break-proc procedure?
                     #:optional-break-proc procedure?
                     #:size-proc procedure?
                     #:finish-segment-proc procedure?) . ->* . (listof any/c))
  (define start-signal (gensym))
  (define (finish-segment pieces) (finish-segment-proc (reverse (dropf pieces optional-break?))))
  (define last-optional-break-k #f)
  (call/prompt ;; continuation boundary for last-optional-break-k
   (thunk
    (define (capture-optional-break-k!) (let/cc k (set! last-optional-break-k k) #f))
    (for/fold ([segments null]
               [pieces null]
               [size-so-far start-signal]
               #:result (append* (reverse (cons (finish-segment pieces) segments))))
              ([x (in-list xs)])
      (define-values (size-start size-mid size-end) (size-proc x))
      (define at-start? (eq? size-so-far start-signal))
      (define underflow? (and (not at-start?) (<= (+ size-so-far size-end) target-size)))
      (define (add-to-segment) (values segments (cons x pieces) (if at-start?
                                                                    size-start
                                                                    (+ size-so-far size-mid))))
      (define (insert-break)
        ;; when break is found, q is omitted from accumulation
        ;; and any preceding optional breaks are dropped (that would be trailing before the break)
        (values (list* (list break-val) (finish-segment pieces) segments) null start-signal))
      (cond
        [(mandatory-break? x) (when debug (report x 'got-mandatory-break))
                              (insert-break)]
        [(optional-break? x)
         (cond
           [at-start? (when debug (report x 'skipping-opt-break-at-beginning)) (values segments null size-so-far)]
           [(and underflow? (capture-optional-break-k!)) (when debug (report x 'resuming-breakpoint))
                                                         (set! last-optional-break-k #f) ;; prevents continuation loop
                                                         (insert-break)]
           [else (when debug (report x 'add-optional-break))
                 (add-to-segment)])]
        [(or at-start? underflow?) (when debug (report x 'add-ordinary-char))
                                   (add-to-segment)]
        [last-optional-break-k (when debug (report x 'invoking-last-breakpoint))
                               (last-optional-break-k #t)]
        [else (when debug (report x 'falling-back))
              (match-define-values (vals _ _) (insert-break))
              (values vals (list x) size-start)]))))) ;; fallback if no last-breakpoint-k exists


(define x (q (hasheq 'size (delay (values 1 1 1))) #\x))
(define zwx (q (hasheq 'size (delay (values 0 0 0))) #\z))
(define a (q (hasheq 'size (delay (values 1 1 1))) #\a))
(define b (q (hasheq 'size (delay (values 1 1 1))) #\b))
(define c (q (hasheq 'size (delay (values 1 1 1))) #\c))
(define d (q (hasheq 'size (delay (values 1 1 1))) #\d))
(define sp (q (hasheq 'size (delay (values 0 1 0))) #\space))
(define br (q (hasheq 'size (delay (values 0 0 0))) #\newline))
(define optional-break? (λ (q) (and (quad? q) (memv (car (elems q)) '(#\space)))))

(define (linewrap xs size [debug #f])
  (wrap xs size debug
                 #:break-val 'lb
                 #:mandatory-break-proc (λ (q) (and (quad? q) (memv (car (elems q)) '(#\newline))))
                 #:optional-break-proc optional-break?
                 #:size-proc (λ (q) (let ([val (hash-ref (attrs q) 'size (λ ()
                                                                        (if (memv (car (elems q)) '(#\space))
                                                                            (delay (values 0 1 0))
                                                                            (delay (values 1 1 1)))))])
                                      (if (promise? val) (force val) (val))))))

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

  (define (visual-wrap str int)
    (apply string (for/list ([b (in-list (linewrap (atomize str) int))])
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
                   #:optional-break-proc (λ (x) (eq? x 'lb))
                   #:size-proc (λ (q) (case q
                                        [(lb) (values 0 0 0)]
                                        [else (values 1 1 1)]))))
  (define pbr (q (hasheq 'size (delay (values 0 0 0))) #\page))

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
                 #:size-proc (λ (q) (let ([val (hash-ref (attrs q) 'size (λ ()
                                                                        (if (memv (car (elems q)) '(#\space))
                                                                            (delay (values 0 1 0))
                                                                            (delay (values 1 1 1)))))])
                                      (if (promise? val) (force val) (val))))
                 #:finish-segment-proc (λ (pcs) (list ($slug #f pcs)))))

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