#lang typed/racket/base
(require (for-syntax racket/base racket/syntax))
(require racket/list sugar/debug racket/function racket/vector  "logger-typed.rkt")
(define-logger ocm)

(provide smawky? Entry->Value-Type Value-Type Index-Type Matrix-Proc-Type make-ocm reduce reduce2 concave-minima (prefix-out ocm- (combine-out min-value min-index)))

(: select-elements ((Listof Any) (Listof Index-Type) . -> . (Listof Any)))
(define (select-elements xs is)
  (map (λ([i : Index-Type]) ((inst list-ref Any) xs i)) is))

(: odd-elements ((Listof Any) . -> . (Listof Any)))
(define (odd-elements xs)
  (select-elements xs (range 1 (length xs) 2)))

(: vector-odd-elements ((Vectorof Any) . -> . (Vectorof Any)))
(define (vector-odd-elements xs)
  (for/vector ([i (in-range (vector-length xs))] #:when (odd? i))
    (vector-ref xs i)))

(: even-elements ((Listof Any) . -> . (Listof Any)))
(define (even-elements xs)
  (select-elements xs (range 0 (length xs) 2)))


;; Wrapper for the matrix procedure
;; that automatically maintains a hash cache of previously-calculated values
;; because the minima operations tend to hit the same values.
;; Assuming here that (matrix i j) is invariant
;; and that the matrix function is more expensive than the cache lookup.


(define-syntax-rule (vector-append-item xs value)
  ((inst vector-append Any) xs (vector value)))

(: vector-set (All (a) ((Vectorof a) Integer a -> (Vectorof a))))
(define (vector-set vec idx val)
  (vector-set! vec idx val)
  vec)

(define-syntax-rule (vector-cdr vec)
  (vector-drop vec 1))

(define-syntax-rule (vector-empty? vec)
  (= 0 (vector-length vec)))


(define (integers? x) (and (list? x) (andmap integer? x)))

;; Reduce phase: make number of rows at most equal to number of cols
(: reduce ((Vectorof Index-Type) (Vectorof Index-Type) Matrix-Proc-Type Entry->Value-Type . -> . (Vectorof Index-Type)))
(define (reduce row-indices col-indices matrix-proc entry->value)
  ;(vector? vector? procedure? procedure? . -> . vector?)
  (log-ocm-debug "starting reduce phase with")
  (log-ocm-debug "row-indices = ~a" row-indices)
  (log-ocm-debug "col-indices = ~a" col-indices)
  
  (: process-stack ((Vectorof Index-Type) Index-Type . -> . (Vectorof Index-Type)))
  (define (process-stack stack row-idx)
    (log-ocm-debug "row stack = ~a" stack)
    (let ([last-stack-idx (sub1 (vector-length stack))])
      (cond
        [(and (>= (vector-length stack) 1) 
              (log-ocm-debug "comparing row values at column ~a" (vector-ref col-indices last-stack-idx))
              (log-ocm-debug "end of row stack (~a) value at column ~a = ~a" (vector-ref stack last-stack-idx) (vector-ref col-indices last-stack-idx) (entry->value (matrix-proc (vector-ref stack last-stack-idx) (vector-ref col-indices last-stack-idx))))
              (log-ocm-debug "challenger row (~a) value at column ~a = ~a" row-idx (vector-ref col-indices last-stack-idx) (entry->value (matrix-proc row-idx (vector-ref col-indices last-stack-idx))))
              (> (entry->value (matrix-proc (vector-ref stack last-stack-idx) (vector-ref col-indices last-stack-idx)))
                 (entry->value (matrix-proc row-idx (vector-ref col-indices last-stack-idx)))))
         
         (log-ocm-debug "challenger row (~a) wins with a new minimum ~a, so end of row stack (~a) is removed" row-idx (entry->value (matrix-proc row-idx (vector-ref col-indices last-stack-idx))) (vector-ref stack last-stack-idx))
         (process-stack (vector-drop-right stack 1) row-idx)]
        [else 
         (log-ocm-debug (if (< (vector-length stack) 1) 
                            (format "row stack too short for challenge, pushing row ~a" row-idx)
                            (format "challenger row (~a) loses to end of row stack (~a), so ~a joins stack" row-idx (vector-ref stack last-stack-idx) row-idx)))
         stack])))  
  
  (define reduced-row-indexes
    (for/fold : (Vectorof Index-Type) ([stack (cast (vector) (Vectorof Index-Type))]) ([row-idx (in-vector row-indices)])
      (let ([stack (process-stack stack row-idx)])
        (if (= (vector-length stack) (vector-length col-indices))
            stack
            ((inst vector-append Index-Type) stack (vector row-idx))))))
  (log-ocm-debug "finished reduce. row indexes = ~v" reduced-row-indexes)
  reduced-row-indexes)

(: reduce2 ((Vectorof Index-Type) (Vectorof Index-Type) Matrix-Proc-Type Entry->Value-Type . -> . (Vectorof Index-Type)))
(define (reduce2 row-indices col-indices matrix-proc entry->value)
  (let find-survivors ([rows row-indices][survivors : (Listof Index-Type) empty])
    (cond 
      [(vector-empty? rows) ((inst list->vector Index-Type) (reverse survivors))]
      [else
       (define challenger-row (vector-ref rows 0))
       (cond
         ;; no survivors yet, so push first row and keep going
         [(empty? survivors) (find-survivors (vector-cdr rows) (cons challenger-row survivors))]
         [else
          (define index-of-last-survivor (sub1 (length survivors)))
          (define col-head (vector-ref col-indices index-of-last-survivor))
          (define-syntax-rule (test-function r) (entry->value (matrix-proc r col-head)))
          (cond
            ;; this is the challenge: is the head cell of challenger a new minimum?
            ;; use < not <=, so the recorded winner is the earliest row with the new minimum, not the latest row
            ;; if yes, challenger wins. pop element from stack, and let challenger try again (= leave rows alone)
            [(< (test-function challenger-row) (test-function (car survivors))) (find-survivors rows (cdr survivors))]
            
            ;; if not, challenger lost.
            ;; If we're in the last column, ignore the loser by recurring on the same values
            [(= col-head (vector-last col-indices)) (find-survivors (vector-cdr rows) survivors)]
            
            ;; otherwise challenger lost and we're not in last column, 
            ;; so add challenger to survivor stack
            [else (find-survivors (vector-cdr rows) (cons challenger-row survivors))])])])))

;; define a special type so it can be reused in `interpolate`
;; it is (cons value row-idx)
(define-type Make-Minimum-Input (Pair Any Index-Type)) 
(: make-minimum (Make-Minimum-Input . -> . (HashTable Any Any)))
(define (make-minimum value-rowidx-pair)
  (define ht ((inst make-hash Any Any)))
  (! ht 'value (car value-rowidx-pair))
  (! ht 'row-idx (cdr value-rowidx-pair))
  ht)


;; Interpolate phase: in the minima hash, add results for even rows 

(define-syntax-rule (vector-last v)
  (vector-ref v (sub1 (vector-length v))))

(: interpolate ((HashTable Any Any) (Vectorof Index-Type) (Vectorof Index-Type) Matrix-Proc-Type Entry->Value-Type . -> . (HashTable Any Any)))
(define (interpolate minima row-indices col-indices matrix-proc entry->value)
  ;(hash? vector? vector? procedure? procedure? . -> . hash?)
  (for ([col-idx (in-range 0 (vector-length col-indices) 2)]) ;; even-col-indices
    (define col (vector-ref col-indices col-idx))
    (define idx-of-last-row
      (cast (if (= col-idx (sub1 (vector-length col-indices)))
                (vector-last row-indices)
                (hash-ref (cast (hash-ref minima (vector-ref col-indices (add1 col-idx))) HashTableTop) 'row-idx)) Index-Type))
    
    (define smallest-value-entry
      ((inst vector-argmin Make-Minimum-Input) (λ(x) (entry->value (car x)))
                                               (for/vector : (Vectorof Make-Minimum-Input) 
                                                 ([row-idx (in-list ((inst dropf-right Index-Type) (vector->list row-indices) (λ(x) (not (= x idx-of-last-row)))))])
                                                 (cons (matrix-proc row-idx col) row-idx))))
    
    (! minima col (make-minimum smallest-value-entry)))
  minima)

(: interpolate2 ((HashTable Any Any) (Vectorof Index-Type) (Vectorof Index-Type) Matrix-Proc-Type Entry->Value-Type . -> . (HashTable Any Any)))
(define (interpolate2 minima row-indices col-indices matrix-proc entry->value)
  (define idx-of-last-col (sub1 (vector-length col-indices)))
  (define (smallest-value-entry [col : Index-Type] [idx-of-last-row : Index-Type])
    ((inst argmin Make-Minimum-Input) (λ(x) (entry->value (car x)))
                                      (for/list ([row-idx (stop-after (in-vector row-indices) (λ(x) (= idx-of-last-row x)))])
                                        (cons (matrix-proc row-idx col) row-idx))))
  
  (for ([([col : Index-Type] col-idx) (in-indexed col-indices)] #:when (even? col-idx))
    (define idx-of-last-row (cast (if (= col-idx idx-of-last-col)
                                      (vector-last row-indices)
                                      (hash-ref (cast (hash-ref minima (vector-ref col-indices (add1 col-idx))) HashTableTop) 'row-idx)) Index-Type))   
    (! minima col (make-minimum (smallest-value-entry col idx-of-last-row))))
  minima)


;; The return value `minima` is a hash:
;; the keys are col-indices (integers)
;; the values are pairs of (value row-index).
(: concave-minima ((Vectorof Index-Type) (Vectorof Index-Type) Matrix-Proc-Type Entry->Value-Type . -> . HashTableTop))
(define (concave-minima row-indices col-indices matrix-proc entry->value)
  ;((vector?) ((or/c #f vector?) procedure? procedure?) . ->* . hash?)
  (define reduce-proc reduce2)
  (define interpolate-proc interpolate2)
  (if (= 0 (vector-length col-indices)) 
      (make-hash)
      (let ([row-indices (reduce-proc row-indices col-indices matrix-proc entry->value)])
        (define odd-column-minima (concave-minima row-indices (cast (vector-odd-elements (cast col-indices (Vectorof Any))) (Vectorof Index-Type)) matrix-proc entry->value))
        (interpolate-proc (cast odd-column-minima (HashTable Any Any)) row-indices col-indices matrix-proc entry->value))))


(define no-value 'none)

(define-syntax-rule (@ hashtable key)
  (hash-ref hashtable key))

(define-syntax-rule (! hashtable key value)
  (hash-set! hashtable key value))

(define-type Index-Type Nonnegative-Integer)
(define-type Value-Type Flonum)
(define-type Initial-Value-Type Value-Type)
(define-type No-Value-Type Symbol)
(define-type Finished-Value-Type Index-Type)
(define-type Matrix-Proc-Type (Index-Type Index-Type . -> . Value-Type))
(define-type Entry->Value-Type (Any . -> . Value-Type))
;(define-type OCM-Type (Vector (Vectorof Value-Type) (Vectorof (U Index-Type No-Value-Type)) Finished-Value-Type Matrix-Proc-Type Entry->Value-Type Index-Type Index-Type))
(define-type OCM-Type (Vector Any Any Any Any Any Any))
(define o:min-values 0)
(define o:min-row-indices 1)
(define o:finished 2)
(define o:matrix-proc 3)
(define o:entry->value 4)
(define o:base 5)
(define o:tentative 6)

(: ocm-matrix-proc (OCM-Type . -> . Matrix-Proc-Type))
(define (ocm-matrix-proc o)
  (cast (vector-ref o o:matrix-proc) Matrix-Proc-Type))

(: ocm-set-matrix-proc (OCM-Type Matrix-Proc-Type . -> . Void))
(define (ocm-set-matrix-proc o proc)
  (vector-set! o o:matrix-proc (cast proc Matrix-Proc-Type)))


(: ocm-entry->value (OCM-Type . -> . Entry->Value-Type))
(define (ocm-entry->value o)
  (cast (vector-ref o o:entry->value) Entry->Value-Type))

(: ocm-set-entry->value (OCM-Type Entry->Value-Type . -> . Void))
(define (ocm-set-entry->value o proc)
  (vector-set! o o:entry->value (cast proc Entry->Value-Type)))

(: ocm-finished (OCM-Type . -> . Finished-Value-Type))
(define (ocm-finished o)
  (cast (vector-ref o o:finished) Finished-Value-Type))

(: ocm-set-finished (OCM-Type Finished-Value-Type . -> . Void))
(define (ocm-set-finished o v)
  (vector-set! o o:finished (cast v Finished-Value-Type)))

(: ocm-tentative (OCM-Type . -> . Index-Type))
(define (ocm-tentative o)
  (cast (vector-ref o o:tentative) Index-Type))

(: ocm-set-tentative (OCM-Type Index-Type . -> . Void))
(define (ocm-set-tentative o v)
  (vector-set! o o:tentative (cast v Index-Type)))

(: ocm-base (OCM-Type . -> . Index-Type))
(define (ocm-base o)
  (cast (vector-ref o o:tentative) Index-Type))

(: ocm-set-base (OCM-Type Index-Type . -> . Void))
(define (ocm-set-base o v)
  (vector-set! o o:tentative (cast v Index-Type)))


(: ocm-min-values (OCM-Type . -> . (Vectorof Value-Type)))
(define (ocm-min-values o)
  (cast (vector-ref o o:min-values) (Vectorof Value-Type)))

(: ocm-set-min-values (OCM-Type (Vectorof Value-Type) . -> . Void))
(define (ocm-set-min-values o vs)
  (vector-set! o o:min-values (cast vs (Vectorof Value-Type))))


(: ocm-min-row-indices (OCM-Type . -> . (Vectorof (U Index-Type No-Value-Type))))
(define (ocm-min-row-indices o)
  (cast (vector-ref o o:min-row-indices) (Vectorof (U Index-Type No-Value-Type))))

(: ocm-set-min-row-indices (OCM-Type (Vectorof (U Index-Type No-Value-Type)) . -> . Void))
(define (ocm-set-min-row-indices o vs)
  (vector-set! o o:min-row-indices (cast vs (Vectorof (U Index-Type No-Value-Type)))))



(: make-ocm ((Matrix-Proc-Type Entry->Value-Type) (Initial-Value-Type) . ->* . OCM-Type))
(define (make-ocm matrix-proc entry->value [initial-value 0.0])
  (log-ocm-debug "making new ocm")
  (define ocm (cast (make-vector 7) OCM-Type))
  (ocm-set-min-values ocm (vector initial-value))
  (ocm-set-min-row-indices ocm (vector no-value))
  (ocm-set-finished ocm 0)
  (ocm-set-matrix-proc ocm matrix-proc)
  (ocm-set-entry->value ocm entry->value) ; for converting matrix values to an integer
  (ocm-set-base ocm 0)
  (ocm-set-tentative ocm 0)
  ocm)

;; Return min { Matrix(i,j) | i < j }.
(: min-value (OCM-Type Index-Type . -> . Any))
(define (min-value ocm j)
  (if (< (cast (ocm-finished ocm) Real) j)
      (begin (advance! ocm) (min-value ocm j))
      (vector-ref (ocm-min-values ocm) j)))

;; Return argmin { Matrix(i,j) | i < j }.
(: min-index (OCM-Type Index-Type . -> . (U Index-Type No-Value-Type)))
(define (min-index ocm j)
  (if (< (cast (ocm-finished ocm) Real) j)     
      (begin (advance! ocm) (min-index ocm j))
      ((inst vector-ref (U Index-Type No-Value-Type)) (ocm-min-row-indices ocm) j)))

;; Finish another value,index pair.
(: advance! (OCM-Type . -> . Void))
(define (advance! ocm)
  (define next (add1 (ocm-finished ocm)))  
  (log-ocm-debug "advance! ocm to next = ~a" (add1 (ocm-finished ocm)))
  (cond
    ;; First case: we have already advanced past the previous tentative
    ;; value.  We make a new tentative value by applying ConcaveMinima
    ;; to the largest square submatrix that fits under the base.
    [(> next (ocm-tentative ocm))
     (log-ocm-debug "advance: first case because next (~a) > tentative (~a)" next (ocm-tentative ocm))
     (define rows : (Vectorof Index-Type) (list->vector (range (ocm-base ocm) next)))
          (ocm-set-tentative ocm (+ (ocm-finished ocm) (vector-length rows)))
     (define cols : (Vectorof Index-Type) (list->vector (range next (add1 (ocm-tentative ocm)))))
     (define minima (concave-minima rows cols (ocm-matrix-proc ocm) (ocm-entry->value ocm)))
     (error 'stop) 
     
     (for ([col (in-vector cols)])
       (cond
         [(>= col (vector-length (ocm-min-values ocm)))
          (ocm-set-min-values ocm (vector-append-item (ocm-min-values ocm) (@ (cast (@ minima col) HashTableTop) 'value)))
          (ocm-set-min-row-indices ocm (vector-append-item (ocm-min-row-indices ocm) (@ (cast (@ minima col) HashTableTop) 'row-idx)))]
         [(< ((ocm-entry->value ocm) (@ (cast (@ minima col) HashTableTop) 'value)) ((ocm-entry->value ocm) (vector-ref (ocm-min-values ocm) col)))
          (ocm-set-min-values ocm ((inst vector-set Index-Type) (ocm-min-values ocm) col (cast (@ (cast (@ minima col) HashTableTop) 'value) Index-Type)))
          (ocm-set-min-row-indices ocm ((inst vector-set Index-Type) (ocm-min-row-indices ocm) col (cast (@ (cast (@ minima col) HashTableTop) 'row-idx) Index-Type)))]))
     
     (ocm-set-finished ocm next)]
    
    [else
     ;; Second case: the new column minimum is on the diagonal.
     ;; All subsequent ones will be at least as low,
     ;; so we can clear out all our work from higher rows.
     ;; As in the fourth case, the loss of tentative is
     ;; amortized against the increase in base.
     (define diag ((ocm-matrix-proc ocm) (sub1 next) next))
     (cond
       [(< ((ocm-entry->value ocm) diag) ((ocm-entry->value ocm) (vector-ref (ocm-min-values ocm) next)))
        (log-ocm-debug "advance: second case because column minimum is on the diagonal")
        (ocm-set-min-values ocm (vector-set (ocm-min-values ocm) next diag))
        (ocm-set-min-row-indices ocm (vector-set (ocm-min-row-indices ocm) next (sub1 next)))
        (ocm-set-base ocm (sub1 next))
        (ocm-set-tentative ocm next)
        (ocm-set-finished ocm next)]
       
       ;; Third case: row i-1 does not supply a column minimum in
       ;; any column up to tentative. We simply advance finished
       ;; while maintaining the invariant.
       [(>= ((ocm-entry->value ocm) ((ocm-matrix-proc ocm) (sub1 next) (ocm-tentative ocm)))
            ((ocm-entry->value ocm) (vector-ref (ocm-min-values ocm) (ocm-tentative ocm))))
        (log-ocm-debug "advance: third case because row i-1 does not suppply a column minimum")
        (ocm-set-finished ocm next)]
       
       ;; Fourth and final case: a new column minimum at self._tentative.
       ;; This allows us to make progress by incorporating rows
       ;; prior to finished into the base.  The base invariant holds
       ;; because these rows cannot supply any later column minima.
       ;; The work done when we last advanced tentative (and undone by
       ;; this step) can be amortized against the increase in base.
       [else
        (log-ocm-debug "advance: fourth case because new column minimum")
        (ocm-set-base ocm (sub1 next))
        (ocm-set-tentative ocm next)
        (ocm-set-finished ocm next)])]))

(: print (OCM-Type . -> . Void))
(define (print ocm)
  (displayln (ocm-min-values ocm))
  (displayln (ocm-min-row-indices ocm)))


(: smawky? ((Listof (Listof Real)) . -> . Boolean))
(define (smawky? m)
  (: position-of-minimum ((Listof Real) . -> . Index-Type)) 
  (define (position-of-minimum xs)
    ;; put each element together with its list index
    (let ([xs : (Listof (Pairof Index-Type Real)) (map (inst cons Index-Type Real) (range (length xs)) xs)]) 
      ;; find the first one with the min value, and grab the list index
      (car ((inst argmin (Pairof Index-Type Real)) cdr (filter (λ([x : (Pairof Index-Type Real)]) (not (negative? (cdr x)))) xs)))))
  ;; tests if penalty matrix is monotone for non-negative values.
  (define increasing-minima? (apply <= (cast (map position-of-minimum m) (List* Real Real (Listof Real)))))
  
  (define monotone? : Boolean
    (for/and ([ridx (in-range 1 (length m))])
      (for/and : Boolean ([cidx (in-range (sub1 (length (car m))))])
        (cast (let* ([prev-row : (Listof Real) ((inst list-ref (Listof Real)) m (sub1 ridx))]
                     [row : (Listof Real) (list-ref m ridx)]
                     [a : Real (list-ref prev-row cidx)]
                     [b : Real (list-ref prev-row (add1 cidx))]
                     [c : Real (list-ref row cidx)]
                     [d : Real (list-ref row (add1 cidx))])
                (if (andmap (λ([x : Real]) (not (negative? x))) (list a b c d)) ;; smawk disregards negative values
                    (cond
                      [(< c d) (if (< a b) #t (error (format "Submatrix ~a not monotone in ~a" (list (list a b) (list c d)) m)))]
                      [(= c d) (if (<= a b) #t (error (format "Submatrix ~a not monotone in ~a" (list (list a b) (list c d)) m)))]
                      [else #t])
                    #t)) Boolean))))
  
  (and increasing-minima? monotone?))
