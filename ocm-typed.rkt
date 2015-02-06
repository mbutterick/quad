#lang typed/racket/base
(require (for-syntax racket/base racket/syntax))
(require/typed sugar/cache [make-caching-proc (Procedure . -> . Procedure)])
(require racket/list sugar/debug rackunit racket/function racket/vector  "logger-typed.rkt")
(define-logger ocm)

#|
Totally monotone matrix searching algorithms.

The offline algorithm in ConcaveMinima is from Agarwal, Klawe, Moran,
Shor, and Wilbur, Geometric applications of a matrix searching algorithm,
Algorithmica 2, pp. 195-208 (1987).

The online algorithm in OnlineConcaveMinima is from Galil and Park,
A linear time algorithm for concave one-dimensional dynamic programming,
manuscript, 1989, which simplifies earlier work on the same problem
by Wilbur (J. Algorithms 1988) and Eppstein (J. Algorithms 1990).

D. Eppstein, March 2002, significantly revised August 2005

|#

;(provide smawky? make-ocm reduce reduce2 (prefix-out ocm- (combine-out min-value min-index)))

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
  (vector-append xs (vector value)))

(define-syntax-rule (vector-set vec idx val)
  (begin
    (vector-set! vec idx val)
    vec))

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



#|
    Search for the minimum value in each column of a matrix.
    The return value is a dictionary mapping ColIndices to pairs
    (value,rowindex). We break ties in favor of earlier rows.
    
    The matrix is defined implicitly as a function, passed
    as the third argument to this routine, where Matrix(i,j)
    gives the matrix value at row index i and column index j.
    The matrix must be concave, that is, satisfy the property
        Matrix(i,j) > Matrix(i',j) => Matrix(i,j') > Matrix(i',j')
    for every i<i' and j<j'; that is, in every submatrix of
    the input matrix, the positions of the column minima
    must be monotonically nondecreasing.
    
    The rows and columns of the matrix are labeled by the indices
    given in order by the first two arguments. In most applications,
    these arguments can simply be integer ranges.
|#

;; The return value `minima` is a hash:
;; the keys are col-indices (integers)
;; the values are pairs of (value row-index).
(: concave-minima (((Vectorof Index-Type)) ((Vectorof Index-Type) Matrix-Proc-Type Entry->Value-Type) . ->* . HashTableTop))
(define (concave-minima row-indices [col-indices (vector)] [matrix-proc (cast (make-caching-proc identity) Matrix-Proc-Type)] [entry->value (cast identity Entry->Value-Type)])
  ;((vector?) ((or/c #f vector?) procedure? procedure?) . ->* . hash?)
  (define reduce-proc reduce2)
  (define interpolate-proc interpolate2)
  (if (= 0 (vector-length col-indices)) 
      (make-hash)
      (let ([row-indices (reduce-proc row-indices col-indices matrix-proc entry->value)])
        (define odd-column-minima (concave-minima row-indices (cast (vector-odd-elements (cast col-indices (Vectorof Any))) (Vectorof Index-Type)) matrix-proc entry->value))
        (interpolate-proc (cast odd-column-minima (HashTable Any Any)) row-indices col-indices matrix-proc entry->value))))


#|
    Online concave minimization algorithm of Galil and Park.
    
    OnlineConcaveMinima(Matrix,initial) creates a sequence of pairs
    (self.value(j),self.index(j)), where
        self.value(0) = initial,
        self.value(j) = min { Matrix(i,j) | i < j } for j > 0,
    and where self.index(j) is the value of j that provides the minimum.
    Matrix(i,j) must be concave, in the same sense as for ConcaveMinima.
    
    We never call Matrix(i,j) until value(i) has already been computed,
    so that the Matrix function may examine previously computed values.
    Calling value(i) for an i that has not yet been computed forces
    the sequence to be continued until the desired index is reached.
    Calling iter(self) produces a sequence of (value,index) pairs.
    
    Matrix(i,j) should always return a value, rather than raising an
    exception, even for j larger than the range we expect to compute.
    If j is out of range, a suitable value to return that will not
    violate concavity is Matrix(i,j) = -i.  It will not work correctly
    to return a flag value such as None for large j, because the ties
    formed by the equalities among such flags may violate concavity.
|#

;; Online Concave Minima object
;(struct $ocm  (values indices finished matrix-proc base tentative) #:transparent #:mutable)

;; State used by self.value(), self.index(), and iter(self) =
;; $ocm-values, $ocm-indices, $ocm-finished

#|
State used by the internal algorithm:
$ocm-matrix, $ocm-base, $ocm-tentative

We allow self._values to be nonempty for indices > finished,
keeping invariant that
(1) self._values[i] = Matrix(self._indices[i], i),
(2) if the eventual correct value of self.index(i) < base,
    then self._values[i] is nonempty and correct.

In addition, we keep a column index self._tentative, such that
(3) if i <= tentative, and the eventual correct value of
    self.index(i) <= finished, then self._values[i] is correct.
|#


(define no-value 'none)

(define-syntax-rule (@ hashtable key)
  (hash-ref hashtable key))

(define-syntax-rule (! hashtable key value)
  (hash-set! hashtable key value))

(: ocm-ref (OCM-Type Index-Type . -> . Any)) 
(define (ocm-ref ocm key)
  (vector-ref ocm key))

(: ocm-set! (OCM-Type Index-Type Any . -> . Void)) 
(define (ocm-set! ocm key value)
  (vector-set! ocm key value))

(define-type OCM-Type (Vector Any Any Any Any Any Any Any))
(define-type Index-Type Nonnegative-Integer)
(define-type Matrix-Proc-Type (Index-Type Index-Type . -> . Any))
(define-type Entry->Value-Type (Any . -> . Flonum))
(define-type Indices-Type (Vectorof Index-Type))

 
(define o:min-values 0)
(define o:min-row-indices 1)
(define o:finished 2)
(define o:matrix-proc 3)
(define o:entry->value 4)
(define o:base 5)
(define o:tentative 6)

(: make-ocm ((Procedure) (Any Procedure) . ->* . OCM-Type))
(define (make-ocm matrix-proc [initial-value 0][entry->value identity])
  (log-ocm-debug "making new ocm")
  (define ocm (cast (make-vector 7) OCM-Type))
  (ocm-set! ocm o:min-values (vector initial-value))
  (ocm-set! ocm o:min-row-indices (vector no-value))
  (ocm-set! ocm o:finished 0)
  (ocm-set! ocm o:matrix-proc (make-caching-proc matrix-proc))
  (ocm-set! ocm o:entry->value entry->value) ; for converting matrix values to an integer
  (ocm-set! ocm o:base 0)
  (ocm-set! ocm o:tentative 0)
  (cast ocm OCM-Type))

;; Return min { Matrix(i,j) | i < j }.
(: min-value (OCM-Type Index-Type . -> . Any))
(define (min-value ocm j)
  (if (< (cast (ocm-ref ocm o:finished) Real) j)
      (begin (advance! ocm) (min-value ocm j))
      (vector-ref (cast (ocm-ref ocm o:min-values) VectorTop) j)))

;; Return argmin { Matrix(i,j) | i < j }.
(: min-index (OCM-Type Index-Type . -> . Index-Type))
(define (min-index ocm j)
  (if (< (cast (ocm-ref ocm o:finished) Real) j)     
      (begin (advance! ocm) (min-index ocm j))
      ((inst vector-ref Index-Type) (cast (ocm-ref ocm o:min-row-indices) (Vectorof Index-Type)) j)))

;; Finish another value,index pair.
(: advance! (OCM-Type . -> . Void))
(define (advance! ocm)
  (define next (add1 (cast (ocm-ref ocm o:finished) Index-Type)))  
  (log-ocm-debug "advance! ocm to next = ~a" (add1 (cast (ocm-ref ocm o:finished) Number)))
  (cond
    ;; First case: we have already advanced past the previous tentative
    ;; value.  We make a new tentative value by applying ConcaveMinima
    ;; to the largest square submatrix that fits under the base.
    [(> next (cast (ocm-ref ocm o:tentative) Real))
     (log-ocm-debug "advance: first case because next (~a) > tentative (~a)" next (ocm-ref ocm o:tentative))
     (define rows : (Vectorof Index-Type) (list->vector (range (cast (ocm-ref ocm o:base) Index-Type) next)))
     (ocm-set! ocm o:tentative (+ (cast (ocm-ref ocm o:finished) Number) (vector-length rows)))
     (define cols : (Vectorof Index-Type) (list->vector (range next (add1 (cast (ocm-ref ocm o:tentative) Index-Type)))))
     (define minima (concave-minima rows cols (cast (ocm-ref ocm o:matrix-proc) Matrix-Proc-Type) (cast (ocm-ref ocm o:entry->value) Entry->Value-Type)))
     (for ([col (in-vector cols)])
       (cond
         [(>= col (vector-length (cast (ocm-ref ocm o:min-values) VectorTop)))
          (ocm-set! ocm o:min-values (vector-append-item (ocm-ref ocm o:min-values) (@ (cast (@ minima col) HashTableTop) 'value)))
          (ocm-set! ocm o:min-row-indices (vector-append-item (ocm-ref ocm o:min-row-indices) (@ (@ minima col) 'row-idx)))]
         [(< ((ocm-ref ocm o:entry->value) (@ (@ minima col) 'value)) ((ocm-ref ocm o:entry->value) (vector-ref (ocm-ref ocm o:min-values) col)))
          (ocm-set! ocm o:min-values (vector-set (ocm-ref ocm o:min-values) col (@ (@ minima col) 'value)))
          (ocm-set! ocm o:min-row-indices (vector-set (ocm-ref ocm o:min-row-indices) col (@ (@ minima col) 'row-idx)))]))
     (ocm-set! ocm o:finished next)]
    
    [else
     ;; Second case: the new column minimum is on the diagonal.
     ;; All subsequent ones will be at least as low,
     ;; so we can clear out all our work from higher rows.
     ;; As in the fourth case, the loss of tentative is
     ;; amortized against the increase in base.
     (define diag ((ocm-ref ocm o:matrix-proc) (sub1 next) next))
     (cond
       [(< ((ocm-ref ocm o:entry->value) diag) ((ocm-ref ocm o:entry->value) (vector-ref (ocm-ref ocm o:min-values) next)))
        (log-ocm-debug "advance: second case because column minimum is on the diagonal")
        (ocm-set! ocm o:min-values (vector-set (ocm-ref ocm o:min-values) next diag))
        (ocm-set! ocm o:min-row-indices (vector-set (ocm-ref ocm o:min-row-indices) next (sub1 next)))
        (ocm-set! ocm o:base (sub1 next))
        (ocm-set! ocm o:tentative next)
        (ocm-set! ocm o:finished next)]
       
       ;; Third case: row i-1 does not supply a column minimum in
       ;; any column up to tentative. We simply advance finished
       ;; while maintaining the invariant.
       [(>= ((ocm-ref ocm o:entry->value) ((ocm-ref ocm o:matrix-proc) (sub1 next) (ocm-ref ocm o:tentative)))
            ((ocm-ref ocm o:entry->value) (vector-ref (ocm-ref ocm o:min-values) (ocm-ref ocm o:tentative))))
        (log-ocm-debug "advance: third case because row i-1 does not suppply a column minimum")
        (ocm-set! ocm o:finished next)]
       
       ;; Fourth and final case: a new column minimum at self._tentative.
       ;; This allows us to make progress by incorporating rows
       ;; prior to finished into the base.  The base invariant holds
       ;; because these rows cannot supply any later column minima.
       ;; The work done when we last advanced tentative (and undone by
       ;; this step) can be amortized against the increase in base.
       [else
        (log-ocm-debug "advance: fourth case because new column minimum")
        (ocm-set! ocm o:base (sub1 next))
        (ocm-set! ocm o:tentative next)
        (ocm-set! ocm o:finished next)])]))