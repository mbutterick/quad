#lang racket/base
(require (for-syntax racket/base racket/syntax))
(require racket/list sugar/debug rackunit racket/function racket/vector sugar/cache "logger.rkt")
(define-logger ocm)
;(activate-logger ocm-logger)


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

(provide smawky? make-ocm reduce reduce2 (prefix-out ocm- (combine-out min-value min-index)))

(define (select-elements xs is)
  (map (curry list-ref xs) is))

(define (odd-elements xs)
  (select-elements xs (range 1 (length xs) 2)))

(define (vector-odd-elements xs)
  (for/vector ([i (in-range (vector-length xs))] #:when (odd? i))
    (vector-ref xs i)))

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
(define (reduce row-indices col-indices matrix-proc entry->value)
  ;(vector? vector? procedure? procedure? . -> . vector?)
  (log-ocm-debug "starting reduce phase with")
  (log-ocm-debug "row-indices = ~a" row-indices)
  (log-ocm-debug "col-indices = ~a" col-indices)
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
    (for/fold ([stack (vector)]) ([row-idx (in-vector row-indices)])
      (let ([stack (process-stack stack row-idx)])
        (if (= (vector-length stack) (vector-length col-indices))
            stack
            (vector-append stack (vector row-idx))))))
  (log-ocm-debug "finished reduce. row indexes = ~v" reduced-row-indexes)
  reduced-row-indexes)


(define (reduce2 row-indices col-indices matrix-proc entry->value)
  (let find-survivors ([rows row-indices][survivors empty])
    (cond 
      [(vector-empty? rows) (list->vector (reverse survivors))]
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



(define (make-minimum value row-idx)
  (define ht (make-hash))
  (! ht 'value value)
  (! ht 'row-idx row-idx)
  ht)

;; Interpolate phase: in the minima hash, add results for even rows 

(define-syntax-rule (vector-last v)
  (vector-ref v (sub1 (vector-length v))))

(define (interpolate minima row-indices col-indices matrix-proc entry->value)
  ;(hash? vector? vector? procedure? procedure? . -> . hash?)
  (for ([col-idx (in-range 0 (vector-length col-indices) 2)]) ;; even-col-indices
    (define col (vector-ref col-indices col-idx))
    (define idx-of-last-row
      (if (= col-idx (sub1 (vector-length col-indices)))
          (vector-last row-indices)
          (: (hash-ref minima (vector-ref col-indices (add1 col-idx))) 'row-idx)))
    
    (define smallest-value-entry
      (vector-argmin (compose1 entry->value car)
                     (for/vector ([row-idx (in-list (dropf-right (vector->list row-indices) (negate (curry = idx-of-last-row))))])
                       (list (matrix-proc row-idx col) row-idx))))
    
    (! minima col (apply make-minimum smallest-value-entry)))
  minima)

(define (interpolate2 minima row-indices col-indices matrix-proc entry->value)
  (define idx-of-last-col (sub1 (vector-length col-indices)))
  (define (smallest-value-entry col idx-of-last-row)
    (argmin (compose1 entry->value car)
            (for/list ([row-idx (stop-after (in-vector row-indices) (curry = idx-of-last-row))])
              (list (matrix-proc row-idx col) row-idx))))
  
  (for ([(col col-idx) (in-indexed col-indices)] #:when (even? col-idx))
    (define idx-of-last-row (if (= col-idx idx-of-last-col)
                                (vector-last row-indices)
                                (: (hash-ref minima (vector-ref col-indices (add1 col-idx))) 'row-idx)))   
    (! minima col (apply make-minimum (smallest-value-entry col idx-of-last-row))))
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
(require rackunit)
(define (concave-minima row-indices [col-indices null] [matrix-proc (make-caching-proc identity)] [entry->value identity])
  ;((vector?) ((or/c #f vector?) procedure? procedure?) . ->* . hash?)
  (define reduce-proc reduce2)
  (define interpolate-proc interpolate2)
  (if (= 0 (vector-length col-indices)) 
      (make-hash)
      (let ([row-indices (reduce-proc row-indices col-indices matrix-proc entry->value)])
        (define odd-column-minima (concave-minima row-indices (vector-odd-elements col-indices) matrix-proc entry->value))
        (interpolate-proc odd-column-minima row-indices col-indices matrix-proc entry->value))))


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

(define-syntax-rule (: hashtable key)
  (hash-ref hashtable key))

(define-syntax-rule (! hashtable key value)
  (hash-set! hashtable key value))

(define-syntax-rule (ocm-ref ocm key)
  (vector-ref ocm key))

(define-syntax-rule (ocm-set! ocm key value)
  (vector-set! ocm key value))

(define o:min-values 0)
(define o:min-row-indices 1)
(define o:finished 2)
(define o:matrix-proc 3)
(define o:entry->value 4)
(define o:base 5)
(define o:tentative 6)

(define (make-ocm matrix-proc [initial-value 0][entry->value identity])
  (log-ocm-debug "making new ocm")
  (define ocm (make-vector 7))
  (ocm-set! ocm o:min-values (vector initial-value))
  (ocm-set! ocm o:min-row-indices (vector no-value))
  (ocm-set! ocm o:finished 0)
  (ocm-set! ocm o:matrix-proc (make-caching-proc matrix-proc))
  (ocm-set! ocm o:entry->value entry->value) ; for converting matrix values to an integer
  (ocm-set! ocm o:base 0)
  (ocm-set! ocm o:tentative 0)
  ocm)


;; Return min { Matrix(i,j) | i < j }.
(define (min-value ocm j)
  (if (< (ocm-ref ocm o:finished) j)
      (begin (advance! ocm) (min-value ocm j))
      (vector-ref (ocm-ref ocm o:min-values) j)))

;; Return argmin { Matrix(i,j) | i < j }.
(define (min-index ocm j)
  (if (< (ocm-ref ocm o:finished) j)     
      (begin (advance! ocm) (min-index ocm j))
      (vector-ref (ocm-ref ocm o:min-row-indices) j)))

;; Finish another value,index pair.
(define (advance! ocm)
  (define next (add1 (ocm-ref ocm o:finished)))  
  (log-ocm-debug "advance! ocm to next = ~a" (add1 (ocm-ref ocm o:finished)))
  (cond
    ;; First case: we have already advanced past the previous tentative
    ;; value.  We make a new tentative value by applying ConcaveMinima
    ;; to the largest square submatrix that fits under the base.
    [(> next (ocm-ref ocm o:tentative))
     (log-ocm-debug "advance: first case because next (~a) > tentative (~a)" next (ocm-ref ocm o:tentative))
     (define rows (list->vector (range (ocm-ref ocm o:base) next)))
     (ocm-set! ocm o:tentative (+ (ocm-ref ocm o:finished) (vector-length rows)))
     (define cols (list->vector (range next (add1 (ocm-ref ocm o:tentative)))))
     (define minima (concave-minima rows cols (ocm-ref ocm o:matrix-proc) (ocm-ref ocm o:entry->value)))
     (for ([col (in-vector cols)])
       (cond
         [(>= col (vector-length (ocm-ref ocm o:min-values)))
          (ocm-set! ocm o:min-values (vector-append-item (ocm-ref ocm o:min-values) (: (: minima col) 'value)))
          (ocm-set! ocm o:min-row-indices (vector-append-item (ocm-ref ocm o:min-row-indices) (: (: minima col) 'row-idx)))]
         [(< ((ocm-ref ocm o:entry->value) (: (: minima col) 'value)) ((ocm-ref ocm o:entry->value) (vector-ref (ocm-ref ocm o:min-values) col)))
          (ocm-set! ocm o:min-values (vector-set (ocm-ref ocm o:min-values) col (: (: minima col) 'value)))
          (ocm-set! ocm o:min-row-indices (vector-set (ocm-ref ocm o:min-row-indices) col (: (: minima col) 'row-idx)))]))
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

(define (print ocm)
  (displayln (ocm-ref ocm o:min-values))
  (displayln (ocm-ref ocm o:min-row-indices)))

(define (smawky? m)
  (define (position-of-minimum xs)
    ;; put each element together with its list index
    (let ([xs (map cons (range (length xs)) xs)]) 
      ;; find the first one with the min value, and grab the list index
      (car (argmin cdr (filter (compose1 not negative? cdr) xs)))))
  ;; tests if penalty matrix is monotone for non-negative values.
  (define increasing-minima? (apply <= (map position-of-minimum m)))
  (define monotone? 
    (for*/and ([ridx (in-range 1 (length m))]
               [cidx (in-range (sub1 (length (car m))))])
      (let* ([prev-row (list-ref m (sub1 ridx))]
             [row (list-ref m ridx)]
             [a (list-ref prev-row cidx)]
             [b (list-ref prev-row (add1 cidx))]
             [c (list-ref row cidx)]
             [d (list-ref row (add1 cidx))])
        (if (andmap (compose1 not negative?) (list a b c d)) ;; smawk disregards negative values
            (cond
              [(< c d) (if (< a b) #t (error (format "Submatrix ~a not monotone in ~a" (list (list a b) (list c d)) m)))]
              [(= c d) (if (<= a b) #t (error (format "Submatrix ~a not monotone in ~a" (list (list a b) (list c d)) m)))]
              [else #t])
            #t))))
  (and increasing-minima? monotone?))


(module+ test
  
  (require rackunit)
  
  
  (define m '((25 42 57 78 90 103 123 142 151)
              (21 35 48 65 76 85 105 123 130)
              (13 26 35 51 58 67 86 100 104)
              (10 20 28 42 48 56 75 86 88)
              (20 29 33 44 49 55 73 82 80)
              (13 21 24 35 39 44 59 65 59)
              (19 25 28 38 42 44 57 61 52)
              (35 37 40 48 48 49 62 62 49)
              (37 36 37 42 39 39 51 50 37)
              (41 39 37 42 35 33 44 43 29)
              (58 56 54 55 47 41 50 47 29)
              (66 64 61 61 51 44 52 45 24)
              (82 76 72 70 56 49 55 46 23)
              (99 91 83 80 63 56 59 46 20)
              (124 116 107 100 80 71 72 58 28)
              (133 125 113 106 86 75 74 59 25)
              (156 146 131 120 97 84 80 65 31)
              (178 164 146 135 110 96 92 73 39)))
  (define m2 (apply map list m))
  (check-true (smawky? m))
  (check-true (smawky? m2))
  ;; proc must return a value even for out-of-bounds i and j
  (define (simple-proc i j) (with-handlers [(exn:fail? (λ(exn) (* -1 i)))]
                              (list-ref (list-ref m i) j))) 
  (define (simple-proc2 i j) (with-handlers [(exn:fail? (λ(exn) (* -1 i)))]
                               (list-ref (list-ref m2 i) j))) 
  (check-equal? (simple-proc 0 2) 57) ; 0th row, 2nd col
  (check-equal? (simple-proc2 2 0) 57) ; flipped
  (define o (make-ocm simple-proc))
  (define row-indices (list->vector (range (length m))))
  (define col-indices (list->vector (range (length (car m)))))
  (define result (concave-minima row-indices col-indices simple-proc identity))
  (check-equal?
   (for/list ([j (in-vector col-indices)])
     (define h (hash-ref result j))
     (list (hash-ref h 'value) (hash-ref h 'row-idx)))
   '((10 3) (20 3) (24 5) (35 5) (35 9) (33 9) (44 9) (43 9) (20 13))) ; checked against SMAWK.py
  (check-equal?
   (for/list ([j (in-vector col-indices)])
     (list (min-value o j) (min-index o j)))
   '((0 none) (42 0) (48 1) (51 2) (48 3) (55 4) (59 5) (61 6) (49 7))) ; checked against SMAWK.py
  
  (define o2 (make-ocm simple-proc2))
  (define row-indices2 (list->vector (range (length m2))))
  (define col-indices2 (list->vector (range (length (car m2)))))
  (define result2 (concave-minima row-indices2 col-indices2 simple-proc2 identity))
  (check-equal?
   (for/list ([j (in-vector col-indices2)])
     (define h (hash-ref result2 j))
     (list (hash-ref h 'value) (hash-ref h 'row-idx)))
   '((25 0) (21 0) (13 0) (10 0) (20 0) (13 0) (19 0) (35 0) (36 1) (29 8) (29 8) (24 8) (23 8) (20 8) (28 8) (25 8) (31 8) (39 8))) ; checked against SMAWK.py
  (check-equal?
   (for/list ([j (in-vector col-indices2)])
     (list (min-value o2 j) (min-index o2 j)))
   '((0 none) (21 0) (13 0) (10 0) (20 0) (13 0) (19 0) (35 0) (36 1) (29 8) (-9 9) (-10 10) (-11 11) (-12 12) (-13 13) (-14 14) (-15 15) (-16 16))) ; checked against SMAWK.py
  
  )