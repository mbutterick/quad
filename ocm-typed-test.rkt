#lang typed/racket
(require "ocm-typed.rkt")
(require typed/rackunit)
(require math)

(define m0 (matrix ((25.0 42.0 57.0 78.0 90.0 103.0 123.0 142.0 151.0)
                    (21.0 35.0 48.0 65.0 76.0 85.0 105.0 123.0 130.0)
                    (13.0 26.0 35.0 51.0 58.0 67.0 86.0 100.0 104.0)
                    (10.0 20.0 28.0 42.0 48.0 56.0 75.0 86.0 88.0)
                    (20.0 29.0 33.0 44.0 49.0 55.0 73.0 82.0 80.0)
                    (13.0 21.0 24.0 35.0 39.0 44.0 59.0 65.0 59.0)
                    (19.0 25.0 28.0 38.0 42.0 44.0 57.0 61.0 52.0)
                    (35.0 37.0 40.0 48.0 48.0 49.0 62.0 62.0 49.0)
                    (37.0 36.0 37.0 42.0 39.0 39.0 51.0 50.0 37.0)
                    (41.0 39.0 37.0 42.0 35.0 33.0 44.0 43.0 29.0)
                    (58.0 56.0 54.0 55.0 47.0 41.0 50.0 47.0 29.0)
                    (66.0 64.0 61.0 61.0 51.0 44.0 52.0 45.0 24.0)
                    (82.0 76.0 72.0 70.0 56.0 49.0 55.0 46.0 23.0)
                    (99.0 91.0 83.0 80.0 63.0 56.0 59.0 46.0 20.0)
                    (124.0 116.0 107.0 100.0 80.0 71.0 72.0 58.0 28.0)
                    (133.0 125.0 113.0 106.0 86.0 75.0 74.0 59.0 25.0)
                    (156.0 146.0 131.0 120.0 97.0 84.0 80.0 65.0 31.0)
                    (178.0 164.0 146.0 135.0 110.0 96.0 92.0 73.0 39.0))))
(define m (matrix->list* m0))
(define m2 (matrix->list* (matrix-transpose m0)))


(check-true (smawky? m))
(check-true (smawky? m2))

(: simple-entry->value Entry->Value-Type)
(define (simple-entry->value e)
  (fl (cast e Real)))

;; proc must return a value even for out-of-bounds i and j
(: simple-proc Matrix-Proc-Type)
(define (simple-proc i j) (cast (fl (with-handlers [(exn:fail? (λ(exn) (* -1 i)))]
                                      ((inst list-ref Value-Type) ((inst list-ref (Listof Value-Type)) m i) j))) Value-Type))
(: simple-proc2 Matrix-Proc-Type)
(define (simple-proc2 i j) (cast (fl (with-handlers [(exn:fail? (λ(exn) (* -1 i)))]
                                       ((inst list-ref Value-Type) ((inst list-ref (Listof Value-Type)) m2 i) j))) Value-Type)) 
(check-equal? (simple-proc 0 2) 57.0) ; 0th row, 2nd col
(check-equal? (simple-proc2 2 0) 57.0) ; flipped

(define row-indices (cast (list->vector (range (length m))) (Vectorof Index-Type)))
(define col-indices (cast (list->vector (range (length (car m)))) (Vectorof Index-Type)))
(define result (concave-minima row-indices col-indices simple-proc simple-entry->value))


(check-equal?
 (for/list : (Listof (List (U Index-Type Value-Type) (U Index-Type Value-Type))) ([j (in-vector col-indices)])
   (define h (cast (hash-ref result j) (HashTable Symbol (U Index-Type Value-Type))))
   (list (hash-ref h 'value) (hash-ref h 'row-idx)))
 '((10.0 3) (20.0 3) (24.0 5) (35.0 5) (35.0 9) (33.0 9) (44.0 9) (43.0 9) (20.0 13))) ; checked against SMAWK.py

(check-equal?
 (for/list : (Listof (List Value-Type Index-Type)) ([j (in-vector col-indices)])
   (define h (cast (hash-ref result j) (HashTable Symbol Any)))
   (list (cast (hash-ref h 'value) Value-Type) (cast (hash-ref h 'row-idx) Index-Type)))
 '((10.0 3) (20.0 3) (24.0 5) (35.0 5) (35.0 9) (33.0 9) (44.0 9) (43.0 9) (20.0 13))) ; checked against SMAWK.py



(define o (make-ocm simple-proc simple-entry->value))

  (check-equal?
   (for/list : (Listof (List Value-Type (U Index-Type No-Value-Type))) ([j (in-vector col-indices)])
     (list (cast (ocm-min-value o j) Value-Type) (ocm-min-index o j)))
   '((0.0 none) (42.0 0) (48.0 1) (51.0 2) (48.0 3) (55.0 4) (59.0 5) (61.0 6) (49.0 7))) ; checked against SMAWK.py


  (define row-indices2 (cast (list->vector (range (length m2))) (Vectorof Index-Type)))
  (define col-indices2 (cast (list->vector (range (length (car m2)))) (Vectorof Index-Type)))
  (define result2 (concave-minima row-indices2 col-indices2 simple-proc2 simple-entry->value))
  (check-equal?
   (for/list : (Listof (List Value-Type Index-Type)) ([j (in-vector col-indices2)])
     (define h (cast (hash-ref result2 j) (HashTable Symbol (U Index-Type Value-Type))))
     (list (cast (hash-ref h 'value) Value-Type) (cast (hash-ref h 'row-idx) Index-Type)))
   '((25.0 0) (21.0 0) (13.0 0) (10.0 0) (20.0 0) (13.0 0) (19.0 0) (35.0 0) (36.0 1) (29.0 8) (29.0 8) (24.0 8) (23.0 8) (20.0 8) (28.0 8) (25.0 8) (31.0 8) (39.0 8))) ; checked against SMAWK.py

(define o2 (make-ocm simple-proc2 simple-entry->value))
  (check-equal?
   (for/list : (Listof (List Value-Type (U Index-Type No-Value-Type))) ([j (in-vector col-indices2)])
     (list (cast (ocm-min-value o2 j) Value-Type) (ocm-min-index o2 j)))
   '((0.0 none) (21.0 0) (13.0 0) (10.0 0) (20.0 0) (13.0 0) (19.0 0) (35.0 0) (36.0 1) (29.0 8) (-9.0 9) (-10.0 10) (-11.0 11) (-12.0 12) (-13.0 13) (-14.0 14) (-15.0 15) (-16.0 16))) ; checked against SMAWK.py