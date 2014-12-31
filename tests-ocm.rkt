#lang racket/base
(require racket/list racket/function rackunit "ocm.rkt" sugar)


(define (vector-range n)
  (build-vector n identity))

(define (random-ascending len start finish)
  (if (= len 0)
      null
      (let ([r (+ start (random (- finish start)))])
        (cons r (random-ascending (sub1 len) r finish)))))


(define (concave-list len min [lowval 0][highval 100])
  (append (reverse (random-ascending min lowval highval)) (random-ascending (- len min) lowval highval)))


(define (make-matrix2 rows cols)
  (define row-minima-indexes (random-ascending rows 0 cols))
  (reverse (for/list ([row-minima-index (in-list row-minima-indexes)])
             (concave-list cols row-minima-index 20 400))))


(define (make-matrix rows cols)
  (define seed (for/list ([i (in-range (max rows cols))])
                 (random 100)))
  (for/list ([i (in-range rows)])
    (for/list ([j (in-range cols)])
      (if (< i j)
          (apply + (sublist seed i (add1 j)))
          (apply + (sublist seed j (add1 i)))))))


(define (make-matrix-proc m [is (range (length m))] [js (range (length (car m)))])
  (let ([ipairs (apply hash (flatten (map cons is (range (length is)))))]
        [jpairs (apply hash (flatten (map cons js (range (length js)))))])
    (λ(i j)
      (define my-i (hash-ref ipairs i))
      (define my-j (hash-ref jpairs j))
      (with-handlers [(exn:fail? (λ(exn) (* -1 i)))]
        (list-ref (list-ref m my-i) my-j)))))


(define (compare-reductions m)
  (check-equal? 
   (reduce2 (vector-range (length m)) (vector-range (length (car m))) (make-matrix-proc m) identity)
   (reduce (vector-range (length m)) (vector-range (length (car m))) (make-matrix-proc m) identity)))


(define (do-it x)
  (repeat x
          (define rows (+ 2 (random 40)))
          (define cols (+ 2 (random rows)))
          (define m (make-matrix rows cols))
          (check-true (smawky? m))
          (compare-reductions m)))

(define me '((25 21 13 10 20 13 19 35 37 41 58 66 82 99 124 133 156 178) (42 35 26 20 29 21 25 37 36 39 56 64 76 91 116 125 146 164) (57 48 35 28 33 24 28 40 37 37 54 61 72 83 107 113 131 146) (78 65 51 42 44 35 38 48 42 42 55 61 70 80 100 106 120 135) (90 76 58 48 49 39 42 48 39 35 47 51 56 63 80 86 97 110) (103 85 67 56 55 44 44 49 39 33 41 44 49 56 71 75 84 96) (123 105 86 75 73 59 57 62 51 44 50 52 55 59 72 74 80 92) (142 123 100 86 82 65 61 62 50 43 47 45 46 46 58 59 65 73) (151 130 104 88 80 59 52 49 37 29 29 24 23 20 28 25 31 39)))



(define (bug-test bugmatrix bugrows bugcols)
  (define bugproc (make-matrix-proc bugmatrix bugrows bugcols))
  (check-equal? (reduce (list->vector bugrows) (list->vector bugcols) bugproc identity)
  (reduce2 (list->vector bugrows) (list->vector bugcols) bugproc identity)))


(bug-test '((19496.0 14025.0 7134.0 5027.0) (108793.0 102427.0 93819.0 90268.0) (101409.0 93357.0 81509.0 75236.0) (106662.0 93357.0 71417.0 56665.0))
          '(0 1 2 3)
          '(4 5 6 7))


(bug-test '((25 42 57 78 90 103 123 142 151) (21 35 48 65 76 85 105 123 130) (13 26 35 51 58 67 86 100 104) (10 20 28 42 48 56 75 86 88) (20 29 33 44 49 55 73 82 80) (13 21 24 35 39 44 59 65 59) (19 25 28 38 42 44 57 61 52) (35 37 40 48 48 49 62 62 49) (37 36 37 42 39 39 51 50 37) (41 39 37 42 35 33 44 43 29) (58 56 54 55 47 41 50 47 29) (66 64 61 61 51 44 52 45 24) (82 76 72 70 56 49 55 46 23) (99 91 83 80 63 56 59 46 20) (124 116 107 100 80 71 72 58 28) (133 125 113 106 86 75 74 59 25) (156 146 131 120 97 84 80 65 31) (178 164 146 135 110 96 92 73 39))
          '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17)
          '(0 1 2 3 4 5 6 7 8))


(do-it 10)