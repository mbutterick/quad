#lang quad/dev
(provide (all-defined-out))
(require "measure.rkt")

(define last-bp-k #f)
(define line-measure 80)
(define col-measure 150)
(define page-measure 300)

(define (typeset qs)
  (for*/fold ([page-pos 0]
              [col-pos 0]
              [line-pos 0])
             ([q (in-vector qs)])
    (cond
      [(not (quad-dim q)) ; fit pass
       (measure! q)
       (cond
         [(> page-pos page-measure) (last-bp-k 'break-page)]
         [(> col-pos col-measure) (last-bp-k 'break-col)]
         [(> line-pos line-measure) (last-bp-k 'break-line)]
         [(and ($white? q) (let/cc bp-k (set! last-bp-k bp-k) #f))
          =>
          (λ(k-result)
            (quad-dim-set! q k-result)
            (case k-result
              [(break-line) (values page-pos col-pos 0)]
              [(break-col)  (values page-pos 0 0)]
              [(break-page) (values 0 0 0)]))]
         [else (define qpos (quad-dim q))
               (values (+ page-pos qpos) (+ col-pos qpos) (+ line-pos qpos))])]
      [else ; fill pass
       (values page-pos col-pos line-pos)]))
  qs)

(module+ test
  (require "atomize.rkt")
  (time (typeset (atomize (quad #f "Meg is an ally. Meg is an ally. Meg is an ally. Meg is an ally. Meg is an ally. Meg is an ally.")))))