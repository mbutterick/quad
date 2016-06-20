#lang quad/dev
(provide (all-defined-out))
(require "measure.rkt")

(define last-bp-k #f)
(define line-measure 80)
(define col-measure 150)
(define page-measure 300)

(struct tp (page col line) #:transparent)

(define (increment-tpos tpos page col line)
  (tp (+ (tp-page tpos) page) (+ (tp-col tpos) col) (+ (tp-line tpos) line)))

(define (typeset qs)
  (for/fold ([tpos (tp 0 0 0)])
            ([q (in-vector qs)])
    (cond
      [(not (quad-dim q)) ; fit pass
       (measure! q)
       (cond
         [(> (tp-page tpos) page-measure) (last-bp-k 'break-page)]
         [(> (tp-col tpos) col-measure) (last-bp-k 'break-col)]
         [(> (tp-line tpos) line-measure) (last-bp-k 'break-line)]
         [(and ($white? q) (let/cc bp-k (set! last-bp-k bp-k) #f))
          =>
          (λ(k-result)
            (quad-dim-set! q k-result)
            (case k-result
              [(break-line) (tp (tp-page tpos) (tp-col tpos) 0)]
              [(break-col)  (tp (tp-page tpos) 0 0)]
              [(break-page) (tp 0 0 0)]))]
         [else (define qpos (quad-dim q))
               (increment-tpos tpos qpos qpos qpos)])]
      [else ; fill pass
       tpos]))
  qs)

(module+ test
  (require "atomize.rkt")
  (time (typeset (atomize (quad #f "Meg is an ally. Meg is an ally. Meg is an ally. Meg is an ally. Meg is an ally. Meg is an ally.")))))