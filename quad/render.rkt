#lang quad/dev
(require racket/format)
(provide (all-defined-out))

(define (debug-render qs)
  (define line-counter #f)
  (define (line-counter-increment!)  (set! line-counter (add1 line-counter)))
  (define (line-counter-reset!) (set! line-counter 1))
  (line-counter-reset!)
  (printf "   ")
  (for ([i (in-range 1 71)])
       (printf (cond
                 [(zero? (modulo i 10)) "|"]
                 [(zero? (modulo i 5)) "'"]
                 [else "·"])))
  (define (print-line-counter)
    (printf "\n~a "(~r line-counter #:min-width 2 #:pad-string " " #:base 10)))
  (print-line-counter)
  (for ([q (in-vector qs)])
       (define qd (quad-dim q))
       (cond
         [(symbol? qd)
          (case qd
            [(line-break) (line-counter-increment!)]
            [(column-break)  (line-counter-reset!) (printf "\n--col--")]
            [(page-break) (printf "\n\n==page==\n")])
          (print-line-counter)]
         [(or ($black? q) ($soft? q)) (printf "~a" (quad-val q))]
         [else (void)]))
  (printf "\n\n"))