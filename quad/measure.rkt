#lang quad/dev
(require "measure-chars.rkt")
(provide (all-defined-out))

(define (measure! q)
  (quad-dim-set! q
                  (cond
                    [(or ($black? q) ($white? q))
                     (* (measure-char (quad-font q) (quad-val q)) (quad-font-size q))]
                    [else 0])))

(module+ test
  (require rackunit)
  (define q ($black '#hasheq((size . 12) (font . "sc.otf")) 0 #\n))
  (check-equal? (measure-char (quad-font q) (quad-val q)) .6))
