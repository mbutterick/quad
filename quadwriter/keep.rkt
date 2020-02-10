#lang debug racket
(require quad/quad
         quad/util
         "attrs.rkt"
         "struct.rkt")
(provide (all-defined-out))



(define (make-nobreak! q) (quad-set! q :no-colbr #true)) ; scooperates with col-wrap

(define (do-keep-with-next! reversed-lines)
  ;; paints nobreak onto the kwn line itself,
  ;; and any line spacers that follow (could be one or more)
  ;; (we are iterating backward, so the geometrically previous ln follows the spacer)
  (define (is-kwn-line? ln) (quad-ref ln :keep-with-next))
  (let loop ([lines (reverse reversed-lines)])
    (unless (null? lines)
      (match lines
        [(list* (? is-kwn-line? kwn) (? line-spacer-quad? lsqs) ..1 rest)
         (for-each make-nobreak! (cons kwn lsqs))
         (loop rest)]
        [(cons ln rest) (loop rest)]))))

(define (apply-keeps lines)
  (define groups-of-lines (contiguous-group-by (λ (x) (quad-ref x :display)) lines))
  (for*/fold ([reversed-lines null]
              #:result (begin
                         (do-keep-with-next! reversed-lines)
                         (reverse reversed-lines)))
             ([group (in-list groups-of-lines)]
              [group-len (in-value (length group))]
              [(ln idx0) (in-indexed group)])
    (define idx (add1 idx0))
    ;; always catch last line of block in this case
    ;; so later cases are guaranteed to have earlier lines.
    (define keep-first (quad-ref ln :keep-first-lines))
    (define keep-last (quad-ref ln :keep-last-lines))
    (unless (eq? idx group-len)
      (when (or
             ;; if we have keep all we can skip :keep-first and :keep-last cases
             (or (equal? keep-first "all") (equal? keep-last "all"))
             ;; to keep n lines, we only paint the first n - 1
             ;; (because each nobr line sticks to the next)
             (and (number? keep-first) (< idx keep-first))
             (and (number? keep-last) (< (- group-len keep-last) idx)))
        (make-nobreak! ln)))
    (cons ln reversed-lines)))