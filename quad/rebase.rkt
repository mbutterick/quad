#lang racket/base
(require racket/list)
(provide (all-defined-out))

(define (base-base digit-str
                   #:offset [offset 0]
                   #:min-width [min-width #f])
  (define digits (list->vector (string->list digit-str)))
  (define len (vector-length digits))
  (values
   (λ (num)
     (let loop ([num (+ num offset)] [acc null])
       (if (zero? num)
           (list->string
            (if (and min-width (< (length acc) min-width))
                (append (make-list (- min-width (length acc)) (vector-ref digits 0)) acc)
                acc))
           (let* ([r (modulo num len)]
                  [q (quotient (- num r) len)]) 
             (loop q (cons (vector-ref digits r) acc))))))
   (λ (str)
     (define digit-table (for/hash ([c (in-string digit-str)]
                                    [i (in-naturals)])
                                   (values c i)))
     (- (for/sum ([c (in-list (reverse (string->list str)))]
                  [i (in-naturals)])
                 (* (hash-ref digit-table c) (expt len i))) offset))))

(define-values (base62 unbase62)
  (base-base "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"))

(define-values (base-readable unbase-readable)
  (base-base "abcdefghjkmpqrtuvwxy2346789"))

(define-values (zbase32 unzbase32)
  (base-base "ybndrfg8ejkmcpqxot1uwisza345h769"))

(define-values (rfc4648 unrfc4648)
  (base-base "abcdefghijklmnopqrstuvwxyz234567"))

(define-values (no-vowel un-no-vowel)
  (base-base "bcdfghjklmnpqrstvwxz0123456789"))

(define-values (base32-uc unbase32-uc)
  (base-base "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"))

(define-values (base-uc unbase-uc)
  (base-base "ABCDEFGHIJKLMNOPQRSTUVWXYZ"))


(module+ test
  (require rackunit)
  (check-equal? (base62 10234) "2F4")
  (check-equal? (unbase62 "2F4") 10234))