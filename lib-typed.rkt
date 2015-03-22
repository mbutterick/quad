#lang typed/racket/base
(provide (all-defined-out))

;; Typed versions of common library functions, to avoid require/typed

(: empty? (Any . -> . Boolean : Null))
(define (empty? l) 
  (null? l))

(: empty Null)
(define empty '())

#;(: flatten (Any . -> . (Listof Any)))
#;(define (flatten orig-sexp)
  (let loop ([sexp orig-sexp] [acc null])
    (cond [(null? sexp) acc]
          [(pair? sexp) (loop (car sexp) (loop (cdr sexp) acc))]
          [else (cons sexp acc)])))