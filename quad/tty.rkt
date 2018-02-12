#lang debug br/quicklang
(require racket/contract "qexpr.rkt")
(provide (except-out (all-from-out br/quicklang) #%module-begin)
         (rename-out [mb #%module-begin]))

(define/contract (render-tty qexpr)
  (qexpr? . -> . string?)
  (define page 0) (define row 0) (define col 0)
  (define (increment-page!) (set! page (add1 page)) page)
  (define (increment-row!) (set! row (add1 row)) row)
  (define (increment-col!) (set! col (add1 col)) col)
  (define strs
    (let loop ([x qexpr])
      (match x
        [`(char ,x) (list (format "move to col ~a" (increment-col!)) (format "draw char ~a" (char->integer (car (string->list x)))))]
        [`(line ,@xs) (list (format "move to row ~a" (increment-row!)) (map loop xs) (begin (set! col 0) #f) )]
        [`(page ,@xs) (list (format "move to page ~a" (increment-page!)) (map loop xs) (begin (set! row 0) #f))]
        [`(doc ,@xs) (map loop xs)])))
  (string-join (map string-downcase (filter values (flatten strs))) "\n"))

(module+ test
  (require rackunit)
  (define q '(doc
              (page (line (char "a") (char " ") (char "b")) (line (char "c") (char "d")))
              (page (line (char "e") (char "f")) (line (char "g") (char " ") (char "h")))))
  (check-equal? (render-tty q)
                "move to page 1
move to row 1
move to col 1
draw char 97
move to col 2
draw char 32
move to col 3
draw char 98
move to row 2
move to col 1
draw char 99
move to col 2
draw char 100
move to page 2
move to row 1
move to col 1
draw char 101
move to col 2
draw char 102
move to row 2
move to col 1
draw char 103
move to col 2
draw char 32
move to col 3
draw char 104"))
      
(define-macro (mb ARG)
  #'(#%module-begin
     (display (render-tty ARG))))

(module reader syntax/module-reader
  quad/tty
  #:read read
  #:read-syntax read-syntax
  (require racket/base))