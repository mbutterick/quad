#lang typed/racket
(require typed/rackunit)
(require "wrap-typed.rkt" "utils-typed.rkt" "quads-typed.rkt")


(define megs (split-quad (block '(size 10 font "Courier") "Meg is an ally.")))
(define measure 40.0)
(check-equal? (map quad->string (wrap-first megs measure)) '("Meg is" "an" "ally."))
(check-equal? (map quad->string (wrap-best megs measure)) '("Meg" "is an" "ally."))


(define eqs (split-quad (block '(x-align center font "Equity Text B" size 10) "Foo-d" (word '(size 13) "og ") "and " (box) " Zu" (word-break '(nb "c" bb "k-")) "kerman's. Instead of a circle, the result is a picture of the code that, if it were used as an expression, would produce a circle. In other words, code is not a function, but instead a new syntactic form for creating pictures; the bit between the opening parenthesis with code is not an expression, but instead manipulated by the code syntactic form. This helps explain what we meant in the previous section when we said that racket provides require and the function-calling syntax. Libraries are not restricted to exporting values, such as functions; they can also define new syntactic forms. In this sense, Racket isn’t exactly a language at all; it’s more of an idea for how to structure a language so that you can extend it or create entirely " (word '(font "Courier" size 5) "lang."))))
  (set! measure 200.0)

;; these wrap results generated from untyped wrap.rkt
;; so a winning test is where typed and untyped agree
;; todo: make it easy to check results from both directions.
;; not as simple as importing untyped variants with a prefix.
;; the "quad" data structure itself is different.
;; so it would need macrology.

(check-equal? (map quad->string (wrap-first eqs measure)) '("Foo-dog and  Zuckerman's. Instead of a circle,"
  "the result is a picture of the code that, if it were"
  "used as an expression, would produce a circle. In"
  "other words, code is not a function, but instead a"
  "new syntactic form for creating pictures; the bit"
  "between the opening parenthesis with code is not"
  "an expression, but instead manipulated by the"
  "code syntactic form. This helps explain what we"
  "meant in the previous section when we said that"
  "racket provides require and the function-calling"
  "syntax. Libraries are not restricted to exporting"
  "values, such as functions; they can also define new"
  "syntactic forms. In this sense, Racket isn’t exactly"
  "a language at all; it’s more of an idea for how to"
  "structure a language so that you can extend it or"
  "create entirely lang."))
(check-equal? (map quad->string (wrap-best eqs measure)) '("Foo-dog and  Zuckerman's. Instead of a circle,"
  "the result is a picture of the code that, if it were"
  "used as an expression, would produce a circle. In"
  "other words, code is not a function, but instead"
  "a new syntactic form for creating pictures; the"
  "bit between the opening parenthesis with code is"
  "not an expression, but instead manipulated by the"
  "code syntactic form. This helps explain what we"
  "meant in the previous section when we said that"
  "racket provides require and the function-calling"
  "syntax. Libraries are not restricted to exporting"
  "values, such as functions; they can also define new"
  "syntactic forms. In this sense, Racket isn’t exactly"
  "a language at all; it’s more of an idea for how to"
  "structure a language so that you can extend it or"
  "create entirely lang."))