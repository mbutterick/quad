#lang typed/racket/base
(require (for-syntax racket/base racket/syntax))
(require sugar/list sugar/debug racket/list racket/function math/flonum racket/vector math/statistics)
(require "ocm-typed.rkt" "quads-typed.rkt" "utils-typed.rkt" "measure-typed.rkt" "world-typed.rkt" "logger-typed.rkt")


(define-syntax (define/typed stx)
  (syntax-case stx ()
    [(_ (proc-name arg ... . rest-arg) type-expr body ...)
     #'(define/typed proc-name type-expr
         (λ(arg ... . rest-arg) body ...))]
    [(_ proc-name type-expr body ...)
     #'(begin
         (: proc-name type-expr)
         (define proc-name body ...))]))

;; predicate for the soft hyphen
(define/typed (soft-hyphen? x)
  (String . -> . Boolean)
  (equal? (format "~a" world:soft-hyphen) x))

;; visible characters that also mark possible breakpoints
(define/typed (visible-breakable? x)
  (String . -> . Boolean)
  (and (member x world:hyphens-and-dashes) #t))

;; invisible characters that denote possible breakpoints
(define/typed (invisible-breakable? x)
  (String . -> . Boolean)
  (and (member x (cons world:empty-string world:spaces)) #t))

;; union of visible & invisible
(define/typed (breakable? x)
  (Any . -> . Boolean)
  (cond
    [(string? x) (or (visible-breakable? x) (invisible-breakable? x))]
    [(word? x) (breakable? (word-string (cast x Quad)))]
    [else #f]))

;; used by insert-spacers to determine which characters 
;; can be surrounded by stretchy spacers 
(define/typed (takes-justification-space? x)
  (Any . -> . Boolean)
  (whitespace/nbsp? x))

;; test if a quad can be a word break:
;; either it's an explicit word break,
;; or it's breakable (and can be converted to a word break)
(define/typed (possible-word-break-quad? q)
  (Quad . -> . Boolean)
  (or (word-break? q) (breakable? q)))


;; convert a possible word break into an actual one
(define/typed (convert-to-word-break q)
  (Quad . -> . Quad)
  (when (not (possible-word-break-quad? q))
    (error 'convert-to-word-break "input is not a possible word break:" q))
  (define result (cond 
    [(word-break? q) q]
    [(word? q)
     (define str (word-string q)) ; str will be one character long, because we've exploded our input
     (apply word-break
            (merge-attrs q ; take q's attributes for formatting purposes
                         (cond
                           ;; a space is ordinarily visible, but disappears at the end of a line
                           [(equal? str " ") (list world:no-break-key " " world:before-break-key "")]
                           ;; soft hyphen is ordinarily invisible, but appears at the end of a line
                           [(soft-hyphen? str) (list world:no-break-key "" world:before-break-key "-")]
                           ;; a visible breakable character is always visible
                           [(visible-breakable? str) (list world:no-break-key str world:before-break-key str)]
                           [else (cast (world:default-word-break-list) HashableList)])) (quad-list q))]
    [else #f]))
  (or result (error 'convert-to-word-break "result was a not word break for input:" q)))