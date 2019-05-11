#lang debug racket/base
(require  (for-syntax racket/base) racket/match racket/string)

(module+ reader
  (require syntax/strip-context)
  (provide (rename-out [rs read-syntax]))
  (define (rs name ip)
    (define lines
      (for*/list ([line (in-lines ip)]
                  [str (in-value (string-trim (string-trim line #px"#.*" #:left? #false)))]
                  #:when (non-empty-string? str))
        (match-define (list* codepoints tag _) (string-split str ";"))
        ;; codepoints might be a single value, an x..y range, or two values (base value and modifier)
        ;; we want to ignore the modifier
        (define cp-or-range (match (string-split (string-trim codepoints) " ")
                        [(list cp-or-range) cp-or-range]
                        [(list cp modifiers ...) cp]))
        (list (map (λ (str) (string->number (string-trim str) 16)) (string-split cp-or-range ".."))
              (string->symbol (string-trim tag)))))
    (strip-context
     (with-syntax ([LINES lines])
       #'(module _ quad/unicode/emoji-prep
           . LINES)))))

(define-syntax (make-cond stx)
  (syntax-case stx ()
    [(_ ID VAL) #'(eq? ID VAL)] ;; I believe `eq?` is OK because a codepoint is a fixnum
    [(_ ID LVAL RVAL) #'(<= LVAL ID RVAL)]))

(provide (rename-out [mb #%module-begin])
         (except-out (all-from-out racket/base) #%module-begin))
(define-syntax (mb stx)
  (syntax-case stx ()
    [(_ (VALS RES) ...)
     (with-syntax ([F (datum->syntax stx 'emoji?)])
       #'(#%module-begin
          (provide F)
          (define (F x)
            (define cint (let loop ([x x])
                           (match x
                           [(? char? c) (char->integer c)]
                           [(? string? s) #:when (= 1 (string-length s))
                                          (loop (car (string->list s)))]
                           [(? integer?) x]
                           [_ (raise-argument-error 'emoji? "integer, character, or one-character string" x)])))
            (cond
              [(make-cond cint . VALS) 'RES] ...
              [else #f]))))]))