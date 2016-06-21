#lang racket/base

#|
Same semantics as `#lang quad`,
but substitutes a Scribble-style text-based reader
|#

(module reader syntax/module-reader
  quad/main
  #:read quad-read
  #:read-syntax quad-read-syntax
  #:whole-body-readers? #t ;; need this to make at-reader work
  (require scribble/reader racket/list)
  (require sugar/debug)
  
  (define (quad-read p)
    (syntax->datum (quad-read-syntax (object-name p) p)))
  
  (define quad-command-char #\@)
  
  (define (quad-read-syntax path-string p)
    (define quad-at-reader (make-at-reader
                            #:command-char quad-command-char
                            #:syntax? #t 
                            #:inside? #t))
    (define source-stx (quad-at-reader path-string p))
    (define source-stx-list (syntax->list source-stx))
    (define config-line (car source-stx-list))
    ;; we dump all whitespace lines in plain-text mode, as they have no semantic purpose
    ;; the at-reader will kindly separate these all-whitespace lines into their own list elements
    (define source-stx-no-interline-whitespace
      (filter-not (λ(stx)
                    (define datum (syntax->datum stx))
                    (and (string? datum) (regexp-match #px"^\\s+$" datum))) (cdr source-stx-list)))
    (datum->syntax source-stx (cons config-line source-stx-no-interline-whitespace) source-stx)))