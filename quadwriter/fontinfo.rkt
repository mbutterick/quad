#lang debug racket/base
(require racket/string
         racket/runtime-path
         "log.rkt"
         (for-syntax racket/base))
(provide #%top #%datum #%app #%top-interaction
         (for-syntax #%datum)
         (rename-out [mb #%module-begin]))

(define-syntax (mb stx)
  (syntax-case stx ()
    [(_ PATH-STRING
        ((ATTR-NAME ATTR-VAL) ...)
        . PT)
     (with-syntax ([FONTINFO-ID (datum->syntax #'PATH-STRING 'fontinfo)])
       #'(#%module-begin
          (provide FONTINFO-ID)
          (define-runtime-path ATTR-NAME ATTR-VAL) ...
          (define FONTINFO-ID
            (for/hasheq ([k (in-list (list 'ATTR-NAME ...))]
                       [v (in-list (list ATTR-NAME ...))])
              (unless (file-exists? v)
                (log-quadwriter-warning (format "~a does not exist" v)))
              (values k v)))))]))

(module reader racket/base
  (require "lang-helper.rkt")
  (provide (rename-out [rs read-syntax]) get-info)
  (define rs (make-read-syntax 'quadwriter/fontinfo
                               (λ (path ip)
                                 (for/list ([tok (in-port read ip)])
                                   tok)))))