#lang debug racket/base
(provide (rename-out
          [quad-lang-read read]
          [quad-lang-read-syntax read-syntax]
          [qr-mod:get-info get-info]))

(module quad-reader syntax/module-reader
  #:language 'quad/lang
  #:info qgi:get-info
  #:read my-read
  #:read-syntax my-read-syntax
  #:whole-body-readers? #true
  (require (prefix-in qgi: quad/get-info) (prefix-in at: scribble/reader))
  (define (my-read ip) (syntax->datum (my-read-syntax ip)))
  (define (my-read-syntax src ip)
    (define reader (at:make-at-reader 
                    #:command-char #\◊
                    #:syntax? #t 
                    #:inside? #t))
    (reader src ip)))

(require debug/reader (prefix-in qr-mod: 'quad-reader))

#|
Use wrap-reader on the whole-module read function that would be exported
by the reader module, not the single-expression read function like
at:read-syntax that you deal with within syntax/module-reader or normal use.
|#

(define quad-lang-read (wrap-reader qr-mod:read))
(define quad-lang-read-syntax (wrap-reader qr-mod:read-syntax))