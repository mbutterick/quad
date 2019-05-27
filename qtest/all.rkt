#lang racket
(require quadwriter pitfall/check-pdf racket/runtime-path)

(define-for-syntax (test-pdf-name path)
  (path-add-extension (path-replace-extension path #".pdf") #"" #" copy."))

(define-syntax (make-test-pdf stx)
  (syntax-case stx ()
    [(_ PATH)
     (with-syntax ([PDF-NAME (test-pdf-name (syntax-e #'PATH))])
       #'(parameterize ([quadwriter-test-mode #t])
           (render-pdf (dynamic-require PATH 'doc) PDF-NAME)))]))

(define-syntax (test-one stx)
  (syntax-case stx ()
    [(_ PATH)
     (with-syntax ([PDF-NAME (test-pdf-name (syntax-e #'PATH))])
       #'(begin
           (define-runtime-path path-to-test PATH)
           (define-runtime-path test-base PDF-NAME)
           (println PATH)
           (check-pdfs-equal? (time (parameterize ([quadwriter-test-mode #t]
                                                   [current-output-port (open-output-nowhere)])
                                      (render-pdf (dynamic-require path-to-test 'doc) #f))) test-base)))]))

(define-syntax-rule (test-each PATH ...)
  (begin (test-one PATH) ...))

(test-each "hello.rkt"
           "hello.rkt"
           "hello.rkt")

