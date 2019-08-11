#lang racket
(require quadwriter pitfall/check-pdf racket/runtime-path)

(define-for-syntax (test-pdf-name path)
  (path-add-extension (path-replace-extension path #".pdf") #"" #"-tester."))

(define-syntax (make-test-pdf stx)
  (syntax-case stx ()
    [(_) #'(begin)]
    [(_ PATH . REST)
     (with-syntax ([PDF-NAME (test-pdf-name (syntax-e #'PATH))])
       #'(begin
           (define-runtime-path rp PATH)
           (time
            (parameterize ([quadwriter-test-mode #t])
              (render-pdf (dynamic-require PATH 'doc) PDF-NAME rp)))
           (make-test-pdf . REST)))]))

(define-syntax (test-each stx)
  (syntax-case stx ()
    [(_) #'(begin)]
    [(_ MOD-PATH . REST)
     (with-syntax ([PDF-NAME (test-pdf-name (syntax-e #'MOD-PATH))])
       #'(begin
           (define-runtime-path path-to-test MOD-PATH)
           (define-runtime-path test-base PDF-NAME)
           (println MOD-PATH)
           (define-runtime-path path (path-replace-extension MOD-PATH #".pdf"))
           (check-pdfs-equal? (time (parameterize ([quadwriter-test-mode #t]
                                                   [current-output-port (open-output-nowhere)])
                                      (render-pdf (dynamic-require path-to-test 'doc) path path-to-test)
                                      path)) test-base)
           (test-each . REST)))]))

(test-each "test-docs.rkt"
           "test-emoji.rkt"
           "test-fallback-mini.rkt"
           "test-fallback-super.rkt"
           "test-hello.rkt"
           "test-kafka.rkt"
           "test-sections.rkt"
           "test-symbol.rkt"
           "test-image.rkt")
