#lang debug racket/base
(require quadwriter/core
         racket/date
         racket/file
         racket/string
         racket/list
         racket/match
         racket/path
         txexpr
         quad/qexpr)
(provide (all-defined-out))

(define-logger fontproof)

(define (increment-path path)
  (for*/first ([i (in-naturals 2)]
               [incremented-path (in-value
                                  (path-replace-extension path
                                                          (string->bytes/utf-8
                                                           (format " ~a~a.pdf" (if (< i 10) "0" "") i))))]
               #:unless (file-exists? incremented-path))
              incremented-path))

(define (boolean->string bool) (if bool "true" "false"))

(define font-file-extensions '(#".otf" #".ttf" #".woff"))
(define (font-path-string? x)
  (and (path-string? x)
       (member (path-get-extension (string->path x)) font-file-extensions)
       #true))

(define (resolve-family-bold-italic font-name-arg)
  (define bi-suffix-pat #px"\\s*((?i:bold))?\\s*((?i:italic))?$")
  (match-define (list suffix bold? italic?) (regexp-match bi-suffix-pat font-name-arg))
  (values (string-trim font-name-arg suffix #:left? #false) bold? italic?))

(define (make-proof font-or-qml [sample-text #f]
                    #:bold [make-bold #false]
                    #:italic [make-italic #false]
                    #:font-sizes [font-sizes-arg #false]
                    #:page-size [page-size-arg #false]
                    #:line-heights [line-heights-arg #false]
                    #:output-file-path [output-file-path-arg #false]
                    #:qml [output-qml? #true]
                    #:replace [replace #false])
  (define-values (doc output-file-path)
    (match font-or-qml
      [(? qml-path? qml-ps)
       (define doc (qml->qexpr (file->string qml-ps)))
       (define output-file-path (path-replace-extension qml-ps #".pdf"))
       (values doc output-file-path)]
      [font-name-arg
       (unless (and sample-text (non-empty-string? sample-text))
         (raise-user-error "no text to proof; exiting"))
       (define-values (initial-font-family initial-font-bold initial-font-italic)
         (match font-name-arg
           [(? font-path-string? ps) (values (path->string (path->complete-path ps)) #f #f)]
           [_ (resolve-family-bold-italic font-name-arg)]))
       (log-fontproof-info (format "generating proof for ~a" font-name-arg))
       (define page-size (or page-size-arg "letter"))
       (define font-sizes (string-split (or font-sizes-arg "12 10.5 9")))
       (define line-heights (string-split (or line-heights-arg "1.25em")))
       (define bold-variants (cond
                               [initial-font-bold '(#t)]
                               [make-bold '(#f #t)]
                               [else '(#f)]))
       (define italic-variants (cond
                                 [initial-font-italic '(#t)]
                                 [make-italic '(#f #t)]
                                 [else '(#f)]))
       (when (and initial-font-bold make-bold)
         (log-fontproof-info (format "starting font is bold; omitting bold proof~a" (match (length italic-variants) [2 "s"] [_ ""]))))
       (when (and initial-font-italic make-italic)
         (log-fontproof-info (format "starting font is italic; omitting italic proof~a" (format "starting font is bold; omitting bold proof~a" (match (length italic-variants) [2 "s"] [_ ""])))))
       (define doc-interior
         (cons 'q
               (add-between
                (for*/list ([font-size (in-list font-sizes)]
                            [font-bold (in-list bold-variants)]
                            [font-italic (in-list italic-variants)]
                            [line-height (in-list line-heights)])
                           (attr-set*
                            (txexpr* 'q null sample-text)
                            'font-size font-size
                            'font-italic (boolean->string font-italic)
                            'font-bold (boolean->string font-bold)
                            'line-height line-height
                            'footer-text (format "~a test ~a/~a · ~a"
                                                 initial-font-family
                                                 font-size
                                                 line-height
                                                 (date->string (current-date) #t))))
                page-break)))
       (define doc (attr-set*
                    doc-interior
                    'page-size page-size
                    'page-margin-left "12p"
                    'page-margin-right "12p"
                    'font-family initial-font-family
                    'hyphenate "false"
                    'footer-display "true"))
       (define output-file-path
         (match (path->complete-path
                 (or output-file-path-arg
                     (build-path (find-system-path 'desk-dir)
                                 (format "~a proof.pdf" font-name-arg))))
           [(? file-exists? path) #:when (not replace) (increment-path path)]
           [path path]))
       (values doc output-file-path)]))
  (begin0
    (render-pdf doc output-file-path)
    (when output-qml?
      (define qml-path (path-replace-extension output-file-path #".qml"))
      (call-with-output-file* qml-path
                              (λ (op) (display (qexpr->qml doc) op ))
                              #:exists 'replace))))