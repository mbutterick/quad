#lang debug racket/base
(require quadwriter/core
         racket/date
         racket/string
         racket/list
         racket/match
         txexpr)
(provide make-proof)


(define (increment-path path)
  (for*/first ([path (in-value path)]
               [i (in-naturals 2)]
               [incremented-path (in-value (path-replace-extension path (string->bytes/utf-8 (format " ~a~a.pdf" (if (< i 10) "0" "") i))))]
               #:unless (file-exists? incremented-path))
              incremented-path))
  

(define (make-proof family-name [doc-arg #f]
                    #:font-sizes [font-sizes-arg #false]
                    #:page-size [page-size-arg #false]
                    #:line-heights [line-heights-arg #false]
                    #:output-file-path [output-file-path-arg #false]
                    #:replace [replace #false])
  (define output-file-path
    (match (path->complete-path
            (or output-file-path-arg
                (build-path (find-system-path 'desk-dir)
                            (format "~a proof.pdf" family-name))))
      [(? file-exists? path) #:when (not replace)
                             (displayln (format "File \"~a\" exists" path))
                             (display "Overwrite? [y]es [n]o [k]eep both: ")
                             (case (read)
                               [(y yes) path]
                               [(k keep) (increment-path path)]
                               [else #false])]
      [path path]))
  (when output-file-path
    (displayln (format "generating test for ~a" family-name))
    (define page-size (or page-size-arg "letter"))
    (define font-sizes (string-split (or font-sizes-arg "12 10.5 9")))
    (define line-heights (string-split (or line-heights-arg "1.25em")))
    (define sample-text (or doc-arg "no doc provided"))
    (define doc-interior
      (cons 'q
            (add-between
             (for*/list ([font-size (in-list font-sizes)]
                         [line-height (in-list line-heights)])
                        (attr-set*
                         (list
                          'q sample-text)
                         'font-size font-size
                         'line-height line-height
                         'footer-text (format "~a test ~a/~a · ~a"
                                              family-name
                                              font-size
                                              line-height
                                              (date->string (current-date) #t))))
             page-break)))
    (define doc (attr-set*
                 doc-interior
                 'page-size page-size
                 'page-margin-left "12p"
                 'page-margin-right "12p"
                 'font-family family-name
                 'footer-display "true"
                 'line-wrap "best"))
    (render-pdf doc output-file-path)))