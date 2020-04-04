#lang racket/base
(require quadwriter/core
         racket/date
         racket/string
         racket/list)
(provide make-proof)

(define doc "todo: more than nothing")

(define (make-proof family-name
                    #:font-sizes [font-sizes-arg #false]
                    #:page-size [page-size-arg #false]
                    #:line-heights [line-heights-arg #false]
                    #:output-file-path [output-file-path-arg #false])
  (define page-size (or page-size-arg "letter"))
  (define font-sizes (string-split (or font-sizes-arg "12 10.5 9")))
  (define line-heights (string-split (or line-heights-arg "1.25em")))
  (define output-file-path
    (or output-file-path-arg (build-path (find-system-path 'desk-dir)
                                         (format "~a proof.pdf" family-name))))
  (displayln (format "generating test for ~a" family-name))
  (render-pdf
   `(q
     ((page-size ,page-size)
      (page-margin-left "12p")
      (page-margin-right "12p")
      (font-family ,family-name)
      (footer-display "true")
      (line-wrap "best"))
     ,@(add-between
        (for*/list ([font-size (in-list font-sizes)]
                    [line-height (in-list line-heights)])
                   `(q ((font-size ,font-size)
                        (line-height ,line-height)
                        (footer-text ,(format "~a test ~a/~a · ~a"
                                              family-name
                                              font-size
                                              line-height
                                              (date->string (current-date) #t))))
                       ,doc))
        section-break))
   output-file-path))