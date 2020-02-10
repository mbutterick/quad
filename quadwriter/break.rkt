#lang debug racket
(require "struct.rkt"
         "attrs.rkt"
         quad/quad)
(provide (all-defined-out))


(define (convert-break-quad q)
  ;; this is verbose & ugly because `struct-copy` is a macro
  ;; we want to use break prototypes but also preserve their type
  (match (quad-ref q :break)
    ["para" (quad-copy para-break-quad q:para-break [attrs (quad-attrs q)])]
    ["line" (quad-copy line-break-quad q:line-break [attrs (quad-attrs q)])]
    ["page" (quad-copy page-break-quad q:page-break [attrs (quad-attrs q)])]
    ["column" (quad-copy column-break-quad q:column-break [attrs (quad-attrs q)])]
    ["hr" (quad-copy hr-break-quad q:hr-break [attrs (quad-attrs q)])]
    ["section" (quad-copy section-break-quad q:section-break [attrs (quad-attrs q)])]
    [_ q]))


(module+ test
  (require rackunit quad/qexpr)
  (check-equal? (quad-ref (convert-break-quad (qexpr->quad '(q ((break "page") (foo "bar"))))) 'foo) "bar"))


(define q:line-break (make-line-break-quad #:printable #f
                                           #:id 'line-break))
(define q:para-break (make-para-break-quad #:printable #f
                                           #:id 'para-break))
(define q:hr-break (make-hr-break-quad #:printable #t
                                       #:id 'hr-break))
(define q:column-break (make-column-break-quad #:printable #f
                                               #:id 'column-break))

(define q:page-break (make-page-break-quad #:printable #f
                                           #:id 'page-break))

(define q:section-break (make-section-break-quad #:printable #f
                                                 #:id 'section-break))

(define para-break '(q ((break "para"))))
(define line-break '(q ((break "line"))))
(define page-break '(q ((break "page"))))
(define column-break '(q ((break "column"))))
(define hr-break '(q ((break "hr"))))
(define section-break '(q ((break "section"))))

(module+ test
  (require rackunit quad/atomize)
  (check-true (line-break-quad? (second (quad-elems (q "foo" q:page-break "bar")))))
  (check-true (line-break-quad? (second (atomize (q "foo" q:page-break "bar"))))))
