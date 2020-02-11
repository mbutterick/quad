#lang debug racket
(require "struct.rkt"
         "attrs.rkt"
         quad/quad)
(provide (all-defined-out))


(define (convert-break-quad q)
  ;; this is verbose & ugly because `struct-copy` is a macro
  ;; we want to use break prototypes but also preserve their type
  (match (quad-tag q)
    [(== 'para-break eq?)
     (quad-copy para-break-quad q:para-break [attrs (quad-attrs q)])]
    [(== 'line-break eq?)
     (quad-copy line-break-quad q:line-break [attrs (quad-attrs q)])]
    [(== 'page-break eq?)
     (quad-copy page-break-quad q:page-break [attrs (quad-attrs q)])]
    [(== 'column-break eq?)
     (quad-copy column-break-quad q:column-break [attrs (quad-attrs q)])]
    [(== 'hr eq?) (quad-copy hr-break-quad q:hr-break [attrs (quad-attrs q)])]
    [(== 'section-break eq?)
     (quad-copy section-break-quad q:section-break [attrs (quad-attrs q)])]
    [_ #false]))


(module+ test
  (require rackunit quad/qexpr)
  (check-equal? (quad-ref (convert-break-quad (qexpr->quad '(page-break ((foo "bar"))))) 'foo) "bar"))


(define q:line-break (make-line-break-quad #:printable #f
                                           #:tag 'line-break))
(define q:para-break (make-para-break-quad #:printable #f
                                           #:tag 'para-break))
(define q:hr-break (make-hr-break-quad #:printable #t
                                       #:tag 'hr-break))
(define q:column-break (make-column-break-quad #:printable #f
                                               #:tag 'column-break))

(define q:page-break (make-page-break-quad #:printable #f
                                           #:tag 'page-break))

(define q:section-break (make-section-break-quad #:printable #f
                                                 #:tag 'section-break))

(define para-break '(para-break))
(define line-break '(line-break))
(define page-break '(page-break))
(define column-break '(column-break))
(define hr-break '(hr))
(define section-break '(section-break))

(module+ test
  (require rackunit quad/atomize)
  (check-true (line-break-quad? (second (quad-elems (q "foo" q:page-break "bar")))))
  (check-true (line-break-quad? (second (atomize (q "foo" q:page-break "bar"))))))
