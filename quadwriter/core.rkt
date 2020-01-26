#lang debug racket/base
(require "layout.rkt"
         "render.rkt"
         "param.rkt")
(provide render-pdf
         para-break
         line-break
         page-break
         column-break
         hr-break
         section-break
         q:para-break
         q:line-break
         q:page-break
         q:hr-break
         (all-from-out "param.rkt"))

(define para-break '(q ((break "para"))))
(define line-break '(q ((break "line"))))
(define page-break '(q ((break "page"))))
(define column-break '(q ((break "column"))))
(define hr-break '(q ((break "hr"))))
(define section-break '(q ((break "section"))))
