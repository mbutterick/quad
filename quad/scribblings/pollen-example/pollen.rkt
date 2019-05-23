#lang racket
(require pollen/decode quadwriter)
(provide root render-pdf)
 
(define (root . xs)
  `(q ,@(add-between (decode-paragraphs xs 'q) para-break)))
