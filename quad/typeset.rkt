#lang quad/dev
(provide (all-defined-out))
(require "tokenize.rkt" "parse.rkt")

#;(define (typeset x)
    (pages->doc
     (append*
      (for/list ([multipage (in-list (input->nested-blocks x))])
                (columns->pages
                 (append*
                  (for/list ([multicolumn (in-list multipage)])
                            (lines->columns
                             (append*
                              (for/list ([block-quads (in-list multicolumn)])
                                        (block-quads->lines block-quads)))))))))))


(define input (quad #f "Meg is" (quad #f 'line-break) " an ally."))
(syntax->datum (parse (tokenize input)))
