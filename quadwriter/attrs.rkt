#lang debug racket/base
(provide (all-defined-out))
(define block-attrs '(display
                      inset-top
                      inset-bottom
                      inset-left
                      inset-right
                      border-inset-top
                      border-inset-bottom
                      border-inset-left
                      border-inset-right
                      border-width-left
                      border-width-right
                      border-width-top
                      border-width-bottom
                      border-color-left
                      border-color-right
                      border-color-top
                      border-color-bottom
                      background-color
                      keep-lines
                      keep-first
                      keep-last
                      keep-all
                      keep-with-next
                      line-align
                      line-align-last
                      first-line-indent
                      line-wrap))