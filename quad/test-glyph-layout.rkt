#lang br
(require fontland/font)

(define f (open-font "fira.ttf"))

(define gr (time (layout f "fifl")))

gr