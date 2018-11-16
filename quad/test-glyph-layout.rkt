#lang br
(require fontland/font)

(define f (openSync "fira.ttf"))

(define gr (time (layout f "fifl")))

(get-field glyphs gr)

(get-field positions gr)

(send gr advanceWidth)