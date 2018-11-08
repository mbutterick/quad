#lang br
(require fontkit/font)

(define f (openSync "fira.ttf"))

(define gr (time (layout f "fifl")))

(get-field glyphs gr)

(get-field positions gr)

(send gr advanceWidth)