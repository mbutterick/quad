#lang br
(require quadwriter/core quadwriter/param quad/quad)

(draw-debug? #true)
(verbose-quad-printing? #f)

(render-pdf '(q ((page-width "300") (page-height "300")
                 (page-margin-left "60")
                 (page-margin-top "40")
                 (line-align "right")) "Hello world") "foo.pdf")