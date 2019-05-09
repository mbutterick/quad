#lang br
(require quadwriter/core quadwriter/param quad/quad)

(draw-debug? #true)
(zoom-factor 2)
(verbose-quad-printing? #f)

(render-pdf '(q ((page-width "250") (page-height "250")
                 (page-margin-left "30")
                 (page-margin-top "20")
                 (line-align "center")) "Hello world") "foo.pdf")