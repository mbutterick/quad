#lang quadwriter

'(q ((page-width "4in")(page-height "4in"))

"Hello world"

(q ((draw "line") (x1 "10") (y1 "10") (x2 "100") (y2 "200")))
(q ((draw "line") (x1 "100") (y1 "200") (x2 "200") (y2 "100")))

(q ((break "page")))

"Goodbye fools"

(q ((draw "text")(font-italic "true")(x "20")(y "20")(text "Hello from 20,20")) "outside")

)