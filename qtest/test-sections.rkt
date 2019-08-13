#lang quadwriter

'(q ((page-margin-gutter "1.5in")(page-margin-left "0.5in")(page-margin-right "0.5in"))

"Section 1 Page 1 on right"

(q ((break "section")))

(q ((page-width "5in")(page-height "5in")(page-side-start "right")) "Section 2 Page 1 on right"

(q ((break "page")))

"Section 2 Page 2 on left")

(q ((break "section")))

(q ((page-width "5in")(page-height "5in")(page-side-start "left")) "Section 3 Page 1 on left"

(q ((break "page")))

"Section 3 Page 2 on right")

)