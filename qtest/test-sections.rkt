#lang quadwriter

"Section 1 Page 1 on right"

'(q ((break "section")))

'(q ((page-width "5in")(page-height "5in")(page-side-start "right")) "Section 2 Page 1 on right"

(q ((break "page")))

"Section 2 Page 2 on left")

'(q ((break "section")))

'(q ((page-width "5in")(page-height "5in")(page-side-start "left")) "Section 3 Page 1 on left")
