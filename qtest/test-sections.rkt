#lang quadwriter

'(q ((page-margin-gutter "1.5in")(page-margin-left "0.5in")(page-margin-right "0.5in"))

"Section 1 Page 1 on right"

(section-break)

(q ((page-width "5in")(page-height "5in")(page-side-start "right")) "Section 2 Page 1 on right"

(page-break)

"Section 2 Page 2 on left")

(section-break)

(q ((page-width "5in")(page-height "5in")(page-side-start "left")) "Section 3 Page 1 on left"

(page-break)

"Section 3 Page 2 on right")

)