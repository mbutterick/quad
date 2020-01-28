#lang quadwriter

'(q ((page-height "6in")(column-count "2"))

"Page 1 Column 1 Line 1"

(q ((break "line")))

"Page 1 Column 1 Line 2"

(q ((break "column")))

(q ((break "para")))

"Page 1 Column 2 Line 1"

(q ((break "line")))

"Page 1 Column 2 Line 2"

(q ((break "page")))


"Page 2 Column 1 Line 1"

(q ((break "line")))

(q ((break "column")))

(q ((break "page")))


"Page 3 Column 1 Line 1"

(q ((break "page")))

(q ((break "page")))

"Page 5 Column 1 Line 1"

)