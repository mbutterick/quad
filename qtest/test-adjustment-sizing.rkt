#lang quadwriter


'(q ((column-count "2")(font-size "18")(line-height "2em")) "The lines in both columns should be vertically aligned. The column on the left is font size 18pt with line height 2em, or 36pt. The right column is font size 50%, or 9pt, with line height 4em, or 36pt."
(q ((break "column")))
(q ((break "para")))
(q ((font-size "50%")(line-height "4em")) "The lines in both columns should be vertically aligned. The column on the left is font size 18pt with line height 2em, or 36pt. The right column is font size 50%, or 9pt, with line height 4em, or 36pt."))