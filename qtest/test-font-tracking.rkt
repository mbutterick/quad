#lang quadwriter

'(q "Left aligned")
'(para-break)

'(q ((font-size "20")(font-tracking "5")) "we have the same tracking you see")
'(para-break)
'(q ((font-size "20")(font-tracking "0.25em")) "we have the same tracking you see")

'(para-break)

'(q "Justified")
'(para-break)


'(q ((font-size "20")(font-tracking "5")(line-align "justify")) "we have the same tracking you see")
'(para-break)
'(q ((font-size "20")(font-tracking "0.25em")(line-align "justify")) "we have the same tracking you see")
