#lang quadwriter


'(q ((font-features "liga 0")) "No ligs: fifle")

'(q ((break "para")))

'(q ((font-features "liga 1")) "Ligs: fifle")

'(q ((break "para")))

'(q ((font-features "liga 1")) (q ((font-features "liga 0")) "No ligs: fifle"))

'(q ((break "para")))

'(q ((font-features "liga 1")) (q ((font-features "+ liga 0")) "No ligs: fifle"))

'(q ((break "para")))

'(q ((font-features "zero 1")) (q ((font-features "liga 0")) "No ligs, no slashed zero: fifle0"))

'(q ((break "para")))

'(q ((font-features "zero 1")) (q ((font-features "+ liga 0")) "No ligs, slashed zero: fifle0"))