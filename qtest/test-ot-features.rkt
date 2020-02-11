#lang quadwriter


'(q ((font-features "liga 0")) "No ligs: fifle")

'(para-break)

'(q ((font-features "liga 1")) "Ligs: fifle")

'(para-break)

'(q ((font-features "liga 1")) (q ((font-features "liga 0")) "No ligs: fifle"))

'(para-break)

'(q ((font-features "liga 1")) (q ((font-features "+ liga 0")) "No ligs: fifle"))

'(para-break)

'(q ((font-features "zero 1")) (q ((font-features "liga 0")) "No ligs, no slashed zero: fifle0"))

'(para-break)

'(q ((font-features "zero 1")) (q ((font-features "+ liga 0")) "No ligs, slashed zero: fifle0"))