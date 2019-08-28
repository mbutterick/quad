#lang quadwriter

#:page-height "8in"
#:page-width "6in"

'(q ((flow "footnote")(fn-text "0")(font-size-adjust "80%")(line-heightr-adjust "70%")) "Leftover from previous footnote." (q ((break "para"))))

"Hello" '(q ((fn-ref "1")) "*")

'(q ((flow "footnote")(fn-text "1")(font-size-adjust "80%")(line-height-adjust "70%")) (q ((fn-text-start "1")) "*") "A convertible value in the sense of convertible? is used in a renderer-specific way, but values convertible to 'text renders the same as the resulting string. If a renderer is not able to convert the value to a known format, the value is converted to a string using write." (q ((break "para"))))

" world." '(q ((fn-ref "2")) "†")

'(q ((flow "footnote")(fn-text "2")(font-size-adjust "80%")(line-height-adjust "70%")) (q ((fn-text-start "2")) "†") "An instance of link-element has a tag for the target of the link." (q ((break "para"))))

" I love you."