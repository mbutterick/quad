#lang quadwriter

#:page-height "8in"
#:page-width "6in"

"Hello"

'(q ((flow "footnote")(font-size-adjust "80%")(line-height-adjust "70%")) "A convertible value in the sense of convertible? is used in a renderer-specific way, but values convertible to 'text renders the same as the resulting string. If a renderer is not able to convert the value to a known format, the value is converted to a string using write." (q ((break "para"))))

" world."

'(q ((flow "footnote")(font-size-adjust "80%")(line-height-adjust "70%")) "An instance of link-element has a tag for the target of the link." (q ((break "para"))))

" I love you."