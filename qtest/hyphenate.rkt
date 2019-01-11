#lang qtest/markdown

# Hyphenate

A simple _hyphenation engine_ that uses the Knuth–Liang hyphenation algorithm originally developed for TeX.

I **have added little** to their work. Accordingly, I take no credit, except a spoonful of *snako-bits.*

And now, for something __altogether__ the same.

## 1. Installation

At the command line:

We said `raco pkg install hyphenate` dude

What?!

> Hyphenate `xexpr` by calculating hyphenation points and inserting
`joiner` at those points. By default, `joiner` is the soft hyphen
\(Unicode 00AD = decimal 173\). Words shorter than
`#:min-length` `length` will not be hyphenated. To hyphenate words of
any length, use `#:min-length` `#f`.

A [list of web colors](https://en.wikipedia.org/wiki/Web_colors).
Certain word processors allow users to [insert soft
hyphens](http://practicaltypography.com/optional-hyphens.html) in their
text.