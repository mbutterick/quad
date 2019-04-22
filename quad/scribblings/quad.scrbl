#lang scribble/manual

@(require (for-label racket/base racket/draw)
pollen/scribblings/mb-tools)

@title[#:style 'toc]{Quad: document processor}

@author[(author+email "Matthew Butterick" "mb@mbtype.com")]

@defmodulelang*[(quadwriter
quadwriter/markdown
quadwriter/markup)]

@italic{This software is under development. Set expectations accordingly.}

@section{Installing Quad}

At the command line:
@verbatim{raco pkg install quad}

After that, you can update the package like so:
@verbatim{raco pkg update quad}

Quad is not stable, usable software. It is currently in documented-demo mode. Fiddle with it at your own risk. I make no commitment to maintain the API in its current state.

@section{What is Quad?}

A document processor that outputs to PDF.

@section{What is Quadwriter?}

A demo language built on Quad that takes a text-based source file as input, calculates the typesetting and layout, and then outputs a PDF.

@section{What's a document processor?}

A document processor is a rule-driven typesetter. It takes a text-based source file as input and converts it into a page layout. 

For instance, LaTeX is a document processor. So are web browsers. Quad borrows from both traditions — it's an attempt to modernize the good ideas in LaTeX, and generalize the good ideas in web browsers.

Document processors sit opposite WYSIWYG tools like Word and InDesign. There, the user controls the layout by manipulating a representation of the page on the screen. This is fine as far as it goes. But changes to the layout — for instance, a new page size — often require a new round of manual adjustments. 

A document processor, by contrast, relies on markup codes within the text to determine the layout programmatically. Compared to WYSIWYG, this approach offers less granular control. But it also creates a more flexible relationship between the source and its possible layouts. 

Another benefit of document processors is that it permits every document to have a high-level, text-based source file that's independent of any particular output format (rather than the opaque binary formats endemic to Word, InDesign, et al.)

@subsection{Why not just use LaTeX?}

I wouldn't want to criticize software merely for being old. It's a great compliment to LaTeX that it's endured this long. But —

@itemlist[#:style 'ordered

@item{It's never made much headway beyond its original audience of scientific & technical writers.}

@item{The last 25 years of advances in digital typesetting have been implemented as a huge (occasionally tenuous) tower of patches.}

@item{The source code is increasingly opaque to today's programmers. Meaning, if LaTeX were plausibly rewritable, it would've been rewritten by now.}
]

Instead, let's take its good ideas — there are a few — and terraform a new planet. 

@margin-note{Quad has no ambition to supplant LaTeX. I am not an academic writer. Those typesetting problems are not my problems. So I am not proposing to solve them. Best of luck, though.}

@subsection{Why not use more HTML/CSS?}

In principle, it's possible to generate PDF documents from a web browser. Support for paper-based layouts has been part of the CSS concept @link["https://www.w3.org/People/howcome/p/cascade.html"]{since the beginning} (though it's been lightly used).

But web browsers have some limitations:

@itemlist[#:style 'ordered

@item{Web browsers only render HTML, and many typesetting concepts (e.g., footnotes) don't correspond to any HTML entity. So there is a narrowing of possiblities.}

@item{Browsers are built for speed, so high-quality typesetting (e.g., the Knuth–Plass linebreaking algorithm) is off the table}

@item{Browsers are inconsistent in how they render pages.}

@item{Taking off my typography-snob tiara here — browsers are unstable. What seems well supported today can be broken or removed tomorrow. So browsers can't be a part of a dependable publishing workflow that yields reproducible results.}
]


@section{What does Quad do?}

Quad produces PDFs using three ingredients: 

@itemlist[#:style 'ordered
  @item{A @bold{font engine} that handles glyph shaping and positioning using standard TTF or OTF font files.}

  @item{A @bold{layout engine} that converts typesetting instructions into an output-independent layout — e.g., putting characters into lines, and lines into pages.}

  @item{A @bold{PDF engine} that takes this layout and renders it as a finished PDF file.}
]

While there's no reason Quad couldn't produce an HTML layout, that's an easier problem, because most of the document-layout chores can (and should) be delegated to the web browser. For now, most of Quad's apparatus is devoted to its layout engine so it can produce PDFs.

Much of the font-parsing and PDF-rendering code in Quad is adapted from @link["http://github.com/foliojs/"]{FolioJS} by Devon Govett. I thank Mr. Govett for figuring out a lot of details that would've made me squeal in agony. 

@section{What doesn't Quad do?}

@itemlist[#:style 'ordered
@item{Quad is not a WYSIWYG or interactive previewing tool.}

@item{Quad does not have user-level representations of formatting, à la Word style sheets.}

@item{Quad does not handle semantic or configurable markup. Its markup is limited to its specific, layout-based vocabulary.}
]
Rather, it's designed to cooperate with tools that offer these facilities. For instance, Quadwriter is a demonstration language that provides an interface to a small set of word-processor-like features that are implemented with Quad.

@section{Theory of operation}

A document processor starts with input that we can think of as one giant line of text. It breaks this into smaller lines, and then distributes these lines across pages. Various complexities surface along the way. But that's the basic idea.

More specifically:

@itemlist[#:style 'ordered
  @item{Quad starts with a source file. In this demo, we can will use the @code{#lang quadwriter} language. For the most part, it's text with markup codes (though it may also include things like diagrams and images).}

  @item{Each markup entity is called a @defterm{quad}. A quad roughly corresponds to a box. ``Roughly'' because quads can have zero or negative dimension. Also, at the input stage, the contents of some quads may end up being spread across multiple non-overlapping boxes (e.g., a quad containing a word might be hyphenated to appear on two lines). The more precise description of a quad is therefore ``contiguous formatting region''. Quads can be recursively nested inside other quads, thus the input file is tree-shaped.}

  @item{This tree-shaped input file is flattened into a list of @defterm{atomic} quads. ``Atomic'' in the sense that these are the smallest items the typesetter can manipulate. (For instance, the word @italic{bar} would become three one-character quads. An image or other indivisible box would remain as is.) During the flattening, tags from higher in the tree are propagated downward by copying them into the atomic quads. The result is that all the information needed to typeset an atomic quad is contained within the quad itself.

  @margin-note{The input is flattened because typesetting operations are easier to reason about as a linear sequence of instructions (i.e., an imperative model). To see why, consider how you'd handle a page break within a tree model. No matter how deep you were in your typesetting tree, you'd have to jump back to the top level to handle your page break (because it affects the positioning of all subsequent items). Then you'd have to jump back to where you were, deep in the tree. That's unnatural.}}

  @item{Atomic quads are composed into lines using one of two algorithms. (Each line is just another quad, of a certain width, that contains these atomic quads.) The first-fit algorithm puts as many quads onto a line as it can before moving on to the next. The best-fit algorithm minimizes the total looseness of all the lines in a paragraph (also known as the @defterm{Knuth–Plass linebreaking algorithm} developed for TeX). Best fit is slower, of course.}

  @item{Once the lines are broken, extra space is distributed within each line according to whether the line should appear centered, left-aligned, justified, etc. The result is a list of quads that fills the full column width.}

  @item{Lines are composed into pages.}

  @item{Before the typeset markup is passed to the renderer, it goes through a simplification phase — a lot of adjacent quads will have the same formatting characteristics, and these can be consolidated into runs of text.}

  @item{The renderer walks through the markup and positions and draws each quad, using information in the markup attributes to determine position, color, font, size, style, etc.}

]


@section{Enough talk — let's rock}

Open DrRacket (or the editor you prefer) and start a new document with @code{#lang quadwriter} as the first line:


@fileblock["test.rkt"
@codeblock|{
#lang quadwriter
Brennan and Dale like fancy sauce.
}|
]

Save the document. Any place, any name is fine. 

@onscreen{Run} the document. You'll get REPL output like this:

@repl-output{
hyphenate: cpu time: 0 real time: 0 gc time: 0
line-wrap: cpu time: 27 real time: 30 gc time: 0
page-wrap: cpu time: 0 real time: 1 gc time: 0
position: cpu time: 1 real time: 0 gc time: 0
draw: cpu time: 77 real time: 76 gc time: 23
wrote PDF to /Desktop/test.pdf
}

Congratulations — you just made your first PDF. If you want to have a look, either open the file manually, or enter this command on the REPL, which will open the PDF in your default viewer:

@terminal{
> (view-result)
}

Next, on the REPL enter this:

@terminal{
> doc
}

You will see the actual input to Quad, which is called a @defterm{Q-expression}:

@repl-output{
'(q "\n" "Brennan and Dale like fancy sauce.")
}

In the demos that follow, the input language will change slightly. But the PDF will be rendered the same way (by running the source file) and you can always look at @racket[doc].


@subsection{Soft rock: Quadwriter + Markdown}

I @link["https://docs.racket-lang.org/pollen/second-tutorial.html#%28part._the-case-against-markdown%29"]{don't recommend} that writers adopt Markdown for serious projects. But for goofing around, why not.

Let's update the first line of @racket["test.rkt"] so it uses the @racket[quadwriter/markdown] dialect instead of the plain @racket[quadwriter] language:

@fileblock["test.rkt"
@codeblock|{
#lang quadwriter/markdown
Brennan and Dale like fancy sauce.
}|
]

Run the file. The PDF result is the same. Why? Because a short line of plain text comes out the same way in both dialects.

Behind the scenes, however, @racket[quadwriter/markdown] is doing more heavy lifting. We can enter text with Markdown notation, and it will automatically be converted to the appropriate Quad formatting commands to make things look right. For instance, this sample combines a Markdown heading, bullet list, code block, and bold and italic formatting.

@fileblock["test.rkt"
@codeblock|{
#lang quadwriter/markdown
# Did you know?

__Brennan__ and **Dale** like:

* *Fancy* sauce
* _Chicken_ fingers

```
And they love to code
```
}|
]

You are welcome to paste in bigger Markdown files that you have laying around and see what happens. As a demo language, I'm sure there are tortured combinations of Markdown notation that will be confusing to @racket[quadwriter/markdown]. But I've tried with big vanilla files, and it's fine.

Curious characters can do this:

@terminal{
> doc
}

To see this:

@repl-output{
  '(q
  ((line-height "17"))
  <para-break>
  (q ((font-family "fira-sans-light") (first-line-indent "0") (display "block") (font-size "20") (line-height "24.0") (border-width-top "0.5") (border-inset-top "9") (inset-bottom "-3") (inset-top "6") (keep-with-next "true") (id "did-you-know")) "Did you know?")
  <para-break>
  (q ((keep-first "2") (keep-last "3") (line-align "left") (font-size-adjust "100%") (character-tracking "0") (hyphenate "true") (display "g190718")) (q ((font-bold "true") (font-size-adjust "100%")) "Brennan") " and " (q ((font-bold "true") (font-size-adjust "100%")) "Dale") " like:")
  <para-break>
  (q ((inset-left "30.0")) (q ((list-index "•")) (q ((font-italic "true") (font-size-adjust "100%")) "Fancy") " sauce") <para-break> (q ((list-index "•")) (q ((font-italic "true") (font-size-adjust "100%")) "Chicken") " fingers"))
  <para-break>
  (q
   ((display "block") (background-color "aliceblue") (first-line-indent "0") (font-family "fira-mono") (font-size "11") (line-height "14") (border-inset-top "10") (border-width-left "2") (border-color-left "#669") (border-inset-left "0") (border-inset-bottom "-4") (inset-left "12") (inset-top "12") (inset-bottom "8"))
   (q ((font-family "fira-mono") (font-size "10") (bg "aliceblue")) "And they love to code"))
  <para-break>)
}

This is the Q-expression that the source file produces. This Q-expression is passed to Quad for layout and rendering.

@subsection{Hard rock: Quadwriter + markup}

Suppose Markdown is just not your thing. You prefer to enter your markup the old-fashioned way — by hand. I hear you. So let's switch to the @racket[quadwriter/markup] dialect. First we try our simple test:

@fileblock["test.rkt"
@codeblock|{
#lang quadwriter/markup
Brennan and Dale like fancy sauce.
}|
]

We get the same PDF result as before, again because a short line of plain text is the same in this dialect as the others.

But if we want to reproduce the result of the Markdown notation, this time we use the equivalent markup tags:

@fileblock["test.rkt"
@codeblock|{
#lang quadwriter/markup
◊h1{Did you know?}

◊strong{Brennan} and ◊strong{Dale} like:

◊ul{
◊li{◊em{Fancy} sauce}
◊li{◊em{Chicken} fingers}
}

◊pre{
◊code{
And they love to code
}
}
}|
]

The special @litchar{◊} character is called a @defterm{lozenge}. It introduces markup tags. @link["https://docs.racket-lang.org/pollen/pollen-command-syntax.html#%28part._the-lozenge%29"]{Instructions for typing it}, but for now it suffices to copy & paste, or use the @onscreen{Insert Command Char} button in the DrRacket toolbar.

Under the hood, the @racket[quadwriter/markdown] dialect is converting the Markdown surface notation into markup tags that look like this. So the @racket[quadwriter/markup] dialect just lets us start with those tags. 

Curious characters can prove that this is so by again typing at the REPL:

@terminal{
> doc
}

This Q-expression is exactly the same as the one that resulted with the @racket[quadwriter/markdown] source file.

@subsection{Heavy metal: Quadwriter + Q-expressions}

@racket[quadwriter/markdown] showed a high-level notation that generated a Q-expression. Then @racket[quadwriter/markup] showed a mid-level notation that generated another (identical) Q-expression.

If we wish, we can also skip the notational layers and just write Q-expressions directly in our source file. We do this with the basic @racket[quadwriter] language. As usual, first comes our simple test:

@fileblock["test.rkt"
@codeblock|{
#lang quadwriter
Brennan and Dale like fancy sauce.
}|
]

The result is the same as before.




@;{
This is the compiled Quadwriter markup, showing what is sent to the typesetting engine.


@code{#lang quadwriter} is a Racket-implemented DSL (= domain-specific language). It's not a language in the sense of Turing-complete. Rather, a @code{#lang quadwriter} ``program'' resembles text annotated with high-level layout-description commands (not unlike XML/HTML). @code{#lang quadwriter} programs can be written directly, or generated as the output of other programs.

Each @"@"-expression in @code{#lang quadwriter} is interpreted as a @italic{quad} (roughly a box; more precisely a contiguous formatting region). A quad has the following syntax:

@code|{@quad-name[(list 'attr-name attr-val ...)]{text-or-more-quads ...}}|

The @code{(list 'attr-name attr-val ...)} is an interleaved list of symbols and values, as you might provide to @racket[hash]. The attribute list is mandatory. If a quad has no attributes of its own, this can be signaled with either @racket[empty] or @racket[#f]:

@codeblock|{
@quad-name[empty]{text-or-more-quads ...}
@quad-name[#f]{text-or-more-quads ...}
}|

If you thought this resembled an @link["http://docs.racket-lang.org/pollen/second-tutorial.html#%28part._.X-expressions%29"]{X-expression}, you wouldn't be wrong. Like X-expressions, quads are recursively composable. Also like X-expressions, the attributes in a quad apply to all the text or quads within, unless superseded by another attribute declaration deeper down.

Let's see how this works. The simplest kind of quad is a @code{block}. If we wrap our text in a @code{block} without attributes, what happens to the PDF?

@codeblock|{
#lang quadwriter
@block[#f]{Brennan and Dale like fancy sauce.}
}|

Right — nothing. A block without attributes just evaporates. Move the boundaries of the block:

@codeblock|{
#lang quadwriter
@block[#f]{Brennan and Dale} like fancy sauce.
}|

Still the same. Let's add some bold formatting with the @code{weight} attribute:

@codeblock|{
#lang quadwriter
@block['(weight bold)]{Brennan and Dale} like fancy sauce.
}|

What an accomplishment. To show you that attributes are additive, we'll put a quad inside our quad:

@codeblock|{
#lang quadwriter
@block['(weight bold)]{Brennan and @block['(color "red")]{Dale}} like fancy sauce.
}|

You're getting the idea. In terms of type styling, here are the attributes and values that Quad understands:

@code{'weight} = @code{'normal} (default) or @code{'bold}
@(linebreak)@code{'style} = @code{'normal} (default) or @code{'italic}
@(linebreak)@code{'font} = family name as string
@(linebreak)@code{'size} = point size as floating-point number
@(linebreak)@code{leading} = baseline-to-baseline measure in points
@(linebreak)@code{'color} = color string from @racket[color-database<%>]

Feel free to impose these on your demo program.

Though we're using @"@"-expressions, a @code{#lang quadwriter} source file doesn't imply any formatting characteristics as it would in Scribble or Pollen. For instance, see what happens if you add two line breaks and some more text:

@codeblock|{
#lang quadwriter
@block['(size 16)]{Brennan and @block['(color "red")]{Dale}} like fancy sauce.

Derek does not.
}|

The text ``Derek does not'' appears flush against the first sentence. In Scribble those linebreaks would suggest a paragraph break. In HTML they would suggest a word space. In @code{#lang quadwriter} they suggest neither. Why not? Because @code{#lang quadwriter} is strictly a language for describing explicit typesetting. Newlines have no meaning.

OK, so how do we create a paragraph? Quad supports a special set of quads called @italic{breaks} that move the typesetting position. For instance, @code{@"@"(block-break)} will act like a carriage return, moving the next typeset item below the previous item and all the way to the left edge of the column:

@codeblock|{
#lang quadwriter
@block['(size 16)]{Brennan and @block['(color "red")]{Dale}} like fancy sauce.
@(block-break)
Derek does not.
}|

Now ``Derek does not'' appears on its own line. What about things like paragraph spacing and first-line indents? Again, because @code{#lang quadwriter} is about explicit typesetting, all these things need to be inserted explicitly in the code. For instance, to make an indent, we add a @code{box} with a @code{'width} attribute:

@codeblock|{
#lang quadwriter
@block['(size 16)]{Brennan and @block['(color "red")]{Dale}} like fancy sauce.
@(block-break)
@box['(width 15)]
Derek does not.
}|

Quad also handles @code|{@(line-break)}|, @code|{@(column-break)}|, and @code|{@(page-break)}|. Try the last one:

@codeblock|{
#lang quadwriter
@block['(size 16)]{Brennan and @block['(color "red")]{Dale}} like fancy sauce.
@(page-break)
@box['(width 15)]
Derek does not.
}|

Next, let's look at Quad's linebreaking mechanism. For the next sample, please paste in a large block of plain text between the curly braces so you'll get some linewrapping:

@codeblock|{
#lang quadwriter
@block[#f]{A text that goes on for a while ...}
}|

You will see a block of justified text. The demo is running at maximum quality, so two other things will also be true. 

First, the lines are broken using the Knuth-Plass algorithm developed for TeX. This is a very nice algorithm that looks at all possible ways of breaking the lines in the paragraph and picks the one that leaves the smallest total gap at the right edge. (Quad will also hyphenate if necessary, but only if all the unhyphenated possibilities are bad.)

Second, notice that punctuation hangs a little outside the text block. This is an optical adjustment that makes for neater-looking blocks. Whether you literally care about this kind of optical adjustment is not the point. The point is that the Quad typesetting engine @italic{permits} it. And that is really what we are going for here: a hackable typesetting engine. When you have fine control over all the page elements, then other things become possible (for instance, mathematical-equation typesetting, which is quite a bit more involved than just hanging punctuation off the edges).

Justification is the default setting for the demo. To override this setting, use these attributes:

@code{'x-align} = @code{'justify} (default), @code{'left}, @code{'center}, or @code{'right}
@code{'x-align-last-line} = @code{'justify} (default), @code{'left}, @code{'center}, or @code{'right}


@codeblock|{
#lang quadwriter
@block['(x-align center x-align-last-line center)]{A text that goes on for a while ...}
}|

Then you can combine blocks with different styles:

@codeblock|{
#lang quadwriter
@block['(x-align center x-align-last-line center size 24)]{Very important headline}
@(block-break)
@block['(style italic)]{A subhead that maybe lasts a few lines}
@(block-break)
@box['(width 10)]@block[#f]{A text that goes on for a while ...}
}|

In sum, you can build up complex typesetting with a relatively small vocabulary of typesetting commands.

You are welcome to shovel large quantities of plain text into your @code{#lang quadwriter} window to see it broken into lines and paginated.

}


@section{Why is it called Quad?}

In letterpress printing, a @italic{quad} was a piece of metal used as spacing material within a line.