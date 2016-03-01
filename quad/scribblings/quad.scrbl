#lang scribble/manual

@(require (for-label racket/base racket/draw))

@title[#:style 'toc]{Quad: document processor}

@author[(author+email "Matthew Butterick" "mb@mbtype.com")]

@defmodulelang[quad]

@italic{This documentation explains software that is under development. It is therefore rife with optimistic claims and wishful thinking.}

@section{Installing Quad}

At the command line:
@verbatim{raco pkg install quad}

After that, you can update the package like so:
@verbatim{raco pkg update quad}

Quad is not usable software. It is currently in ``documented demo'' mode. There is no need to submit issues or bug reports. Fiddle with it at your own risk.


@section{Why care about document processors?}

A document processor is a rule-driven typesetter. It takes a text-based source file as input and converts it into a page layout. 

For instance, LaTeX is a document processor. So are web browsers. Quad borrows from both traditions — it's an attempt to modernize the good ideas in LaTeX, and generalize the good ideas in web browsers.

Document processors sit opposite WYSIWYG tools like Word and InDesign. There, the user controls the layout by manipulating a representation of the page on the screen. This is fine as far as it goes. But changes to the layout — for instance, a new page size — often require a new round of manual adjustments. 

A document processor, by contrast, relies on markup codes within the text to determine the layout programmatically. Compared to WYSIWYG, this approach offers less granular control. But it also creates a more flexible relationship between the source and its possible layouts. 

Another benefit of document processors is that it permits every document to have a high-level, text-based source file that's independent of any particular output format (rather than the opaque binary formats endemic to Word, InDesign, et al.)

@subsection{Why not keep using LaTeX?}

I wouldn't want to criticize software merely for being old. It's a great compliment to LaTeX that it's endured this long. But 1) it's never made much headway beyond its original audience of scientific & technical writers; 2) the last 25 years of advances in digital typesetting have been implemented as a huge (occasionally tenuous) tower of patches; 3) the core codebase is increasingly opaque to today's programmers. Meaning, if LaTeX were plausibly rewritable, it would've been rewritten by now. 

Instead, let's take its good ideas — there are many — and terraform a new planet.

@subsection{Why not use more HTML/CSS?}

In principle, it's possible to generate PDF documents from a web browser. Support for paper-based layouts has been part of the CSS concept @link["https://www.w3.org/People/howcome/p/cascade.html"]{since the beginning} (though it's been lightly used).

But web browsers have a few limitations. First, web browsers only render HTML, and many typesetting concepts (e.g., footnotes) don't correspond to any HTML entity. So there is a narrowing of possiblities. Second, browsers are built for speed, so high-quality typesetting (e.g., the Knuth–Plass linebreaking algorithm) is off the table. Third, browsers are  inconsistent in how they render pages. Fourth — taking off my typography-snob tiara here — browsers are unstable. What seems well supported today can be broken or removed tomorrow. So browsers can't be a part of a dependable publishing workflow that yields reproducible results.


@section{What does Quad do?}

Quad produces finished document layouts using three ingredients: 

@itemlist[#:style 'ordered
  @item{A @bold{markup-based language} for embedding high-level typesetting instructions in a text document. (Sort of like XML/HTML.)}

  @item{A @bold{typesetting engine} that converts these typesetting instructions into an output-independent layout — e.g., putting characters into lines, and lines into pages.}

  @item{A @bold{rendering engine} that takes this layout and prepares it for a particular output format (e.g., PDF, SVG).}
]

While there's no reason Quad couldn't produce an HTML layout, that's an easier problem, because most of the document-layout chores can (and should) be delegated to the web browser. For now, most of Quad's apparatus is devoted to its typesetting engine so it can produce layouts for PDF.

@section{What doesn't Quad do?}

@itemlist[#:style 'ordered
@item{Quad is not a WYSIWYG or interactive previewing tool.}

@item{Quad does not have user-level representations of formatting, à la Word style sheets.}

@item{Quad does not handle semantic or configurable markup. Its markup is limited to its specific, layout-based vocabulary.}
]
Rather, it is designed to cooperate with tools that offer these facilities.

@section{Theory of operation}

A document processor starts with input that we can think of as one giant line of text. It breaks this into smaller lines, and then distributes these lines across pages. Conceptually, it's a bin-packing problem.

@itemlist[#:style 'ordered
  @item{Quad starts with an input file written in the @code{#lang quad} markup language. For the most part, it's text with markup codes (though it may also include things like diagrams and images).}

  @item{Each markup entity is called a @defterm{quad}. A quad roughly corresponds to a box. ``Roughly'' because quads can have zero or negative dimension. Also, at the input stage, the contents of some quads may end up being spread across multiple non-overlapping boxes (e.g., a quad containing a word might be hyphenated to appear on two lines). The more precise description of a quad is therefore ``contiguous formatting region.'' Quads can be recursively nested inside other quads, thus the input file is tree-shaped.}

  @item{This tree-shaped input file is flattened into a list of atomic  quads. ``Atomic'' because these are the smallest items the typesetter can manipulate. (For instance, the word @italic{bar} would become three one-character quads. An image or other indivisible box would remain as is.) During the flattening, tags from higher in the tree are propagated downward by copying them into the atomic quads. The result is a ``stateless'' representation of the input, in the sense that all the information needed to typeset an atomic quad is contained within the quad itself.

  @margin-note{The input is flattened because typesetting operations are easier to think about as a linear sequence (i.e., an imperative model). To see why, consider how you'd handle a page-break instruction within a tree model. No matter how deep you were in your typesetting tree, you'd have to jump back to the top level to handle your page break (because it affects the positioning of all subsequent items). Then you'd have to jump back to where you were, deep in the tree. That's not a natural way to traverse any tree. This is also why, to my mind, typesetting does not lend itself to a class- or object-based approach, as these create hierarchies that just lead you back to this tree problem.}}

  @item{Atomic quads are composed into lines using one of three algorithms. (A line is just a quad of a certain width.) The first-fit algorithm puts as many quads onto a line as it can before moving on to the next. The best-fit algorithm minimizes the total looseness of all the lines in a paragraph (aka the Knuth–Plass linebreaking algorithm developed for TeX). Because best-fit is more expensive, Quad also has an adaptive-fit algorithm that uses a statistical heuristic to guess whether the paragraph will benefit from best-fit; if not, it uses first-fit.}

  @item{If a typeset paragraph still exceeds certain looseness tolerances, it is hyphenated and the lines recalculated.}

  @item{Once the lines are broken, extra space is distributed within each line according to whether the line should appear centered, left-aligned, justified, etc. The result is a list of quads that fills the full column width.}

  @item{Lines are composed into columns. (A column is just a quad of a certain height.) To support things like footnotes, columns are composed using a backtracking constraint-satisfaction algorithm.}

  @item{Columns are composed into pages.}

  @item{This completes the typesetting phase. Note that at every step in the process, the document is represented in the Quad markup language. There isn't a distinction between the public and private markup interface, or the high- and low-level markup entities. Thus, external tools that generate Quad markup have some latitude.}

  @item{Before the typeset markup is passed to the renderer, it goes through a simplification phase — a lot of adjacent quads will have the same formatting characteristics, and these can be consolidated into runs of text.}

  @item{The renderer walks through the markup and draws each quad, using information in the markup attributes to determine position, color, font, size, style, etc.}

]


@section{Enough talk — let's rock}

Open DrRacket and start a new document with @code{#lang quad} as the first line:

@codeblock|{
#lang quad
Brennan and Dale like fancy sauce.
}|

Save the document. Any place, any name is fine. 

Run the document. You'll get output like this:

@racketvalfont{@code{(block '(measure 360.0 font "Times New Roman" x-align justify leading 14.0 size 11.5 x-align-last-line left column-count 1 column-gutter 10.0) "\n" "Brennan and Dale like fancy sauce. ")}}

This is the compiled Quad markup, showing what will get sent to the typesetting engine. This output is itself valid Quad markup (meaning you could put it back in the definitions window and it would compile again).

@code{#lang quad} uses the @"@"-expression reader for ease of use. But these @"@"-expressions become S-expressions in the usual manner. Also as usual, you can prefix any S-expressionized Quad markup with a @litchar{@"@"} in the definitions window to turn it back into an @"@"-expression.

@margin-note{@secref["how-to:reader" #:doc '(lib "scribblings/scribble/scribble.scrbl")] introduces @"@"-expressions.}

Now click the @onscreen{Render and Open PDF} button. After a moment, this should open your PDF previewing program with the Quad-generated PDF, which will say, unsurprisingly, ``Brennan and Dale like fancy sauce.''

As you work through the demo, you can alternatively use the @onscreen{Render PDF} button to regenerate the PDF without opening your previewer. (The Preview app on OS X, for instance, will automatically refresh when it detects the PDF has changed, which prevents a welter of windows.)

@code{#lang quad} is a Racket-implemented DSL (= domain-specific language). It's not a language in the sense of Turing-complete. Rather, a @code{#lang quad} ``program'' resembles text annotated with high-level layout-description commands (not unlike XML/HTML). @code{#lang quad} programs can be written directly, or generated as the output of other programs.

Each @"@"-expression in @code{#lang quad} is interpreted as a @italic{quad} (roughly a box; more precisely a contiguous formatting region). A quad has the following syntax:

@code|{@quad-name[(list 'attr-name attr-val ...)]{text-or-more-quads ...}}|

The @code{(list 'attr-name attr-val ...)} is an interleaved list of symbols and values, as you might provide to @racket[hash]. The attribute list is mandatory. If a quad has no attributes of its own, this can be signaled with either @racket[empty] or @racket[#f]:

@codeblock|{
@quad-name[empty]{text-or-more-quads ...}
@quad-name[#f]{text-or-more-quads ...}
}|

If you thought this resembled an @link["http://docs.racket-lang.org/pollen/second-tutorial.html#%28part._.X-expressions%29"]{X-expression}, you wouldn't be wrong. Like X-expressions, quads are recursively composable. Also like X-expressions, the attributes in a quad apply to all the text or quads within, unless superseded by another attribute declaration deeper down.

Let's see how this works. The simplest kind of quad is a @code{block}. If we wrap our text in a @code{block} without attributes, what happens to the PDF?

@codeblock|{
#lang quad
@block[#f]{Brennan and Dale like fancy sauce.}
}|

Right — nothing. A block without attributes just evaporates. Move the boundaries of the block:

@codeblock|{
#lang quad
@block[#f]{Brennan and Dale} like fancy sauce.
}|

Still the same. Let's add some bold formatting with the @code{weight} attribute:

@codeblock|{
#lang quad
@block['(weight bold)]{Brennan and Dale} like fancy sauce.
}|

What an accomplishment. To show you that attributes are additive, we'll put a quad inside our quad:

@codeblock|{
#lang quad
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

Though we're using @"@"-expressions, a @code{#lang quad} source file doesn't imply any formatting characteristics as it would in Scribble or Pollen. For instance, see what happens if you add two line breaks and some more text:

@codeblock|{
#lang quad
@block['(size 16)]{Brennan and @block['(color "red")]{Dale}} like fancy sauce.

Derek does not.
}|

The text ``Derek does not'' appears flush against the first sentence. In Scribble those linebreaks would suggest a paragraph break. In HTML they would suggest a word space. In @code{#lang quad} they suggest neither. Why not? Because @code{#lang quad} is strictly a language for describing explicit typesetting. Newlines have no meaning.

OK, so how do we create a paragraph? Quad supports a special set of quads called @italic{breaks} that move the typesetting position. For instance, @code{@"@"(block-break)} will act like a carriage return, moving the next typeset item below the previous item and all the way to the left edge of the column:

@codeblock|{
#lang quad
@block['(size 16)]{Brennan and @block['(color "red")]{Dale}} like fancy sauce.
@(block-break)
Derek does not.
}|

Now ``Derek does not'' appears on its own line. What about things like paragraph spacing and first-line indents? Again, because @code{#lang quad} is about explicit typesetting, all these things need to be inserted explicitly in the code. For instance, to make an indent, we add a @code{box} with a @code{'width} attribute:

@codeblock|{
#lang quad
@block['(size 16)]{Brennan and @block['(color "red")]{Dale}} like fancy sauce.
@(block-break)
@box['(width 15)]
Derek does not.
}|

Quad also handles @code|{@(line-break)}|, @code|{@(column-break)}|, and @code|{@(page-break)}|. Try the last one:

@codeblock|{
#lang quad
@block['(size 16)]{Brennan and @block['(color "red")]{Dale}} like fancy sauce.
@(page-break)
@box['(width 15)]
Derek does not.
}|

Next, let's look at Quad's linebreaking mechanism. For the next sample, please paste in a large block of plain text between the curly braces so you'll get some linewrapping:

@codeblock|{
#lang quad
@block[#f]{A text that goes on for a while ...}
}|

You will see a block of justified text. The demo is running at maximum quality, so two other things will also be true. 

First, the lines are broken using the Knuth-Plass algorithm developed for TeX. This is a very nice algorithm that looks at all possible ways of breaking the lines in the paragraph and picks the one that leaves the smallest total gap at the right edge. (Quad will also hyphenate if necessary, but only if all the unhyphenated possibilities are bad.)

Second, notice that punctuation hangs a little outside the text block. This is an optical adjustment that makes for neater-looking blocks. Whether you literally care about this kind of optical adjustment is not the point. The point is that the Quad typesetting engine @italic{permits} it. And that is really what we are going for here: a hackable typesetting engine. When you have fine control over all the page elements, then other things become possible (for instance, mathematical-equation typesetting, which is quite a bit more involved than just hanging punctuation off the edges).

Justification is the default setting for the demo. To override this setting, use these attributes:

@code{'x-align} = @code{'justify} (default), @code{'left}, @code{'center}, or @code{'right}
@code{'x-align-last-line} = @code{'justify} (default), @code{'left}, @code{'center}, or @code{'right}


@codeblock|{
#lang quad
@block['(x-align center x-align-last-line center)]{A text that goes on for a while ...}
}|

Then you can combine blocks with different styles:

@codeblock|{
#lang quad
@block['(x-align center x-align-last-line center size 24)]{Very important headline}
@(block-break)
@block['(style italic)]{A subhead that maybe lasts a few lines}
@(block-break)
@box['(width 10)]@block[#f]{A text that goes on for a while ...}
}|

In sum, you can build up complex typesetting with a relatively small vocabulary of typesetting commands.

You are welcome to shovel large quantities of plain text into your @code{#lang quad} window to see it broken into lines and paginated.


@section{Bottlenecks, roadblocks, & unanswered questions}

In no particular order.

@itemlist[#:style 'ordered

@item{@bold{Flattening is wasteful.} Exploding the input into atomic quads and copying the attributes works, but it creates an enormous data structure with a huge amount of repetition. But, how do you create a stateless representation of the input? 

@italic{Possible improvements}: Put the attributes into a separate data structure that treats each attribute as having a scope. But this makes editing the input data more difficult & fragile, because you have two parallel structures to keep sychronized. Also, there's probably no reason that the attributes have to allow arbitrary key–value pairs. If the keys and certain values were reduced to a fixed vocabulary, they could be encoded as (smaller, quicker) integers rather than symbols and strings.}

@item{@bold{Allocation is wasteful.} Many typesetting operations break bigger quads into smaller ones, or group smaller quads into bigger ones, etc. The result is that there's a lot of allocation & garbage collection relative to the typical Racket program.

@italic{Possible improvements}: Perhaps the input can be fixed some structure and results of each typesetting operation stored as a set of edits (like a diff) rather than copying the whole structure.}

@item{@bold{Pango text measuring is slow.} The most cumulatively expensive operation is measuring text so linebreaks can be calculated. @racketmodname[racket/draw] relies on Pango, which is fine for occasional UI stuff, but not zillions of lookups. (BTW Pango does have higher-level text-layout facilities which are of course faster than measuring characters individually. But the point of Quad is to micromanage the typesetting and thereby make things possible that are not in Pango.) 

@italic{Possible improvements}: First, use the FFI to measure text through the underlying FreeType library. This is a lot faster, but costs some functionality. Second, better caching (but see next note). 
}

@item{@bold{Caching is tricky.} Caching is an essential ingredient in a text-rendering system because so many measurements are reused. Two hard parts, however. First, simplifying the key logic so you don't end up with immensely huge hashtables with commensurately costly lookups. Second, preserving caches between runs of the program. Sure, save it on disk, but a giant hashtable in a .rktd file is still going to take a moment to be reconstituted into memory.

@italic{Possible improvements}: Rely on disk-based hashtables, i.e., cache files that can be read & updated without having to reconstitute the whole file into a RAM-resident hashtable, and then write it all out again. I'm sure someone figured this out in 1972, I just haven't researched it yet.}


@item{@bold{Cairo's PDFs are weak.} Cairo's PDF generator is missing key features  (e.g., @link["http://cairographics.org/roadmap/"]{hyperlinks}) and in general makes PDFs that are bigger and less capable than, say, @tt{tex2pdf}. Since PDFs are undoubtedly the #1 target format for a document processor, this is a major liability. OTOH, the idea of writing a PostScript/PDF compiler is, for me anyhow, daunting.

@italic{Possible improvements}: Bite the bullet and make a PDF compiler. If one wants to be free of LaTeX, and have better-quality PDFs than Cairo allows, there's not really a second option.}

@item{@bold{Overall performance is slow.} Outside of text measurement, most of Quad consists of simple mathematical operations. It seems like it should be highly optimizable. (Using Typed Racket, however, wasn't the answer.)

@italic{Possible improvements}: Use more unsafe math operations, gingerly.}


@item{@bold{Dependencies are broad.} One reason switching to Typed Racket did nothing for Quad is that it touches a lot of other pieces of Racket. In TR's case, creating typed interfaces for untyped libraries consumed all the potential performance gains from static typing. But still, using a small slice of a lot of libraries adds a certain overhead.}

@item{@bold{Glyph shaping is nowhere.} A proper 21st-century typesetting engine needs OpenType glyph shaping, and the only open-source game in town is @link["https://www.freedesktop.org/wiki/Software/HarfBuzz/"]{HarfBuzz}. Haven't used it, don't know how to integrate it.}

@item{@bold{Parallel processing is difficult.} It's unclear to me how to exploit Racket's parallel-processing facilities to speed up typesetting. A typeset document is likely to have a lot of finely interdependent pieces (e.g., table of contents, table of authorities, footnotes, etc.)} 

@item{@bold{Run-to-run caching is difficult.} By this I mean that a common workflow in typesetting is to edit the document, preview the typesetting, make adjustments, preview again, etc. At each step, potentially not that much of the document is changing. But the typesetter needs to run start to finish anyhow.

@italic{Possible improvements}: The most expensive operation is linebreaking. It would be nice to find a way to cache linebreaking between runs — e.g., ``this paragraph hasn't changed, so we can just reuse the linebreaks from last time.'' But this would require some kind of checksumming of each paragraph and disk caching, which itself would get expensive.} 

]

@section{Why is it called Quad?}

In letterpress printing, a @italic{quad} was a piece of metal used as spacing material within a line.