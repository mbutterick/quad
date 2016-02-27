#lang scribble/manual

@title[#:style 'toc]{Quad: document processor}

@author[(author+email "Matthew Butterick" "mb@mbtype.com")]

@defmodule[quad]

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
  @item{A @bold{markup-based language} for embedding high-level typsetting instructions in a text document. (Sort of like XML/HTML.)}

  @item{A @bold{layout engine} that converts these typesetting instructions into an output-independent layout — e.g., putting characters into lines, and lines into pages.}

  @item{A @bold{rendering engine} that takes this layout and prepares it for a particular output format (e.g., PDF, SVG).}
]

While there's no reason Quad couldn't produce an HTML layout, that's an easier problem, because most of the document-layout chores can (and should) be delegated to the web browser. For now, most of Quad's apparatus is devoted to its layout engine so it can produce layouts for PDF.

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

  @item{Each markup entity is called a @defterm{quad}. A quad roughly corresponds to a box. ``Roughly'' because quads can have zero or negative dimension. Also, at the input stage, the contents of some quads may end up being spread across multiple non-overlapping boxes (e.g., a quad containing a word might be hyphenated to appear on two lines). The more precise description of a quad is therefore ``contiguous markup region.'' Quads can be recursively nested inside other quads, thus the input file is tree-shaped.}

  @item{This tree-shaped input file is flattened into a list of atomic  quads. ``Atomic'' because these are the smallest items the typesetter can manipulate. (For instance, the word @italic{bar} would become three one-character quads. An image or other indivisible box would remain as is.) During the flattening, tags from higher in the tree are propagated downward by copying them into the atomic quads. The result is a ``stateless'' representation of the input, in the sense that all the information needed to typeset an atomic quad is contained within the quad itself.}

  @item{Atomic quads are composed into lines using one of three algorithms. (A line is just a quad of a certain width.) The first-fit algorithm puts as many quads onto a line as it can before moving on to the next. The best-fit algorithm minimizes the total looseness of all the lines in a paragraph (aka the Knuth–Plass linebreaking algorithm developed for TeX). Because best-fit is more expensive, Quad also has an adaptive-fit algorithm that uses a statistical heuristic to guess whether the paragraph will benefit from best-fit; if not, it uses first-fit.}

  @item{If a typeset paragraph still exceeds certain looseness tolerances, it is hyphenated and the lines recalculated.}

  @item{Once the lines are broken, extra space is distributed within each line according to whether the line should appear centered, left-aligned, justified, etc. The result is a list of quads that fills the full column width.}

  @item{Lines are composed into columns. (A column is just a quad of a certain height.) To support things like footnotes, columns are composed using a backtracking constraint-satisfaction algorithm.}

  @item{Columns are composed into pages.}

  @item{This completes the typesetting phase. Note that at every step in the process, the document is represented in the Quad markup language. There isn't a distinction between the public and private markup interface, or the high- and low-level markup entities. Thus, external tools that generate Quad markup have some latitude.}

  @item{Before the typeset markup is passed to the renderer, it goes through a simplification phase — a lot of adjacent quads will have the same formatting characteristics, and these can be consolidated into runs of text.}

  @item{The renderer walks through the markup and draws each quad, using information in the markup attributes to determine position, color, font size & style, etc.}

]


@section{The markup language}

@defmodulelang[quad]

Quad's markup language is a Racket-implemented DSL (= domain-specific language). It's not a language in the sense of Turing-complete. Rather, a Quad ``program'' resembles text annotated with high-level layout-description commands (not unlike XML/HTML). 

Quad programs can be written directly, or generated as the output of other programs.






@section{The layout engine}

@section{The rendering engine}


@section{Bottlenecks, roadblocks, irritations}

@section{Why is it called Quad?}

In letterpress printing, a @italic{quad} was a piece of metal used as spacing material within a line.