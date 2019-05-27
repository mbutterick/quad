#lang scribble/manual

@(require racket/runtime-path scribble/example quadwriter (for-label txexpr (except-in pollen #%module-begin) xml racket/base racket/draw quadwriter)
pollen/scribblings/mb-tools quad/pict)

@(define my-eval (make-base-eval))
@(my-eval `(require quad quad/pict))


@title[#:style 'toc]{Quad: document processor}

@author[(author+email "Matthew Butterick" "mb@mbtype.com")]

@defmodule*[(quad quadwriter)]

@defmodulelang*[(quadwriter
quadwriter/markdown
quadwriter/markup)]

@(define-runtime-path quads.png "quads.png")
@(image quads.png #:scale 0.4)

@margin-note{Quad is in progress. It works, but it is unstable — I am still changing things, small and large — and thus I make no commitment to maintain the API in its current state.}


@section{Installing Quad & Quadwriter}

At the command line:
@verbatim{raco pkg install quad}

After that, you can update the package like so:
@verbatim{raco pkg update quad}

@racketmodname[quadwriter] is installed as part of the @racketmodname[quad] package.

Much of the font-parsing and PDF-rendering code in Quad is adapted from @link["http://github.com/foliojs/"]{FolioJS} by Devon Govett. I thank Mr. Govett for figuring out a lot of details that would've made me squeal in agony. 

@section{What is Quad?}

A document processor, which means that it:

@itemlist[#:style 'ordered
  @item{Computes the layout of your document from a series of formatting codes (not unlike a web browser)}

  @item{Renders to PDF (not unlike a word processor).}
]

For instance, LaTeX is a document processor. So are web browsers. Quad borrows from both traditions — it's an attempt to modernize the good ideas in LaTeX, and generalize the good ideas in web browsers, while bypassing some of the limitations of LaTeX (e.g., no Unicode) and of web browsers (e.g., performance and error recovery are valued above all).

Document processors sit opposite WYSIWYG tools like Microsoft Word and Adobe InDesign. There, the user controls the layout by manipulating a representation of the page on the screen. This is fine as far as it goes. But changes to the layout — for instance, a new page size — often require a new round of manual adjustments. 

A document processor, by contrast, relies on markup codes within the text to determine the layout programmatically. Compared to WYSIWYG, this approach offers less granular control. But it also creates a more flexible relationship between the source and its possible layouts. 

Another benefit of document processors is that it permits every document to have a high-level, text-based source file that's independent of any particular output format.



@subsection{How does Quad work?}

Quad produces PDFs using three ingredients: 

@itemlist[#:style 'ordered
  @item{A @bold{font engine} that handles glyph shaping and positioning using standard TTF or OTF font files.}

  @item{A @bold{layout engine} that converts typesetting instructions into an output-independent layout — e.g., putting characters into lines, and lines into pages.}

  @item{A @bold{PDF engine} that takes this layout and renders it as a finished PDF file.}
]


For the most part, neither Quad nor Quadwriter rely much on @racketmodname[racket/draw], and completely avoid its PDF-drawing functions. These facilities are provided by @link["https://www.pango.org/"]{Pango}, which has some major shortcomings in the kind of PDFs it produces (for instance, it doesn't support hyperlinks).


@section{What is Quadwriter?}

A demo app built with Quad. It takes a text-based source file as input, calculates the typesetting and layout, and then outputs a PDF.

You can fiddle with it & then submit issues and feature requests at the @link["http://github.com/mbutterick/quad"]{Quad repo}. 


@section{Quadwriter quick tour}

Open DrRacket (or whatever editor you prefer) and start a new document with @code{#lang quadwriter/markdown} as the first line:


@fileblock["test.rkt"
@codeblock|{
#lang quadwriter/markdown
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

You will see the actual input to Quadwriter, which is called a @tech{Q-expression}:

@repl-output{
'(q () (q ((page-margin-left "120") (page-margin-top "80") (page-margin-bottom "120") (font-family "text") (line-height "17")) (q ((keep-first-lines "2") (keep-last-lines "3") (font-size-adjust "100%") (character-tracking "0") (hyphenate "true") (display "g49598")) "Brennan and Dale like fancy sauce.")))
}

In the demos that follow, the input language will change slightly. But the PDF will be rendered the same way (by running the source file) and you can always look at @racket[doc] or use @racket[view-result].


@subsection{Quadwriter & Markdown}

I @link["https://docs.racket-lang.org/pollen/second-tutorial.html#%28part._the-case-against-markdown%29"]{don't recommend} that writers adopt Markdown for serious projects. But for goofing around, why not.

Our first version of @racket["test.rkt"] had one line of plain text:

@fileblock["test.rkt"
@codeblock|{
#lang quadwriter/markdown
Brennan and Dale like fancy sauce.
}|
]

Behind the scenes, @racketmodname[quadwriter/markdown] is doing more heavy lifting than this sample suggests. We can type our source in Markdown notation, and it will automatically be converted to the appropriate Quad formatting commands to make things look right. 

For instance, try this sample, which combines a Markdown heading, bullet list, code block, and bold and italic formatting:

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

You're welcome to paste in bigger Markdown files that you have laying around and see what happens. As a demo language, I'm sure there are tortured agglomerations of Markdown notation that will confuse @racketmodname[quadwriter/markdown]. But vanilla files should be fine.

Back to the demo. Curious characters can do this:

@terminal{
> doc
}

To see this:

@repl-output{
'(q
  ()
  (q
   ((page-margin-left "120") (page-margin-top "80") (page-margin-bottom "120") (font-family "text") (line-height "17"))
   (q ((break "para")))
   (q ((font-family "heading") (first-line-indent "0") (display "block") (font-size "20") (line-height "24.0") (border-width-top "0.5") (border-inset-top "9") (inset-bottom "-3") (inset-top "6") (keep-with-next "true") (id "did-you-know")) "Did you know?")
   ···
}

This is the first part of the @tech{Q-expression} that the source file produces when it runs and exports via @racket[doc]. This Q-expression is passed to Quadwriter for layout and rendering.

@margin-note{Yes, you can generate your own Q-expressions by other means and pass them to @racketmodname[quadwriter] for layout & rendering. See @racket[render-pdf].}

@subsection{Quadwriter & markup}

Suppose Markdown is just not your thing. You prefer to enter your markup the old-fashioned way — by hand. I hear you. So let's switch to the @racket[quadwriter/markup] dialect. First we try our simple test:

@fileblock["test.rkt"
@codeblock|{
#lang quadwriter/markup
Brennan and Dale like fancy sauce.
}|
]

We get the same PDF result as before, again because a short line of plain text is the same in this dialect as the last.

But if we want to reproduce the result of the Markdown notation, this time we use the equivalent HTML-ish markup tags:

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

The special @litchar{◊} character is called a @deftech{lozenge}. It introduces markup tags. @link["https://docs.racket-lang.org/pollen/pollen-command-syntax.html#%28part._the-lozenge%29"]{Instructions for typing it}, but for now it suffices to copy & paste, or use the @onscreen{Insert Command Char} button in the DrRacket toolbar.

Under the hood, the @racketmodname[quadwriter/markdown] dialect is converting the Markdown surface notation into markup tags that look like this. So the @racket[quadwriter/markup] dialect just lets us start with those tags. 

Curious characters can prove that this is so by again typing at the REPL:

@terminal{
> doc
}

This @tech{Q-expression} is exactly the same as the one that resulted with the @racketmodname[quadwriter/markdown] source file.

@subsection{Quadwriter & Q-expressions}

@racketmodname[quadwriter/markdown] showed high-level notation (= a generous way of describing Markdown) that generated a Q-expression. Then @racketmodname[quadwriter/markup] showed a mid-level notation that generated another (identical) Q-expression.

If we wish, we can also skip the notational foofaraw and just write Q-expressions directly in our source file. We do this with the basic @racketmodname[quadwriter] language. 

Recall our very first example:

@fileblock["test.rkt"
@codeblock|{
#lang quadwriter/markup
Brennan and Dale like fancy sauce.
}|
]

In the REPL, the @racket[doc] was this Q-expression:

@repl-output{
'(q () (q ((page-margin-left "120") (page-margin-top "80") (page-margin-bottom "120") (font-family "text") (line-height "17")) "Brennan and Dale like fancy sauce."))
}

Let's copy this Q-expression and use it as our new source code. This time, however, we'll switch to plain @code{#lang quadwriter} (instead of the @racket[markup] or @racket[markdown] dialects):

@fileblock["test.rkt"
@codeblock|{
#lang quadwriter
'(q () (q ((page-margin-left "120") (page-margin-top "80") 
(page-margin-bottom "120") (font-family "text") 
(line-height "17")) "Brennan and Dale like fancy sauce."))
}|
]

This produces the same one-line PDF as before.

Likewise, we can pick up the @racket[doc] from our more complex example:


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


And again, use the resulting Q-expression in @racket[doc] as the source for a new @racket[quadwriter] program, which will result in the same PDF.

@subsection{Setting top-level attributes}

Even if you're using a @racketmodname[quadwriter] dialect, you can still set top-level formatting attributes for the document. For instance, suppose we wanted to make our original @racketmodname[quadwriter/markdown] example 24 points and red, and put the PDF on wide tabloid (17in × 11in) paper. We can add these top-level attributes to the beginning of our source file as keyword arguments:

@fileblock["test.rkt"
@codeblock|{
#lang quadwriter/markdown

#:page-size tabloid
#:page-orientation wide
#:font-size 18
#:font-color red

Brennan and Dale like fancy sauce.
}|
]

Any of the @secref{Markup} attributes documented below can be used as keyword arguments. The syntax follows the pattern above: one attribute + value pair per line, with the attribute prefixed with @litchar{#:} to make it a keyword, and the value unquoted. 

This keyword syntax works in the @racketmodname[quadwriter], @racketmodname[quadwriter/markdown], and @racketmodname[quadwriter/markup] languages. The idea is to make it easy to adjust the default layout behavior without going outside the source file.


@subsection{Invoking Quadwriter as a library}

Part of the idea of @racketmodname[quad] and @racketmodname[quadwriter] is to make typographic layout & PDF generation a service that can be built into other Racket apps and languages. 

Let's see how this works by doing document layout and rendering from within good old @racketmodname[racket/base]:

@fileblock["test.rkt"
@codeblock|{
#lang racket/base
(require quadwriter)
(define qx `(q "Brennan likes fancy sauce."
                ,para-break
                "Dale hates fancy sauce."))
(define pdf-path "~/Desktop/new.pdf")
(render-pdf qx pdf-path)
}|
]

Here, we create a little @tech{Q-expression}, which we pass to @racket[render-pdf] with a @racket[pdf-path] argument.

@subsection{Combining Quadwriter with Pollen}

Fans of @racketmodname[pollen] might be glad to hear that @racketmodname[quadwriter] can be used to handle layout and PDF rendering for Pollen source files. As usual we start with a Pollen source file, this time with the @racket[pdf.pm] extension to indicate that it's a Pollen markup file that will produce a PDF:

@fileblock["test.pdf.pm"
@codeblock|{
#lang pollen

Brennan likes fancy sauce.

Dale hates fancy sauce.

}|
]

Then we add a simple @racket["pollen.rkt"] that converts the output of our source file into a @tech{Q-expression}:

@fileblock["pollen.rkt"
@codeblock|{
#lang racket
(require pollen/decode quadwriter)
(provide root render-pdf)
 
(define (root . xs)
  `(q ,@(add-between (decode-paragraphs xs 'q) para-break)))
}|
]

All we're doing here is wrapping our paragraphs in @racket[q] tags (rather than the default @racket[p] tags) and then adding explicit Quadwriter paragraph breaks between them (see @racket[para-break]).

Finally, we add a @racket["template.pdf.p"] that passes the @racket[doc] from the Pollen source to @racket[render-pdf]:

@fileblock["template.pdf.p"
@codeblock|{
◊(render-pdf doc #false)
}|
]

In this case, we pass @racket[#false] as the path argument to @racket[render-pdf] so that it returns the actual bytes, which the Pollen renderer will put in the right place.

You can fire up the Pollen project server and see how this works. As usual with Pollen sources, when you make changes to the source file, the rendered PDF will be dynamically updated.


@margin-note{Though a @racketmodname[quadwriter] source file and a @racketmodname[pollen] source file both export something called @racket[doc], these exports don't share any deeper connection. (The name was chosen to be consistent with Scribble, which also exports a @racket[doc].)}

@subsection{Quick tour complete}

In the usual Racket tradition, @racket[quadwriter] and its dialects are just compiling a document from a higher-level representation to a lower-level representation. 

If you're a writer, you might prefer to use the high-level representation (like @racketmodname[quadwriter/markdown]) so that your experience is optimized for ease of use.

If you're a developer, you might prefer to use the lower-level representation for precision. For instance, a @racketmodname[pollen] author who wanted to generate a PDF could design tag functions that emit Q-expressions, and then pass the result to @racket[render-pdf].

Or, you can aim somewhere in between. Like everything else in Racket, you can design functions & macros to emit the pieces of a Q-expression using whatever interface you prefer. 


@section{Quadwriter: developer guide}

@defthing[doc qexpr?]{
Every source file written in a @racketmodname[quadwriter] dialect exports an identifier called @racket[doc] that contains the @tech{Q-expression} that results from running the source.
}

@subsection{Q-expressions}

A Q-expression is an @seclink["X-expressions" #:doc '(lib "pollen/scribblings/pollen.scrbl")]{X-expression}, but more restricted:

@racketgrammar[
#:literals (list q)
qexpr string
      (list q (list (list attr-name attr-val) ...) qexpr ...)
      (list q (list qexpr ...))
]

This grammar means that a Q-expression is either a) a string, b) an X-expression whose tag is @racket[q] and whose elements are themselves Q-expressions.

@examples[#:eval my-eval
(qexpr? "Hello world")
(qexpr? '(q "Hello world"))
(qexpr? '(q () "Hello world"))
(qexpr? '(q ((font-color "pink")) "Hello world"))
(qexpr? '(q ((font-color "pink")) (q "Hello world")))
(code:comment @#,t{malformed Q-expressions})
(qexpr? 42)
(qexpr? '(div "Hello world"))
(qexpr? '(q (("pink" font-color)) "Hello world"))
] 

Because Q-expressions are a subset of X-expressions, you can apply any tools that work with X-expressions (for instance, the @racketmodname[txexpr] library).

@margin-note{Unlike X-expressions, Q-expressions do not support character entities or CDATA, because those are inherent to XML-ish markup.}



@subsection{Markup}

@subsection{Hard breaks}


@deftogether[(@defthing[line-break qexpr?]
@defthing[page-break qexpr?])]{
The Q-expressions @racketresult['#,line-break] and @racketresult['#,page-break], respectively. Quadwriter will automatically insert line breaks and page breaks as needed. But you can also add them explicitly (aka ``hard'' breaks) by inserting the Q-expression denoting the break.
}


@defthing[para-break qexpr?]{
The Q-expression @racketresult['#,para-break]. Used to denote the start of a new paragraph.
}


@subsection{Attributes}

These are the attributes that can be used inside a Q-expression passed to @racketmodname[quadwriter]. Inside a Q-expression, every attribute is a @tech{symbol}, and every attribute value is a @tech{string}.

A @deftech{dimension string} represents a distance in the plane. If unitless, it is treated as points (where 1 point = 1/72 of an inch). If the number has @racket[in], @racket[cm], or @racket[mm] as a suffix, it is treated as inches, centimeters, or millimeters respectively.


@subsubsection{Page-level attributes}

@deftogether[(@defthing[#:kind "attribute" page-size symbol?]
              @defthing[#:kind "attribute" page-orientation symbol?])]{
The usual way of setting the overall page dimensions of the rendered PDF. The value of @racket[page-size] is a @tech{named page size}. The value of @racket[page-orientation] can be either @racket["tall"] or @racket["portrait"] (which both put the longer edge vertically) or @racket["wide"] or @racket["landscape"] (which put the longer edge horizontally).
}


@deftogether[(@defthing[#:kind "attribute" page-width symbol?]
              @defthing[#:kind "attribute" page-height symbol?])]{
The unusual way of setting the overall page dimensions of the rendered PDF. Both values are given as a @tech{dimension string}.
}

@deftogether[(@defthing[#:kind "attribute" page-margin-top symbol?]
              @defthing[#:kind "attribute" page-margin-bottom symbol?]
              @defthing[#:kind "attribute" page-margin-left symbol?]
              @defthing[#:kind "attribute" page-margin-right symbol?])]{
Inset values from the page edges. Value is given as a @tech{dimension string}. Default values depend on size of the page: they are chosen to be not completely bananas.
}

@deftogether[(@defthing[#:kind "attribute" column-count symbol?]
              @defthing[#:kind "attribute" column-gap symbol?])]{
Columns per page. @racket[column-count] is a positive integer; @racket[column-gap] (the space between columns) is a @tech{dimension string}.
}

@subsubsection{Block-level attributes}

A block is a paragraph or other rectangular item (say, a blockquote or code block) with paragraph breaks around it.

@deftogether[(@defthing[#:kind "attribute" inset-top symbol?]
              @defthing[#:kind "attribute" inset-bottom symbol?]
              @defthing[#:kind "attribute" inset-left symbol?]
              @defthing[#:kind "attribute" inset-right symbol?])]{
Inset values increase the layout boundary of the quad. Value is given as a @tech{dimension string}. @racket["0"] by default.
}

@deftogether[(@defthing[#:kind "attribute" border-inset-top symbol?]
              @defthing[#:kind "attribute" border-inset-bottom symbol?]
              @defthing[#:kind "attribute" border-inset-left symbol?]
              @defthing[#:kind "attribute" border-inset-right symbol?])]{
Border-inset values do not change the layout boundary of the quad. Rather, they change the position of the border (if any) relative to the layout boundary. Value is given as a @tech{dimension string}. @racket["0"] by default (meaning, the border sits on the layout boundary).
}

@deftogether[(@defthing[#:kind "attribute" border-width-top symbol?]
              @defthing[#:kind "attribute" border-width-bottom symbol?]
              @defthing[#:kind "attribute" border-width-left symbol?]
              @defthing[#:kind "attribute" border-width-right symbol?])]{
Width of the border on each edge of the quad. Value is given as a @tech{dimension string}. @racket["0"] by default (meaning no border).
}

@deftogether[(@defthing[#:kind "attribute" border-color-top symbol?]
              @defthing[#:kind "attribute" border-color-bottom symbol?]
              @defthing[#:kind "attribute" border-color-left symbol?]
              @defthing[#:kind "attribute" border-color-right symbol?])]{
Color of the border on each edge of the quad. Value is a @tech{hex color} string or @tech{named color} string.
}

@defthing[#:kind "attribute" background-color symbol?]{
Color of the background of the quad. Value is a @tech{hex color} string or @tech{named color} string.
}

@deftogether[(@defthing[#:kind "attribute" keep-first-lines symbol?]
              @defthing[#:kind "attribute" keep-last-lines symbol?]
              @defthing[#:kind "attribute" keep-all-lines symbol?])]{
How many lines of the quad are kept together near a page break. @racket[keep-first-lines] sets the minimum number of lines that appear before a page break; @racket[keep-last-lines] sets the minimum number that appear after. In bother cases, they take a non-negative integer string as a value.

@racket[keep-all-lines] keeps all the lines of a quad on the same page. Activated only when value is @racket["true"]. Be careful with this option — it's possible to make a single quad that is longer than one page, in which case @racketmodname[quadwriter] will ignore the setting to prevent an impossible situation.
}

@defthing[#:kind "attribute" keep-with-next symbol?]{
Whether a quad appears on the same page with the following quad. Activated only when value is @racket["true"]. Essentially this is the ``nonbreaking paragraph space''.
}

@deftogether[(@defthing[#:kind "attribute" line-align symbol?]
              @defthing[#:kind "attribute" line-align-last symbol?])]{
How the lines are aligned horizontally in the quad. Possibilities are @racket["left"], @racket["center"], @racket["left"], and @racket["justify"]. @racket[line-align-last] controls the alignment of the last line; @racket[line-align] controls the others.
}

@defthing[#:kind "attribute" first-line-indent symbol?]{
The indent of the first line in the quad. Value is given as a @tech{dimension string}.
}


@defthing[#:kind "attribute" line-wrap symbol?]{
Selects the linebreak algorithm. A value of @racket["best"] or @racket["kp"] invokes the @link["http://defoe.sourceforge.net/folio/knuth-plass.html"]{Knuth–Plass linebreaking algorithm}, which finds the optimal set of linebreaks (defined as the set that gives the most even spacing throughout the paragraph). Otherwise, you get the ordinary linebreak algorithm, which just puts as many words as it can on each line. The Knuth–Plass algorithm is slower, of course.
}

@defthing[#:kind "attribute" hyphenate symbol?]{
Whether the block is hyphenated. Activated only when value is @racket["true"]. 
}

@defthing[#:kind "attribute" clip symbol?]{
Whether the contents of the block are clipped to its boundary. Activated only when value is @racket["true"]. 
}

@subsubsection{Other attributes}

@deftogether[(@defthing[#:kind "attribute" font-size symbol?]
              @defthing[#:kind "attribute" font-size-adjust symbol?])]{
Two ways of setting the point size for text. @racket[font-size] takes a size string. @racket[font-size-adjust] takes a string representing a percentage (like @racket["120%"] or @racket["1.2"]) and sets the font size to the size of the parent, multiplied by the percentage.
}

@defthing[#:kind "attribute" font-family symbol?]{
Name of the font family. Value is a string with the font-family name. See @secref["Fonts"] for where these names come from.
}

@defthing[#:kind "attribute" font-color symbol?]{
The color of the rendered font. Value is a @tech{hex color} string or @tech{named color} string.
}

@defthing[#:kind "attribute" font-bold symbol?]{
Whether the quad has bold styling applied. Activated only when value is @racket["true"]. 
}

@defthing[#:kind "attribute" font-italic symbol?]{
Whether the quad has italic styling applied. Activated only when value is @racket["true"]. 
}

@defthing[#:kind "attribute" line-height symbol?]{
Distance between baselines. Value is a @tech{dimension string}.
}

TK: OT feature attributes, bullet attributes


@subsection{Rendering}

@defproc[
(render-pdf
[qx qexpr?]
[pdf-path (or/c path? path-string? #false)]
[#:replace replace? any/c #true])
(or/c void? bytes?)]{
Compute the layout for @racket[qx] and render it as a PDF to @racket[pdf-path]. If @racket[pdf-path] is @racket[#false], then the rendered PDF is returned as a @tech{byte string}. Otherwise it is written to @racket[pdf-path]. 

The optional @racket[replace?] argument controls whether an existing file is automatically overwritten. The default is @racket[#true].
}


@subsection{Fonts}

A design goal of Quadwriter is to treat document layout as the result of a program. Along those lines, fonts are handled differently than usual. When you use a word processor, you choose from whatever fonts might be installed on your system. 

Quadwriter, by contrast, relies only on fonts that are @emph{in the same directory} as your other project source files. This is a feature: it means that everything  necessary to render the document travels together in the same directory. You can re-render it anywhere with identical results. You never have the problem — still with us after 35 years of desktop word processing — that ``oh, you need to install such-and-such font in your system before it will work.'' Bah!

Quadwriter supports the usual TrueType (.ttf) and OpenType (.otf) font files. To add fonts to your Quadwriter experience:

@itemlist[#:style 'ordered

@item{Within your project directory, create a subdirectory called @racket["fonts"].}

@item{Within @racket["fonts"], create a subdirectory for each font family you want to use in your Quadwriter document. The names of these subdirectories will become the acceptable values for the @racket[font-family] attribute in your documents.}

@item{If there is only one font file in the family subdirectory, then it is used every time the font family is requested.}

@item{Alternatively, you can specify styled variants by creating within the family directory style subdirectories called @racket["regular"], @racket["bold"], @racket["italic"], and @racket["bold-italic"].}
]

Though this system may seem like a lot of housekeeping, it's nice for two reasons. First, we use the filesystem to map font names to font files, and avoid having another configuration file floating around our project. Second, we create a layer of abstraction between font names and files. This makes it easy to change the fonts in the document: you just put new fonts in the appropriate font-family directory, and you don't need to faff about with the source file itself.

TK: example of font setup

@subsection{Utility}

@defproc[
(view-result)
void?]{
On the REPL, after running a @racketmodname[quadwriter] dialect and generating a PDF, this function will open the PDF.
}



@section{Quad: the details}

As mentioned above, The @racket[quad] library itself knows as little as it can about typography and fonts and pictures. Nor does it even assert a document model like Scribble. Rather, it offers a generic geometric represntation of layout elements. In turn, these elements can be combined into more useful pieces (e.g., @racket[quadwriter]).

@subsection{Data model: the quad}

The eponymous @racket[quad] is a structure type that represents a rectangular layout area. This rectangle is used for layout purposes only. It is not enforced during the rendering phase. Meaning, once positioned, a quad's drawing function can access this rectangle, but does not need to stay within it.

Each quad has nested @deftech{elements}, which is a (possibly empty) list of subquads. Given a certain element, the quad containing it is called its @deftech{parent} quad.

Quads can be freely nested. There are no rules about what kind of quad can be nested in another.


@subsection{Wrapping}

Wrapping is a optional phase where lists of quads are broken into sublists of a certain size. In @racket[quadwriter], the list of words is wrapped to produce a list of lines of a certain horizontal width. In turn, the list of lines is wrapped to produce a list of pages of a certain vertical height.

@subsection{Layout}

The heart of Quad's layout logic is its system of @deftech{anchor points}. A quad is positioned in a layout by aligning its anchor point to an anchor point on the previous quad.

Each quad has a set of 11 anchor points on its perimeter. 

Eight points are named for the compass directions: @racket['n] (= top center) @racket['e] (= right center) @racket['s] (= bottom center) @racket['w] (= left ceter) @racket['ne] (= upper right) @racket['se] (= lower right) @racket['sw] (= lower left) @racket['nw] (= upper left). 

The center of the quad is @racket['c]. 

The other two anchor points are @racket['baseline-in] and @racket['baseline-out] or just @racket['bi] and @racket['bo]. These points are also on the quad perimieter. They allow quads containing type to be aligned according to adjacent baselines. The exact location of these points depends on the direction of the script. For instance, in left-to-right languages, @racket['baseline-in] is on the left edge, and @racket['baseline-out] is on the right. The vertical position of these points depends on the font associated with the quad. If no font is specified, the @racket['bi] and @racket['bo] points are vertically positioned at the southern edge.

By default, each subquad will ultimately be positioned relative to the immediately preceding subquad (or, if it's the first subquad, the parent). Optionally, a subquad can attach to the parent. 

How does a quad know which anchor points to use? Each quad specifies a @deftech{to anchor} on its own perimeter, and a @deftech{from anchor} on the previous quad's perimeter. The quad is positioned by moving it until its @deftech{to anchor} matches the position of the (already positioned) @deftech{from anchor}. Think of it like two tiny magnets clicking together.

A key benefit of the anchor-point system is that it gets rid of notions of ``horizontal'', ``vertical'', ``up'', ``down'', etc. Quads flow in whatever direction is implied by their anchor points.

@examples[#:label #f #:eval my-eval
(define q1 (make-quad #:size '(25 25)))
(define q2 (make-quad #:size '(15 15)))

(quad->pict (position (attach-to q1 'e q2 'w)))
(quad->pict (position (attach-to q1 'nw q2 'se)))
(quad->pict (position (attach-to q1 'w q2 'e)))
(quad->pict (position (attach-to q1 's q2 'n)))
(quad->pict (position (attach-to q1 'e q2 'n)))
]


``Wait a minute — why is the new quad specifying @emph{both} anchor points? Shouldn't the from anchor be specified by the previous quad?'' It could, but it would make the layout system less flexible, because all the subquads hanging onto a certain quad would have to emanate from a single point. This way, every subquad can attach to its neighbor (or the parent) in whatever way it prefers.


@subsection{Rendering}

Once the quads have been positioned, they are passed to the renderer, which recursively visits each quad and calls its drawing function.

Though every quad has a size field, this is just the size used during layout and positioning. Quad doesn't know (or care) about whether the drawing stays within those bounds.


@section{What are your plans for Quad?}

Some things I personally plan to use Quad for:

@itemlist[#:style 'ordered

@item{@bold{A simple word processor}. Quadwriter is the demo of this.}

@item{@bold{Font sample documents}. In my work as a @link["https://mbtype.com"]{type designer}, I have to put together PDFs of fonts. To date, I have done them by hand, but I would like to just write programs to generate them.}

@item{@bold{Racket documentation}. The PDFs of Racket documentation are currently generated by LaTeX. I would like to make Quad good enough to handle them.}

@item{@bold{Book publishing}. My wife is a lawyer and wants to publish a book about a certain area of the law that involves a zillion fiddly charts. If I had to do it by hand, it would take months. But with a Quad program, it could be easy.}

]


@subsection{Why is it called Quad?}

In letterpress printing, a @italic{quad} was a piece of metal used as spacing material within a line.



@(linebreak)
@(linebreak)
@(linebreak)


@italic{``A way of doing something original is by trying something
so painstaking that nobody else has ever bothered with it.'' — Brian Eno}