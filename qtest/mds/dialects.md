# Dialects of Racket and Scheme

We use “Racket” to refer to a specific dialect of the Lisp language, and
one that is based on the Scheme branch of the Lisp family. Despite
Racket’s similarity to Scheme, the `#lang` prefix on modules is a
particular feature of Racket, and programs that start with `#lang` are
unlikely to run in other implementations of Scheme. At the same time,
programs that do not start with `#lang` do not work with the default
mode of most Racket tools.

“Racket” is not, however, the only dialect of Lisp that is supported by
Racket tools. On the contrary, Racket tools are designed to support
multiple dialects of Lisp and even multiple languages, which allows the
Racket tool suite to serve multiple communities. Racket also gives
programmers and researchers the tools they need to explore and create
new languages.

    1 More Rackets
                  
    2 Standards   
      2.1 R5RS    
      2.2 R6RS    
                  
    3 Teaching    

## 1. More Rackets

“Racket” is more of an idea about programming languages than a language
in the usual sense. Macros can extend a base language \(as described in
\[missing\]\), and alternate parsers can construct an entirely new
language from the ground up \(as described in \[missing\]\).

The `#lang` line that starts a Racket module declares the base language
of the module. By “Racket,” we usually mean `#lang` followed by the base
language `racket` or `racket/base` \(of which `racket` is an
extension\). The Racket distribution provides additional languages,
including the following:

* `typed/racket` — like `racket`, but statically typed; see \[missing\]

* `lazy` — like `racket/base`, but avoids evaluating an expression until
  its value is needed; see the Lazy Racket documentation.

* `frtime` — changes evaluation in an even more radical way to support
  reactive programming; see the FrTime documentation.

* `scribble/base` — a language, which looks more like Latex than Racket,
  for writing documentation; see \[missing\]

Each of these languages is used by starting module with the language
name after `#lang`. For example, this source of this document starts
with `#lang scribble/base`.

Furthermore, Racket users can define their own languages, as discussed
in \[missing\]. Typically, a language name maps to its implementation
through a module path by adding `/lang/reader`; for example, the
language name `scribble/base` is expanded to
`scribble/base/lang/reader`, which is the module that implements the
surface-syntax parser. Some language names act as language loaders; for
example, `#lang planet planet-path` downloads, installs, and uses a
language via PLaneT.

## 2. Standards

Standard dialects of Scheme include the ones defined by R5RS and R6RS.

### 2.1. R5RS

“R5RS” stands for [The Revised5 Report on the Algorithmic Language
Scheme](../r5rs/r5rs-std/index.html), and it is currently the most
widely implemented Scheme standard.

Racket tools in their default modes do not conform to R5RS, mainly
because Racket tools generally expect modules, and R5RS does not define
a module system. Typical single-file R5RS programs can be converted to
Racket programs by prefixing them with `#lang r5rs`, but other Scheme
systems do not recognize `#lang r5rs`. The `plt-r5rs` executable \(see
\[missing\]\) more directly conforms to the R5RS standard.

Aside from the module system, the syntactic forms and functions of R5RS
and Racket differ. Only simple R5RS become Racket programs when prefixed
with `#lang racket`, and relatively few Racket programs become R5RS
programs when a `#lang` line is removed. Also, when mixing “R5RS
modules” with Racket modules, beware that R5RS pairs correspond to
Racket mutable pairs \(as constructed with `mcons`\).

See \[missing\] for more information about running R5RS programs with
Racket.

### 2.2. R6RS

“R6RS” stands for [The Revised6 Report on the Algorithmic Language
Scheme](../r6rs/r6rs-std/index.html), which extends R5RS with a module
system that is similar to the Racket module system.

When an R6RS library or top-level program is prefixed with `#!r6rs`
\(which is valid R6RS syntax\), then it can also be used as a Racket
program. This works because `#!` in Racket is treated as a shorthand for
`#lang` followed by a space, so `#!r6rs` selects the `r6rs` module
language. As with R5RS, however, beware that the syntactic forms and
functions of R6RS differ from Racket, and R6RS pairs are mutable pairs.

See \[missing\] for more information about running R6RS programs with
Racket.

## 3. Teaching

The _[How to Design Programs](http://www.htdp.org)_ textbook relies on
pedagogic variants of Racket that smooth the introduction of programming
concepts for new programmers. See the _[How to Design
Programs](http://www.htdp.org)_ language documentation.

The _[How to Design Programs](http://www.htdp.org)_ languages are
typically not used with `#lang` prefixes, but are instead used within
DrRacket by selecting the language from the Choose Language... dialog.
