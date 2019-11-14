Quad [![Build Status](https://travis-ci.org/mbutterick/quad.svg?branch=master)](https://travis-ci.org/mbutterick/quad)
----

A work-in-progress document processor written in [Racket](http://racket-lang.org). 

Create PDFs from a simple text-based source format. Use any TTF or OTF or WOFF fonts you like.

Quad lets you avoid:

* word processors
* page-layout programs
* web browsers
* LaTeX and derivatives
* subscriptions, surveillance, and “creative clouds”

Quad, by contrast —

* is downloadable & self-contained
* is open source
* makes document layout fast & easy
* uses source files that are portable between Mac OS, Windows, and Linux
* works with [Pollen](//pollenpub.com)

Install
---

Using Racket 6.10+, install from the command line:

    raco pkg install quad
    
And update like so:

    raco pkg update --update-deps quad


Docs
---

http://docs.racket-lang.org/quad


License
---

Code: MIT

Fonts: mostly OFL, noted in each directory that contains fonts

Unicode Consortium documents: licensed under Unicode terms of use


![Quad](https://docs.racket-lang.org/quad/quads.png)

