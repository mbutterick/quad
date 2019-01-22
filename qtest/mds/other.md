# More Libraries

This guide covers only the Racket language and libraries that are
documented in \[missing\]. The Racket distribution includes many
additional libraries.

## 1. Graphics and GUIs

Racket provides many libraries for graphics and graphical user
interfaces \(GUIs\):

* The `racket/draw` library provides basic drawing tools, including
  drawing contexts such as bitmaps and PostScript files.

  See \[missing\] for more information.

* The `racket/gui` library provides GUI widgets such as windows,
  buttons, checkboxes, and text fields. The library also includes a
  sophisticated and extensible text editor.

  See \[missing\] for more information.

* The `pict` library provides a more functional abstraction layer over
  `racket/draw`. This layer is especially useful for creating slide
  presentations with Slideshow, but it is also useful for creating
  images for Scribble documents or other drawing tasks. Pictures created
  with the `pict` library can be rendered to any drawing context.

  See \[missing\] for more information.

* The `2htdp/image` library is similar to `pict`. It is more streamlined
  for pedagogical use, but also slightly more specific to screen and
  bitmap drawing.

  See `2htdp/image` for more information.

* The `sgl` library provides OpenGL for 3-D graphics. The context for
  rendering OpenGL can be a window or bitmap created with `racket/gui`.

  See the SGL documentation for more information.

## 2. The Web Server

\[missing\] describes the Racket web server, which supports servlets
implemented in Racket.

## 3. Using Foreign Libraries

\[missing\] describes tools for using Racket to access libraries that
are normally used by C programs.

## 4. And More

[Racket Documentation](../index.html) lists documentation for many other
installed libraries. Run `raco docs` to find documentation for libraries
that are installed on your system and specific to your user account.

[The Racket package repository](https://pkgs.racket-lang.org/) offer
even more downloadable packages that are contributed by Racketeers.

The legacy [PLaneT](http://planet.racket-lang.org/) site offers
additional packages, although maintained packages have generally
migrated to the newer package repository.
