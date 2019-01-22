# Graphics and GUIs

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
