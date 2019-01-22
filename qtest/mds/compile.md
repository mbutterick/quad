# Compilation and Configuration: `raco`

The `raco` \(short for “**Ra**cket **co**mmand”\) program provides a
command-line interface to many additional tools for compiling Racket
programs and maintaining a Racket installation.

* `raco make` compiles Racket source to bytecode.

  For example, if you have a program `"take-over-world.rkt"` and you’d
  like to compile it to bytecode, along with all of its dependencies, so
  that it loads more quickly, then run

    `raco make take-over-the-world.rkt`

  The bytecode file is written as `"take-over-the-world_rkt.zo"` in a
  `"compiled"` subdirectory; `".zo"` is the file suffix for a bytecode
  file.

* `raco setup` manages a Racket installation, including manually
  installed packages.

  For example, if you create your own library collection called
  `"take-over"`, and you’d like to build all bytecode and documentation
  for the collection, then run

    `raco setup take-over`

* `raco pkg` manages packages that can be installed through the Racket
  package manager.

  For example, to see the list of installed packages run:

    `raco pkg show`

  To install a new package named `<package-name>` run:

    `raco pkg install <package-name>`

  See \[missing\] for more details about package management.

For more information on `raco`, see \[missing\].
