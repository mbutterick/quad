# Modules

Modules let you organize Racket code into multiple files and reusable
libraries.

    1 Module Basics               
      1.1 Organizing Modules      
      1.2 Library Collections     
      1.3 Packages and Collections
      1.4 Adding Collections      
                                  
    2 Module Syntax               
      2.1 The `module` Form       
      2.2 The `#lang` Shorthand   
      2.3 Submodules              
      2.4 Main and Test Submodules
                                  
    3 Module Paths                
                                  
    4 Imports: `require`          
                                  
    5 Exports: `provide`          
                                  
    6 Assignment and Redefinition 
                                  
    7 Modules and Macros          

## 1. Module Basics

Each Racket module typically resides in its own file. For example,
suppose the file `"cake.rkt"` contains the following module:

`"cake.rkt"`
```racket
#lang racket                     
                                 
(provide print-cake)             
                                 
; draws a cake with n candles    
(define (print-cake n)           
  (show "   ~a   " n #\.)        
  (show " .-~a-. " n #\|)        
  (show " | ~a | " n #\space)    
  (show "---~a---" n #\-))       
                                 
(define (show fmt n ch)          
  (printf fmt (make-string n ch))
  (newline))                     
```

Then, other modules can import `"cake.rkt"` to use the `print-cake`
function, since the `provide` line in `"cake.rkt"` explicitly exports
the definition `print-cake`. The `show` function is private to
`"cake.rkt"` \(i.e., it cannot be used from other modules\), since
`show` is not exported.

The following `"random-cake.rkt"` module imports `"cake.rkt"`:

`"random-cake.rkt"`
```racket
#lang racket            
                        
(require "cake.rkt")    
                        
(print-cake (random 30))
```

The relative reference `"cake.rkt"` in the import `(require "cake.rkt")`
works if the `"cake.rkt"` and `"random-cake.rkt"` modules are in the
same directory. Unix-style relative paths are used for relative module
references on all platforms, much like relative URLs in HTML pages.

### 1.1. Organizing Modules

The `"cake.rkt"` and `"random-cake.rkt"` example demonstrates the most
common way to organize a program into modules: put all module files in a
single directory \(perhaps with subdirectories\), and then have the
modules reference each other through relative paths. A directory of
modules can act as a project, since it can be moved around on the
filesystem or copied to other machines, and relative paths preserve the
connections among modules.

As another example, if you are building a candy-sorting program, you
might have a main `"sort.rkt"` module that uses other modules to access
a candy database and a control sorting machine. If the candy-database
module itself is organized into sub-modules that handle barcode and
manufacturer information, then the database module could be
`"db/lookup.rkt"` that uses helper modules `"db/barcodes.rkt"` and
`"db/makers.rkt"`.  Similarly, the sorting-machine driver
`"machine/control.rkt"` might use helper modules `"machine/sensors.rkt"`
and `"machine/actuators.rkt"`.

\#<pict>

The `"sort.rkt"` module uses the relative paths `"db/lookup.rkt"` and
`"machine/control.rkt"` to import from the database and machine-control
libraries:

`"sort.rkt"`
```racket
#lang racket                                   
(require "db/lookup.rkt" "machine/control.rkt")
....                                           
```

The `"db/lookup.rkt"` module similarly uses paths relative to its own
source to access the `"db/barcodes.rkt"` and `"db/makers.rkt"` modules:

`"db/lookup.rkt"`
```racket
#lang racket                        
(require "barcode.rkt" "makers.rkt")
....                                
```

Ditto for `"machine/control.rkt"`:

`"machine/control.rkt"`
```racket
#lang racket                           
(require "sensors.rkt" "actuators.rkt")
....                                   
```

Racket tools all work automatically with relative paths. For example,

  `racket sort.rkt`

on the command line runs the `"sort.rkt"` program and automatically
loads and compiles required modules. With a large enough program,
compilation from source can take too long, so use

  `raco make sort.rkt`

> See \[missing\] for more information on `raco make`.

to compile `"sort.rkt"` and all its dependencies to bytecode files.
Running `racket sort.rkt` will automatically use bytecode files when
they are present.

### 1.2. Library Collections

A _collection_ is a hierarchical grouping of installed library modules.
A module in a collection is referenced through an unquoted, suffixless
path. For example, the following module refers to the `"date.rkt"`
library that is part of the `"racket"` collection:

```racket
#lang racket                                             
                                                         
(require racket/date)                                    
                                                         
(printf "Today is ~s\n"                                  
        (date->string (seconds->date (current-seconds))))
```

When you search the online Racket documentation, the search results
indicate the module that provides each binding. Alternatively, if you
reach a binding’s documentation by clicking on hyperlinks, you can hover
over the binding name to find out which modules provide it.

A module reference like `racket/date` looks like an identifier, but it
is not treated in the same way as `printf` or `date->string`. Instead,
when `require` sees a module reference that is unquoted, it converts the
reference to a collection-based module path:

* First, if the unquoted path contains no `/`, then `require`
  automatically adds a `"/main"` to the reference. For example,
  `(require slideshow)` is equivalent to `(require slideshow/main)`.

* Second, `require` implicitly adds a `".rkt"` suffix to the path.

* Finally, `require` resolves the path by searching among installed
  collections, instead of treating the path as relative to the enclosing
  module’s path.

To a first approximation, a collection is implemented as a filesystem
directory. For example, the `"racket"` collection is mostly located in a
`"racket"` directory within the Racket installation’s `"collects"`
directory, as reported by

```racket
#lang racket                                               
                                                           
(require setup/dirs)                                       
                                                           
(build-path (find-collects-dir) ; main collection directory
            "racket")                                      
```

The Racket installation’s `"collects"` directory, however, is only one
place that `require` looks for collection directories. Other places
include the user-specific directory reported by
`(find-user-collects-dir)` and directories configured through the
`PLTCOLLECTS` search path. Finally, and most typically, collections are
found through installed packages.

### 1.3. Packages and Collections

A _package_ is a set of libraries that are installed through the Racket
package manager \(or included as pre-installed in a Racket
distribution\). For example, the `racket/gui` library is provided by the
`"gui"` package, while `parser-tools/lex` is provided by the
`"parser-tools"` library.

> More precisely, `racket/gui` is provided by `"gui-lib"`,
> `parser-tools/lex` is provided by `"parser-tools-lib"`, and the `"gui"`
> and `"parser-tools"` packages extend `"gui-lib"` and
> `"parser-tools-lib"` with documentation.

Racket programs do not refer to packages directly. Instead, programs
refer to libraries via collections, and adding or removing a package
changes the set of collection-based libraries that are available. A
single package can supply libraries in multiple collections, and two
different packages can supply libraries in the same collection \(but not
the same libraries, and the package manager ensures that installed
packages do not conflict at that level\).

For more information about packages, see \[missing\].

### 1.4. Adding Collections

Looking back at the candy-sorting example of Organizing Modules, suppose
that modules in `"db/"` and `"machine/"` need a common set of helper
functions. Helper functions could be put in a `"utils/"` directory, and
modules in `"db/"` or `"machine/"` could access utility modules with
relative paths that start `"../utils/"`. As long as a set of modules
work together in a single project, it’s best to stick with relative
paths. A programmer can follow relative-path references without knowing
about your Racket configuration.

Some libraries are meant to be used across multiple projects, so that
keeping the library source in a directory with its uses does not make
sense. In that case, the best option is add a new collection. After the
library is in a collection, it can be referenced with an unquoted path,
just like libraries that are included with the Racket distribution.

You could add a new collection by placing files in the Racket
installation or one of the directories reported by
`(get-collects-search-dirs)`. Alternatively, you could add to the list
of searched directories by setting the `PLTCOLLECTS` environment
variable.If you set `PLTCOLLECTS`, include an empty path in by starting
the value with a colon \(Unix and Mac OS\) or semicolon \(Windows\) so
that the original search paths are preserved. The best option, however,
is to add a package.

Creating a package _does not_ mean that you have to register with a
package server or perform a bundling step that copies your source code
into an archive format. Creating a package can simply mean using the
package manager to make your libraries locally accessible as a
collection from their current source locations.

For example, suppose you have a directory `"/usr/molly/bakery"` that
contains the `"cake.rkt"` module \(from the beginning of this section\)
and other related modules. To make the modules available as a `"bakery"`
collection, either

* Use the `raco pkg` command-line tool:

    `raco pkg install --link /usr/molly/bakery`

  where the `--link` flag is not actually needed when the provided path
  includes a directory separator.

* Use DrRacket’s Package Manager item from the File menu. In the Do What
  I Mean panel, click Browse..., choose the `"/usr/molly/bakery"`
  directory, and click Install.

Afterward, `(require bakery/cake)` from any module will import the
`print-cake` function from `"/usr/molly/bakery/cake.rkt"`.

By default, the name of the directory that you install is used both as
the package name and as the collection that is provided by the package.
Also, the package manager normally defaults to installation only for the
current user, as opposed to all users of a Racket installation. See
\[missing\] for more information.

If you intend to distribute your libraries to others, choose collection
and package names carefully. The collection namespace is hierarchical,
but top-level collection names are global, and the package namespace is
flat. Consider putting one-off libraries under some top-level name like
`"molly"` that identifies the producer.  Use a collection name like
`"bakery"` when producing the definitive collection of baked-goods
libraries.

After your libraries are put in a collection you can still use `raco
make` to compile the library sources, but it’s better and more
convenient to use `raco setup`. The `raco setup` command takes a
collection name \(as opposed to a file name\) and compiles all libraries
within the collection. In addition, `raco setup` can build documentation
for the collection and add it to the documentation index, as specified
by a `"info.rkt"` module in the collection. See \[missing\] for more
information on `raco setup`.

## 2. Module Syntax

The `#lang` at the start of a module file begins a shorthand for a
`module` form, much like `'` is a shorthand for a `quote` form. Unlike
`'`, the `#lang` shorthand does not work well in a REPL, in part because
it must be terminated by an end-of-file, but also because the longhand
expansion of `#lang` depends on the name of the enclosing file.

### 2.1. The `module` Form

The longhand form of a module declaration, which works in a REPL as well
as a file, is

```racket
(module name-id initial-module-path
  decl ...)                        
```

where the `name-id` is a name for the module, `initial-module-path` is
an initial import, and each `decl` is an import, export, definition, or
expression.  In the case of a file, `name-id` normally matches the name
of the containing file, minus its directory path or file extension, but
`name-id` is ignored when the module is `require`d through its file’s
path.

The `initial-module-path` is needed because even the `require` form must
be imported for further use in the module body. In other words, the
`initial-module-path` import bootstraps the syntax that is available in
the body. The most commonly used `initial-module-path` is `racket`,
which supplies most of the bindings described in this guide, including
`require`, `define`, and `provide`. Another commonly used
`initial-module-path` is `racket/base`, which provides less
functionality, but still much of the most commonly needed functions and
syntax.

For example, the `"cake.rkt"` example of the previous section could be
written as

```racket
(module cake racket                
  (provide print-cake)             
                                   
  (define (print-cake n)           
    (show "   ~a   " n #\.)        
    (show " .-~a-. " n #\|)        
    (show " | ~a | " n #\space)    
    (show "---~a---" n #\-))       
                                   
  (define (show fmt n ch)          
    (printf fmt (make-string n ch))
    (newline)))                    
```

Furthermore, this `module` form can be evaluated in a REPL to declare a
`cake` module that is not associated with any file. To refer to such an
unassociated module, quote the module name:

Examples:

```racket
> (require 'cake)
> (print-cake 3) 
   ...           
 .-|||-.         
 |     |         
---------        
```

Declaring a module does not immediately evaluate the body definitions
and expressions of the module. The module must be explicitly `require`d
at the top level to trigger evaluation. After evaluation is triggered
once, later `require`s do not re-evaluate the module body.

Examples:

```racket
> (module hi racket    
    (printf "Hello\n"))
> (require 'hi)        
Hello                  
> (require 'hi)        
```

### 2.2. The `#lang` Shorthand

The body of a `#lang` shorthand has no specific syntax, because the
syntax is determined by the language name that follows `#lang`.

In the case of `#lang` `racket`, the syntax is

```racket
#lang racket
decl ...    
```

which reads the same as

```racket
(module name racket
  decl ...)        
```

where `name` is derived from the name of the file that contains the
`#lang` form.

The `#lang` `racket/base` form has the same syntax as `#lang` `racket`,
except that the longhand expansion uses `racket/base` instead of
`racket`. The `#lang` `scribble/manual` form, in contrast, has a
completely different syntax that doesn’t even look like Racket, and
which we do not attempt to describe in this guide.

Unless otherwise specified, a module that is documented as a “language”
using the `#lang` notation will expand to `module` in the same way as
`#lang` `racket`. The documented language name can be used directly with
`module` or `require`, too.

### 2.3. Submodules

A `module` form can be nested within a module, in which case the nested
`module` form declares a _submodule_. Submodules can be referenced
directly by the enclosing module using a quoted name. The following
example prints `"Tony"` by importing `tiger` from the `zoo` submodule:

`"park.rkt"`
```racket
#lang racket            
                        
(module zoo racket      
  (provide tiger)       
  (define tiger "Tony"))
                        
(require 'zoo)          
                        
tiger                   
```

Running a module does not necessarily run its submodules. In the above
example, running `"park.rkt"` runs its submodule `zoo` only because the
`"park.rkt"` module `require`s the `zoo` submodule. Otherwise, a module
and each of its submodules can be run independently. Furthermore, if
`"park.rkt"` is compiled to a bytecode file \(via `raco make`\), then
the code for `"park.rkt"` or the code for `zoo` can be loaded
independently.

Submodules can be nested within submodules, and a submodule can be
referenced directly by a module other than its enclosing module by using
a submodule path.

A `module*` form is similar to a nested `module` form:

```racket
(module* name-id initial-module-path-or-#f
  decl ...)                               
```

The `module*` form differs from `module` in that it inverts the
possibilities for reference between the submodule and enclosing module:

* A submodule declared with `module` can be `require`d by its enclosing
  module, but the submodule cannot `require` the enclosing module or
  lexically reference the enclosing module’s bindings.

* A submodule declared with `module*` can `require` its enclosing
  module, but the enclosing module cannot `require` the submodule.

In addition, a `module*` form can specify `#f` in place of an
`initial-module-path`, in which case the submodule sees all of the
enclosing module’s bindings—including bindings that are not exported via
`provide`.

One use of submodules declared with `module*` and `#f` is to export
additional bindings through a submodule that are not normally exported
from the module:

`"cake.rkt"`
```racket
#lang racket                     
                                 
(provide print-cake)             
                                 
(define (print-cake n)           
  (show "   ~a   " n #\.)        
  (show " .-~a-. " n #\|)        
  (show " | ~a | " n #\space)    
  (show "---~a---" n #\-))       
                                 
(define (show fmt n ch)          
  (printf fmt (make-string n ch))
  (newline))                     
                                 
(module* extras #f               
  (provide show))                
```

In this revised `"cake.rkt"` module, `show` is not imported by a module
that uses `(require "cake.rkt")`, since most clients of `"cake.rkt"`
will not want the extra function.  A module can require the `extra`
submodule using `(require (submod "cake.rkt" extras))` to access the
otherwise hidden `show` function.See submodule paths for more
information on `submod`.

### 2.4. Main and Test Submodules

The following variant of `"cake.rkt"` includes a `main` submodule that
calls `print-cake`:

`"cake.rkt"`
```racket
#lang racket                     
                                 
(define (print-cake n)           
  (show "   ~a   " n #\.)        
  (show " .-~a-. " n #\|)        
  (show " | ~a | " n #\space)    
  (show "---~a---" n #\-))       
                                 
(define (show fmt n ch)          
  (printf fmt (make-string n ch))
  (newline))                     
                                 
(module* main #f                 
  (print-cake 10))               
```

Running a module does not run its `module*`-defined submodules.
Nevertheless, running the above module via `racket` or DrRacket prints a
cake with 10 candles, because the `main` submodule is a special case.

When a module is provided as a program name to the `racket` executable
or run directly within DrRacket, if the module has a `main` submodule,
the `main` submodule is run after its enclosing module. Declaring a
`main` submodule thus specifies extra actions to be performed when a
module is run directly, instead of `require`d as a library within a
larger program.

A `main` submodule does not have to be declared with `module*`. If the
`main` module does not need to use bindings from its enclosing module,
it can be declared with `module`. More commonly, `main` is declared
using `module+`:

```racket
(module+ name-id
  decl ...)     
```

A submodule declared with `module+` is like one declared with `module*`
using `#f` as its `initial-module-path`.  In addition, multiple
`module+` forms can specify the same submodule name, in which case the
bodies of the `module+` forms are combined to create a single submodule.

The combining behavior of `module+` is particularly useful for defining
a `test` submodule, which can be conveniently run using `raco test` in
much the same way that `main` is conveniently run with `racket`. For
example, the following `"physics.rkt"` module exports `drop` and
`to-energy` functions, and it defines a `test` module to hold unit
tests:

`"physics.rkt"`
```racket
#lang racket                          
(module+ test                         
  (require rackunit)                  
  (define ε 1e-10))                   
                                      
(provide drop                         
         to-energy)                   
                                      
(define (drop t)                      
  (* 1/2 9.8 t t))                    
                                      
(module+ test                         
  (check-= (drop 0) 0 ε)              
  (check-= (drop 10) 490 ε))          
                                      
(define (to-energy m)                 
  (* m (expt 299792458.0 2)))         
                                      
(module+ test                         
  (check-= (to-energy 0) 0 ε)         
  (check-= (to-energy 1) 9e+16 1e+15))
```

Importing `"physics.rkt"` into a larger program does not run the `drop`
and `to-energy` tests—or even trigger the loading of the test code, if
the module is compiled—but running `raco test physics.rkt` at a command
line runs the tests.

The above `"physics.rkt"` module is equivalent to using `module*`:

`"physics.rkt"`
```racket
#lang racket                          
                                      
(provide drop                         
         to-energy)                   
                                      
(define (drop t)                      
  (* 1/2 49/5 t t))                   
                                      
(define (to-energy m)                 
  (* m (expt 299792458 2)))           
                                      
(module* test #f                      
  (require rackunit)                  
  (define ε 1e-10)                    
  (check-= (drop 0) 0 ε)              
  (check-= (drop 10) 490 ε)           
  (check-= (to-energy 0) 0 ε)         
  (check-= (to-energy 1) 9e+16 1e+15))
```

Using `module+` instead of `module*` allows tests to be interleaved with
function definitions.

The combining behavior of `module+` is also sometimes helpful for a
`main` module. Even when combining is not needed, `(module+ main ....)`
is preferred as it is more readable than `(module* main #f ....)`.

## 3. Module Paths

A _module path_ is a reference to a module, as used with `require` or as
the `initial-module-path` in a `module` form. It can be any of several
forms:

```racket
(quote id)
```
A module path that is a quoted identifier refers to a non-file `module`
declaration using the identifier. This form of module reference makes
the most sense in a REPL.
Examples:

```racket
> (module m racket                             
    (provide color)                            
    (define color "blue"))                     
> (module n racket                             
    (require 'm)                               
    (printf "my favorite color is ~a\n" color))
> (require 'n)                                 
my favorite color is blue                      
```

```racket
rel-string
```
A string module path is a relative path using Unix-style conventions:
`/` is the path separator, `..` refers to the parent directory, and `.`
refers to the same directory. The `rel-string` must not start or end
with a path separator. If the path has no suffix, `".rkt"` is added
automatically.
The path is relative to the enclosing file, if any, or it is relative to
the current directory. \(More precisely, the path is relative to the
value of `(current-load-relative-directory)`, which is set while loading
a file.\)
Module Basics shows examples using relative paths.
If a relative path ends with a `".ss"` suffix, it is converted to
`".rkt"`. If the file that implements the referenced module actually
ends in `".ss"`, the suffix will be changed back when attempting to load
the file \(but a `".rkt"` suffix takes precedence\). This two-way
conversion provides compatibility with older versions of Racket.

```racket
id
```
A module path that is an unquoted identifier refers to an installed
library. The `id` is constrained to contain only ASCII letters, ASCII
numbers, `+`, `-`, `_`, and `/`, where `/` separates path elements
within the identifier. The elements refer to collections and
sub-collections, instead of directories and sub-directories.
An example of this form is `racket/date`. It refers to the module whose
source is the `"date.rkt"` file in the `"racket"` collection, which is
installed as part of Racket. The `".rkt"` suffix is added automatically.
Another example of this form is `racket`, which is commonly used at the
initial import. The path `racket` is shorthand for `racket/main`; when
an `id` has no `/`, then `/main` is automatically added to the end.
Thus, `racket` or `racket/main` refers to the module whose source is the
`"main.rkt"` file in the `"racket"` collection.
Examples:

```racket
> (module m racket                                            
    (require racket/date)                                     
                                                              
    (printf "Today is ~s\n"                                   
            (date->string (seconds->date (current-seconds)))))
> (require 'm)                                                
Today is "Monday, January 21st, 2019"                         
```
When the full path of a module ends with `".rkt"`, if no such file
exists but one does exist with the `".ss"` suffix, then the `".ss"`
suffix is substituted automatically. This transformation provides
compatibility with older versions of Racket.

```racket
(lib rel-string)
```
Like an unquoted-identifier path, but expressed as a string instead of
an identifier. Also, the `rel-string` can end with a file suffix, in
which case `".rkt"` is not automatically added.
Example of this form include `(lib "racket/date.rkt")` and `(lib
"racket/date")`, which are equivalent to `racket/date`. Other examples
include `(lib "racket")`, `(lib "racket/main")`, and `(lib
"racket/main.rkt")`, which are all equivalent to `racket`.
Examples:

```racket
> (module m (lib "racket")                                    
    (require (lib "racket/date.rkt"))                         
                                                              
    (printf "Today is ~s\n"                                   
            (date->string (seconds->date (current-seconds)))))
> (require 'm)                                                
Today is "Monday, January 21st, 2019"                         
```

```racket
(planet id)
```
Accesses a third-party library that is distributed through the PLaneT
server. The library is downloaded the first time that it is needed, and
then the local copy is used afterward.
The `id` encodes several pieces of information separated by a `/`: the
package owner, then package name with optional version information, and
an optional path to a specific library with the package. Like `id` as
shorthand for a `lib` path, a `".rkt"` suffix is added automatically,
and `/main` is used as the path if no sub-path element is supplied.
Examples:

```racket
> (module m (lib "racket")                                   
    ; Use "schematics"'s "random.plt" 1.0, file "random.rkt":
    (require (planet schematics/random:1/random))            
    (display (random-gaussian)))                             
> (require 'm)                                               
0.9050686838895684                                           
```
As with other forms, an implementation file ending with `".ss"` can be
substituted automatically if no implementation file ending with `".rkt"`
exists.

```racket
(planet package-string)
```
Like the symbol form of a `planet`, but using a string instead of an
identifier. Also, the `package-string` can end with a file suffix, in
which case `".rkt"` is not added.
As with other forms, an `".ss"` extension is converted to `".rkt"`,
while an implementation file ending with `".ss"` can be substituted
automatically if no implementation file ending with `".rkt"` exists.

```racket
(planet rel-string (user-string pkg-string vers ...))
                                                     
vers = nat                                           
     | (nat nat)                                     
     | (= nat)                                       
     | (+ nat)                                       
     | (- nat)                                       
```
A more general form to access a library from the PLaneT server. In this
general form, a PLaneT reference starts like a `lib` reference with a
relative path, but the path is followed by information about the
producer, package, and version of the library. The specified package is
downloaded and installed on demand.
The `vers`es specify a constraint on the acceptable version of the
package, where a version number is a sequence of non-negative integers,
and the constraints determine the allowable values for each element in
the sequence. If no constraint is provided for a particular element,
then any version is allowed; in particular, omitting all `vers`es means
that any version is acceptable. Specifying at least one `vers` is
strongly recommended.
For a version constraint, a plain `nat` is the same as `(+ nat)`, which
matches `nat` or higher for the corresponding element of the version
number.  A `(start-nat end-nat)` matches any number in the range
`start-nat` to `end-nat`, inclusive. A `(= nat)` matches only exactly
`nat`. A `(- nat)` matches `nat` or lower.
Examples:

```racket
> (module m (lib "racket")                                         
    (require (planet "random.rkt" ("schematics" "random.plt" 1 0)))
    (display (random-gaussian)))                                   
> (require 'm)                                                     
0.9050686838895684                                                 
```
The automatic `".ss"` and `".rkt"` conversions apply as with other
forms.

```racket
(file string)
```
Refers to a file, where `string` is a relative or absolute path using
the current platform’s conventions. This form is not portable, and it
should _not_ be used when a plain, portable `rel-string` suffices.
The automatic `".ss"` and `".rkt"` conversions apply as with other
forms.

```racket
(submod base element ...+)
                          
base    = module-path     
        | "."             
        | ".."            
                          
element = id              
        | ".."            
```
Refers to a submodule of `base`. The sequence of `element`s within
`submod` specify a path of submodule names to reach the final submodule.
Examples:

```racket
> (module zoo racket                    
    (module monkey-house racket         
      (provide monkey)                  
      (define monkey "Curious George")))
> (require (submod 'zoo monkey-house))  
> monkey                                
"Curious George"                        
```
Using `"."` as `base` within `submod` stands for the enclosing module.
Using `".."` as `base` is equivalent to using `"."` followed by an extra
`".."`. When a path of the form `(quote id)` refers to a submodule, it
is equivalent to `(submod "." id)`.
Using `".."` as an `element` cancels one submodule step, effectively
referring to the enclosing module. For example, `(submod "..")` refers
to the enclosing module of the submodule in which the path appears.
Examples:

```racket
> (module zoo racket                      
    (module monkey-house racket           
      (provide monkey)                    
      (define monkey "Curious George"))   
    (module crocodile-house racket        
      (require (submod ".." monkey-house))
      (provide dinner)                    
      (define dinner monkey)))            
> (require (submod 'zoo crocodile-house)) 
> dinner                                  
"Curious George"                          
```

## 4. Imports: `require`

The `require` form imports from another module. A `require` form can
appear within a module, in which case it introduces bindings from the
specified module into importing module. A `require` form can also appear
at the top level, in which case it both imports bindings and
_instantiates_ the specified module; that is, it evaluates the body
definitions and expressions of the specified module, if they have not
been evaluated already.

A single `require` can specify multiple imports at once:

```racket
(require require-spec ...)
```

Specifying multiple `require-spec`s in a single `require` is essentially
the same as using multiple `require`s, each with a single
`require-spec`. The difference is minor, and confined to the top-level:
a single `require` can import a given identifier at most once, whereas a
separate `require` can replace the bindings of a previous `require`
\(both only at the top level, outside of a module\).

The allowed shape of a `require-spec` is defined recursively:

```racket
module-path
```
In its simplest form, a `require-spec` is a `module-path` \(as defined
in the previous section, Module Paths\). In this case, the bindings
introduced by `require` are determined by `provide` declarations within
each module referenced by each `module-path`.
Examples:

```racket
> (module m racket        
    (provide color)       
    (define color "blue"))
> (module n racket        
    (provide size)        
    (define size 17))     
> (require 'm 'n)         
> (list color size)       
'("blue" 17)              
```

```racket
(only-in require-spec id-maybe-renamed ...)
                                           
id-maybe-renamed = id                      
                 | [orig-id bind-id]       
```
An `only-in` form limits the set of bindings that would be introduced by
a base `require-spec`. Also, `only-in` optionally renames each binding
that is preserved: in a `[orig-id bind-id]` form, the `orig-id` refers
to a binding implied by `require-spec`, and `bind-id` is the name that
will be bound in the importing context instead of `orig-id`.
Examples:

```racket
> (module m (lib "racket")                           
    (provide tastes-great?                           
             less-filling?)                          
    (define tastes-great? #t)                        
    (define less-filling? #t))                       
> (require (only-in 'm tastes-great?))               
> tastes-great?                                      
#t                                                   
> less-filling?                                      
less-filling?: undefined;                            
 cannot reference an identifier before its definition
  in module: top-level                               
> (require (only-in 'm [less-filling? lite?]))       
> lite?                                              
#t                                                   
```

```racket
(except-in require-spec id ...)
```
This form is the complement of `only-in`: it excludes specific bindings
from the set specified by `require-spec`.

```racket
(rename-in require-spec [orig-id bind-id] ...)
```
This form supports renaming like `only-in`, but leaving alone
identifiers from `require-spec` that are not mentioned as an `orig-id`.

```racket
(prefix-in prefix-id require-spec)
```
This is a shorthand for renaming, where `prefix-id` is added to the
front of each identifier specified by `require-spec`.

The `only-in`, `except-in`, `rename-in`, and `prefix-in` forms can be
nested to implement more complex manipulations of imported bindings. For
example,

`(require` `(prefix-in` `m:` `(except-in` `'m` `ghost)))`

imports all bindings that `m` exports, except for the `ghost` binding,
and with local names that are prefixed with `m:`.

Equivalently, the `prefix-in` could be applied before `except-in`, as
long as the omission with `except-in` is specified using the `m:`
prefix:

`(require` `(except-in` `(prefix-in` `m:` `'m)` `m:ghost))`

## 5. Exports: `provide`

By default, all of a module’s definitions are private to the module. The
`provide` form specifies definitions to be made available where the
module is `require`d.

```racket
(provide provide-spec ...)
```

A `provide` form can only appear at module level \(i.e., in the
immediate body of a `module`\).  Specifying multiple `provide-spec`s in
a single `provide` is exactly the same as using multiple `provide`s each
with a single `provide-spec`.

Each identifier can be exported at most once from a module across all
`provide`s within the module. More precisely, the external name for each
export must be distinct; the same internal binding can be exported
multiple times with different external names.

The allowed shape of a `provide-spec` is defined recursively:

```racket
identifier
```
In its simplest form, a `provide-spec` indicates a binding within its
module to be exported. The binding can be from either a local
definition, or from an import.

```racket
(rename-out [orig-id export-id] ...)
```
A `rename-out` form is similar to just specifying an identifier, but the
exported binding `orig-id` is given a different name, `export-id`, to
importing modules.

```racket
(struct-out struct-id)
```
A `struct-out` form exports the bindings created by `(struct struct-id
....)`.
> +See \[missing\] for information on `define-struct`.

```racket
(all-defined-out)
```
The `all-defined-out` shorthand exports all bindings that are defined
within the exporting module \(as opposed to imported\).
Use of the `all-defined-out` shorthand is generally discouraged, because
it makes less clear the actual exports for a module, and because Racket
programmers get into the habit of thinking that definitions can be added
freely to a module without affecting its public interface \(which is not
the case when `all-defined-out` is used\).

```racket
(all-from-out module-path)
```
The `all-from-out` shorthand exports all bindings in the module that
were imported using a `require-spec` that is based on `module-path`.
Although different `module-path`s could refer to the same file-based
module, re-exporting with `all-from-out` is based specifically on the
`module-path` reference, and not the module that is actually referenced.

```racket
(except-out provide-spec id ...)
```
Like `provide-spec`, but omitting the export of each `id`, where `id` is
the external name of the binding to omit.

```racket
(prefix-out prefix-id provide-spec)
```
Like `provide-spec`, but adding `prefix-id` to the beginning of the
external name for each exported binding.

## 6. Assignment and Redefinition

The use of `set!` on variables defined within a module is limited to the
body of the defining module. That is, a module is allowed to change the
value of its own definitions, and such changes are visible to importing
modules. However, an importing context is not allowed to change the
value of an imported binding.

Examples:

```racket
> (module m racket                            
    (provide counter increment!)              
    (define counter 0)                        
    (define (increment!)                      
      (set! counter (add1 counter))))         
> (require 'm)                                
> counter                                     
0                                             
> (increment!)                                
> counter                                     
1                                             
> (set! counter -1)                           
set!: cannot mutate module-required identifier
  at: counter                                 
  in: (set! counter -1)                       
```

As the above example illustrates, a module can always grant others the
ability to change its exports by providing a mutator function, such as
`increment!`.

The prohibition on assignment of imported variables helps support
modular reasoning about programs. For example, in the module,

```racket
(module m racket                 
  (provide rx:fish fishy-string?)
  (define rx:fish #rx"fish")     
  (define (fishy-string? s)      
    (regexp-match? rx:fish s)))  
```

the function `fishy-string?` will always match strings that contain
“fish”, no matter how other modules use the `rx:fish` binding.  For
essentially the same reason that it helps programmers, the prohibition
on assignment to imports also allows many programs to be executed more
efficiently.

Along the same lines, when a module contains no `set!` of a particular
identifier that is defined within the module, then the identifier is
considered a _constant_ that cannot be changed—not even by re-declaring
the module.

Consequently, re-declaration of a module is not generally allowed. For
file-based modules, simply changing the file does not lead to a
re-declaration in any case, because file-based modules are loaded on
demand, and the previously loaded declarations satisfy future requests.
It is possible to use Racket’s reflection support to re-declare a
module, however, and non-file modules can be re-declared in the REPL; in
such cases, the re-declaration may fail if it involves the re-definition
of a previously constant binding.

```racket
> (module m racket                   
    (define pie 3.141597))           
> (require 'm)                       
> (module m racket                   
    (define pie 3))                  
define-values: assignment disallowed;
 cannot re-define a constant         
  constant: pie                      
  in module: 'm                      
```

For exploration and debugging purposes, the Racket reflective layer
provides a `compile-enforce-module-constants` parameter to disable the
enforcement of constants.

```racket
> (compile-enforce-module-constants #f)
> (module m2 racket                    
    (provide pie)                      
    (define pie 3.141597))             
> (require 'm2)                        
> (module m2 racket                    
    (provide pie)                      
    (define pie 3))                    
> (compile-enforce-module-constants #t)
> pie                                  
3                                      
```

## 7. Modules and Macros

Racket’s module system cooperates closely with Racket’s macro system for
adding new syntactic forms to Racket. For example, in the same way that
importing `racket/base` introduces syntax for `require` and `lambda`,
importing other modules can introduce new syntactic forms \(in addition
to more traditional kinds of imports, such as functions or constants\).

We introduce macros in more detail later, in \[missing\], but here’s a
simple example of a module that defines a pattern-based macro:

```racket
(module noisy racket                                   
  (provide define-noisy)                               
                                                       
  (define-syntax-rule (define-noisy (id arg ...) body) 
    (define (id arg ...)                               
      (show-arguments 'id  (list arg ...))             
      body))                                           
                                                       
  (define (show-arguments name args)                   
    (printf "calling ~s with arguments ~e" name args)))
```

The `define-noisy` binding provided by this module is a macro that acts
like `define` for a function, but it causes each call to the function to
print the arguments that are provided to the function:

```racket
> (require 'noisy)             
> (define-noisy (f x y)        
    (+ x y))                   
> (f 1 2)                      
calling f with arguments '(1 2)
3                              
```

Roughly, the `define-noisy` form works by replacing

```racket
(define-noisy (f x y)
  (+ x y))           
```

with

```racket
(define (f x y)                 
  (show-arguments 'f (list x y))
  (+ x y))                      
```

Since `show-arguments` isn’t provided by the `noisy` module, however,
this literal textual replacement is not quite right. The actual
replacement correctly tracks the origin of identifiers like
`show-arguments`, so they can refer to other definitions in the place
where the macro is defined—even if those identifiers are not available
at the place where the macro is used.

There’s more to the macro and module interaction than identifier
binding. The `define-syntax-rule` form is itself a macro, and it expands
to compile-time code that implements the transformation from
`define-noisy` into `define`. The module system keeps track of which
code needs to run at compile and which needs to run normally, as
explained more in \[missing\] and \[missing\].
