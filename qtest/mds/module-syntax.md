# Module Syntax

The `#lang` at the start of a module file begins a shorthand for a
`module` form, much like `'` is a shorthand for a `quote` form. Unlike
`'`, the `#lang` shorthand does not work well in a REPL, in part because
it must be terminated by an end-of-file, but also because the longhand
expansion of `#lang` depends on the name of the enclosing file.

## 1. The `module` Form

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

## 2. The `#lang` Shorthand

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

## 3. Submodules

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

## 4. Main and Test Submodules

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
