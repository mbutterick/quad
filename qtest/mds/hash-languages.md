# Defining new `#lang` Languages

When loading a module as a source program that starts

`#lang` `language`

the `language` determines the way that the rest of the module is parsed
at the reader level. The reader-level parse must produce a `module` form
as a syntax object. As always, the second sub-form after `module`
specifies the module language that controls the meaning of the module’s
body forms. Thus, a `language` specified after `#lang` controls both the
reader-level and expander-level parsing of a module.

    1 Designating a `#lang` Language          
                                              
    2 Using `#lang reader`                    
                                              
    3 Using `#lang s-exp syntax/module-reader`
                                              
    4 Installing a Language                   
                                              
    5 Source-Handling Configuration           
                                              
    6 Module-Handling Configuration           

## 1. Designating a `#lang` Language

The syntax of a `language` intentionally overlaps with the syntax of a
module path as used in `require` or as a module language, so that names
like `racket`, `racket/base`, `slideshow`, or `scribble/manual` can be
used both as `#lang` languages and as module paths.

At the same time, the syntax of `language` is far more restricted than a
module path, because only `a`-`z`, `A`-`Z`, `0`-`9`, `/` \(not at the
start or end\), `_`, `-`, and `+` are allowed in a `language` name.
These restrictions keep the syntax of `#lang` as simple as possible.
Keeping the syntax of `#lang` simple, in turn, is important because the
syntax is inherently inflexible and non-extensible; the `#lang` protocol
allows a `language` to refine and define syntax in a practically
unconstrained way, but the `#lang` protocol itself must remain fixed so
that various different tools can “boot” into the extended world.

Fortunately, the `#lang` protocol provides a natural way to refer to
languages in ways other than the rigid `language` syntax: by defining a
`language` that implements its own nested protocol. We have already seen
one example \(in \[missing\]\): the `s-exp` `language` allows a
programmer to specify a module language using the general module path
syntax. Meanwhile, `s-exp` takes care of the reader-level
responsibilities of a `#lang` language.

Unlike `racket`, `s-exp` cannot be used as a module path with `require`.
Although the syntax of `language` for `#lang` overlaps with the syntax
of module paths, a `language` is not used directly as a module path.
Instead, a `language` obtains a module path by trying two locations:
first, it looks for a `reader` submodule of the main module for
`language`. If this is not a valid module path, then `language`  is
suffixed with `/lang/reader`. \(If neither is a valid module path, an
error is raised.\) The resulting module supplies `read` and
`read-syntax` functions using a protocol that is similar to the one for
`#reader`.

> +\[missing\] introduces `#reader`.

A consequence of the way that a `#lang` `language` is turned into a
module path is that the language must be installed in a collection,
similar to the way that `"racket"` or `"slideshow"` are collections that
are distributed with Racket. Again, however, there’s an escape from this
restriction: the `reader` language lets you specify a reader-level
implementation of a language using a general module path.

## 2. Using `#lang`` ``reader`

The `reader` language for `#lang` is similar to `s-exp`, in that it acts
as a kind of meta-language. Whereas `s-exp` lets a programmer specify a
module language at the expander layer of parsing, `reader` lets a
programmer specify a language at the reader level.

A `#lang reader` must be followed by a module path, and the specified
module must provide two functions: `read` and `read-syntax`. The
protocol is the same as for a `#reader` implementation, but for `#lang`,
the `read` and `read-syntax` functions must produce a `module` form that
is based on the rest of the input file for the module.

The following `"literal.rkt"` module implements a language that treats
its entire body as literal text and exports the text as a `data` string:

`"literal.rkt"`
```racket
#lang racket                                            
(require syntax/strip-context)                          
                                                        
(provide (rename-out [literal-read read]                
                     [literal-read-syntax read-syntax]))
                                                        
(define (literal-read in)                               
  (syntax->datum                                        
   (literal-read-syntax #f in)))                        
                                                        
(define (literal-read-syntax src in)                    
  (with-syntax ([str (port->string in)])                
    (strip-context                                      
     #'(module anything racket                          
         (provide data)                                 
         (define data 'str)))))                         
```

The `"literal.rkt"` language uses `strip-context` on the generated
`module` expression, because a `read-syntax` function should return a
syntax object with no lexical context. Also, the `"literal.rkt"`
language creates a module named `anything`, which is an arbitrary
choice; the language is intended to be used in a file, and the longhand
module name is ignored when it appears in a `require`d file.

The `"literal.rkt"` language can be used in a module `"tuvalu.rkt"`:

`"tuvalu.rkt"`
```racket
#lang reader "literal.rkt"
Technology!               
System!                   
Perfect!                  
```

Importing `"tuvalu.rkt"` binds `data` to a string version of the module
content:

```racket
> (require "tuvalu.rkt")            
> data                              
"\nTechnology!\nSystem!\nPerfect!\n"
```

## 3. Using `#lang`` ``s-exp`` ``syntax/module-reader`

Parsing a module body is usually not as trivial as in `"literal.rkt"`. A
more typical module parser must iterate to parse multiple forms for a
module body. A language is also more likely to extend Racket
syntax—perhaps through a readtable—instead of replacing Racket syntax
completely.

The `syntax/module-reader` module language abstracts over common parts
of a language implementation to simplify the creation of new languages.
In its most basic form, a language implemented with
`syntax/module-reader` simply specifies the module language to be used
for the language, in which case the reader layer of the language is the
same as Racket. For example, with

`"raquet-mlang.rkt"`
```racket
#lang racket                                      
(provide (except-out (all-from-out racket) lambda)
         (rename-out [lambda function]))          
```

and

`"raquet.rkt"`
```racket
#lang s-exp syntax/module-reader
"raquet-mlang.rkt"              
```

then

```racket
#lang reader "raquet.rkt"         
(define identity (function (x) x))
(provide identity)                
```

implements and exports the `identity` function, since
`"raquet-mlang.rkt"` exports `lambda` as `function`.

The `syntax/module-reader` language accepts many optional specifications
to adjust other features of the language. For example, an alternate
`read` and `read-syntax` for parsing the language can be specified with
`#:read` and `#:read-syntax`, respectively. The following
`"dollar-racket.rkt"` language uses `"dollar.rkt"` \(see \[missing\]\)
to build a language that is like `racket` but with a `$` escape to
simple infix arithmetic:

`"dollar-racket.rkt"`
```racket
#lang s-exp syntax/module-reader     
racket                               
#:read $-read                        
#:read-syntax $-read-syntax          
                                     
(require (prefix-in $- "dollar.rkt"))
```

The `require` form appears at the end of the module, because all of the
keyword-tagged optional specifications for `syntax/module-reader` must
appear before any helper imports or definitions.

The following module uses `"dollar-racket.rkt"` to implement a `cost`
function using a `$` escape:

`"store.rkt"`
```racket
#lang reader "dollar-racket.rkt"        
                                        
(provide cost)                          
                                        
; Cost of ‘n' $1 rackets with 7% sales  
; tax and shipping-and-handling fee ‘h':
(define (cost n h)                      
  $n*107/100+h$)                        
```

## 4. Installing a Language

So far, we have used the `reader` meta-language to access languages like
`"literal.rkt"` and `"dollar-racket.rkt"`. If you want to use something
like `#lang literal` directly, then you must move `"literal.rkt"` into a
Racket collection named `"literal"` \(see also \[missing\]\).
Specifically, move `"literal.rkt"` to a `reader` submodule of
`"literal/main.rkt"` for any directory name `"literal"`, like so:

`"literal/main.rkt"`
```racket
#lang racket                                              
                                                          
(module reader racket                                     
  (require syntax/strip-context)                          
                                                          
  (provide (rename-out [literal-read read]                
                       [literal-read-syntax read-syntax]))
                                                          
  (define (literal-read in)                               
    (syntax->datum                                        
     (literal-read-syntax #f in)))                        
                                                          
  (define (literal-read-syntax src in)                    
    (with-syntax ([str (port->string in)])                
      (strip-context                                      
       #'(module anything racket                          
           (provide data)                                 
           (define data 'str))))))                        
```

Then, install the `"literal"` directory as a package:

  `cd /path/to/literal ; raco pkg install`

After moving the file and installing the package, you can use `literal`
directly after `#lang`:

```racket
#lang literal
Technology!  
System!      
Perfect!     
```

> See \[missing\] for more information on using `raco`.

You can also make your language available for others to install by using
the Racket package manager \(see \[missing\]\). After you create a
`"literal"` package and register it with the Racket package catalog
\(see \[missing\]\), others can install it using `raco pkg`:

  `raco pkg install literal`

Once installed, others can invoke the language the same way: by using
`#lang literal` at the top of a source file.

If you use a public source repository \(e.g., GitHub\), you can link
your package to the source. As you improve the package, others can
update their version using `raco pkg`:

  `raco pkg update literal`

> See \[missing\] for more information about the Racket package manager.

## 5. Source-Handling Configuration

The Racket distribution includes a Scribble language for writing prose
documents, where Scribble extends the normal Racket to better support
text. Here is an example Scribble document:

  `#lang scribble/base`                            
                                                   
  `@(define (get-name) "Self-Describing Document")`
                                                   
  `@title[(get-name)]`                             
                                                   
  `The title of this document is “@(get-name).”`   

If you put that program in DrRacket’s definitions area and click Run,
then nothing much appears to happen. The `scribble/base` language just
binds and exports `doc` as a description of a document, similar to the
way that `"literal.rkt"` exports a string as `data`.

Simply opening a module with the language `scribble/base` in DrRacket,
however, causes a Scribble HTML button to appear. Furthermore, DrRacket
knows how to colorize Scribble syntax by coloring green those parts of
the document that correspond to literal text. The language name
`scribble/base` is not hard-wired into DrRacket. Instead, the
implementation of the `scribble/base` language provides button and
syntax-coloring information in response to a query from DrRacket.

If you have installed the `literal` language as described in Installing
a Language, then you can adjust `"literal/main.rkt"` so that DrRacket
treats the content of a module in the `literal` language as plain text
instead of \(erroneously\) as Racket syntax:

`"literal/main.rkt"`
```racket
#lang racket                                             
                                                         
(module reader racket                                    
  (require syntax/strip-context)                         
                                                         
  (provide (rename-out [literal-read read]               
                       [literal-read-syntax read-syntax])
           get-info)                                     
                                                         
  (define (literal-read in)                              
    (syntax->datum                                       
     (literal-read-syntax #f in)))                       
                                                         
  (define (literal-read-syntax src in)                   
    (with-syntax ([str (port->string in)])               
      (strip-context                                     
       #'(module anything racket                         
           (provide data)                                
           (define data 'str)))))                        
                                                         
  (define (get-info in mod line col pos)                 
    (lambda (key default)                                
      (case key                                          
        [(color-lexer)                                   
         (dynamic-require 'syntax-color/default-lexer    
                          'default-lexer)]               
        [else default]))))                               
```

This revised `literal` implementation provides a `get-info` function.
The `get-info` function is called by `read-language` \(which DrRacket
calls\) with the source input stream and location information, in case
query results should depend on the content of the module after the
language name \(which is not the case for `literal`\). The result of
`get-info` is a function of two arguments. The first argument is always
a symbol, indicating the kind of information that a tool requests from
the language; the second argument is the default result to be returned
if the language does not recognize the query or has no information for
it.

After DrRacket obtains the result of `get-info` for a language, it calls
the function with a `'color-lexer` query; the result should be a
function that implements syntax-coloring parsing on an input stream. For
`literal`, the `syntax-color/default-lexer` module provides a
`default-lexer` syntax-coloring parser that is suitable for plain text,
so `literal` loads and returns that parser in response to a
`'color-lexer` query.

The set of symbols that a programming tool uses for queries is entirely
between the tool and the languages that choose to cooperate with it. For
example, in addition to `'color-lexer`, DrRacket uses a
`'drracket:toolbar-buttons` query to determine which buttons should be
available in the toolbar to operate on modules using the language.

The `syntax/module-reader` language lets you specify `get-info` handling
through a `#:info` optional specification. The protocol for an `#:info`
function is slightly different from the raw `get-info` protocol; the
revised protocol allows `syntax/module-reader` the possibility of
handling future language-information queries automatically.

## 6. Module-Handling Configuration

Suppose that the file `"death-list-5.rkt"` contains

`"death-list-5.rkt"`
```racket
#lang racket         
(list "O-Ren Ishii"  
      "Vernita Green"
      "Budd"         
      "Elle Driver"  
      "Bill")        
```

If you `require` `"death-list-5.rkt"` directly, then it prints the list
in the usual Racket result format:

```racket
> (require "death-list-5.rkt")                              
'("O-Ren Ishii" "Vernita Green" "Budd" "Elle Driver" "Bill")
```

However, if `"death-list-5.rkt"` is required by a `"kiddo.rkt"` that is
implemented with `scheme` instead of `racket`:

`"kiddo.rkt"`
```racket
#lang scheme                
(require "death-list-5.rkt")
```

then, if you run `"kiddo.rkt"` file in DrRacket or if you run it
directly with `racket`, `"kiddo.rkt"` causes `"death-list-5.rkt"` to
print its list in traditional Scheme format, without the leading quote:

`("O-Ren Ishii" "Vernita Green" "Budd" "Elle Driver" "Bill")`

The `"kiddo.rkt"` example illustrates how the format for printing a
result value can depend on the main module of a program instead of the
language that is used to implement it.

More broadly, certain features of a language are only invoked when a
module written in that language is run directly with `racket` \(as
opposed to being imported into another module\). One example is
result-printing style \(as shown above\). Another example is REPL
behavior. These features are part of what’s called the _run-time
configuration_ of a language.

Unlike the syntax-coloring property of a language \(as described in
Source-Handling Configuration\), the run-time configuration is a
property of a _module_ per se as opposed to a property of the _source
text_ representing the module. For that reason, the run-time
configuration for a module needs to be available even if the module is
compiled to bytecode form and the source is unavailable. Therefore,
run-time configuration cannot be handled by the `get-info` function
we’re exporting from the language’s parser module.

Instead, it will be handled by a new `configure-runtime` submodule that
we’ll add inside the parsed `module` form. When a module is run directly
with `racket`, `racket` looks for a `configure-runtime` submodule. If it
exists, `racket` runs it. But if the module is imported into another
module, the `'configure-runtime` submodule is ignored. \(And if the
`configure-runtime` submodule doesn’t exist, `racket` just evaluates the
module as usual.\) That means that the `configure-runtime` submodule can
be used for any special setup tasks that need to happen when the module
is run directly.

Going back to the `literal` language \(see Source-Handling
Configuration\), we can adjust the language so that directly running a
`literal` module causes it to print out its string, while using a
`literal` module in a larger program simply provides `data` without
printing. To make this work, we will need an extra module. \(For clarity
here, we will implement this module as a separate file. But it could
equally well be a submodule of an existing file.\)

```racket
.... (the main installation or the user’s space)   
|- "literal"                                       
   |- "main.rkt"            (with reader submodule)
   |- "show.rkt"            (new)                  
```

* The `"literal/show.rkt"` module will provide a `show` function to be
  applied to the string content of a `literal` module, and also provide
  a `show-enabled` parameter that controls whether `show` actually
  prints the result.

* The new `configure-runtime` submodule in `"literal/main.rkt"` will set
  the `show-enabled` parameter to `#t`. The net effect is that `show`
  will print the strings that it’s given, but only when a module using
  the `literal` language is run directly \(because only then will the
  `configure-runtime` submodule be invoked\).

These changes are implemented in the following revised
`"literal/main.rkt"`:

`"literal/main.rkt"`
```racket
#lang racket                                             
                                                         
(module reader racket                                    
  (require syntax/strip-context)                         
                                                         
  (provide (rename-out [literal-read read]               
                       [literal-read-syntax read-syntax])
           get-info)                                     
                                                         
  (define (literal-read in)                              
    (syntax->datum                                       
     (literal-read-syntax #f in)))                       
                                                         
  (define (literal-read-syntax src in)                   
    (with-syntax ([str (port->string in)])               
      (strip-context                                     
       #'(module anything racket                         
           (module configure-runtime racket              
             (require literal/show)                      
             (show-enabled #t))                          
           (require literal/show)                        
           (provide data)                                
           (define data 'str)                            
           (show data)))))                               
                                                         
  (define (get-info in mod line col pos)                 
    (lambda (key default)                                
      (case key                                          
        [(color-lexer)                                   
         (dynamic-require 'syntax-color/default-lexer    
                          'default-lexer)]               
        [else default]))))                               
```

Then the `"literal/show.rkt"` module must provide the `show-enabled`
parameter and `show` function:

`"literal/show.rkt"`
```racket
#lang racket                             
                                         
(provide show show-enabled)              
                                         
(define show-enabled (make-parameter #f))
                                         
(define (show v)                         
  (when (show-enabled)                   
    (display v)))                        
```

With all of the pieces for `literal` in place, try running the following
variant of `"tuvalu.rkt"` directly and through a `require` from another
module:

`"tuvalu.rkt"`
```racket
#lang literal
Technology!  
System!      
Perfect!     
```

When run directly, we’ll see the result printed like so, because our
`configure-runtime` submodule will have set the `show-enabled` parameter
to `#t`:

`Technology!  System!  Perfect!`

But when imported into another module, printing will be suppressed,
because the `configure-runtime` submodule will not be invoked, and
therefore the `show-enabled` parameter will remain at its default value
of `#f`.
