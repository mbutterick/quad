# Creating Languages

The macro facilities defined in the preceding chapter let a programmer
define syntactic extensions to a language, but a macro is limited in two
ways:

* a macro cannot restrict the syntax available in its context or change
  the meaning of surrounding forms; and

* a macro can extend the syntax of a language only within the parameters
  of the language’s lexical conventions, such as using parentheses to
  group the macro name with its subforms and using the core syntax of
  identifiers, keywords, and literals.

> +The distinction between the reader and expander layer is introduced in
> \[missing\].

That is, a macro can only extend a language, and it can do so only at
the expander layer. Racket offers additional facilities for defining a
starting point of the expander layer, for extending the reader layer,
for defining the starting point of the reader layer, and for packaging a
reader and expander starting point into a conveniently named language.

    1 Module Languages                            
      1.1 Implicit Form Bindings                  
      1.2 Using `#lang s-exp`                     
                                                  
    2 Reader Extensions                           
      2.1 Source Locations                        
      2.2 Readtables                              
                                                  
    3 Defining new `#lang` Languages              
      3.1 Designating a `#lang` Language          
      3.2 Using `#lang reader`                    
      3.3 Using `#lang s-exp syntax/module-reader`
      3.4 Installing a Language                   
      3.5 Source-Handling Configuration           
      3.6 Module-Handling Configuration           

## 1. Module Languages

When using the longhand `module` form for writing modules, the module
path that is specified after the new module’s name provides the initial
imports for the module. Since the initial-import module determines even
the most basic bindings that are available in a module’s body, such as
`require`, the initial import can be called a _module language_.

The most common module languages are `racket` or `racket/base`, but you
can define your own module language by defining a suitable module. For
example, using `provide` subforms like `all-from-out`, `except-out`, and
`rename-out`, you can add, remove, or rename bindings from `racket` to
produce a module language that is a variant of `racket`:

> +\[missing\] introduces the longhand `module` form.

```racket
> (module raquet racket                                    
    (provide (except-out (all-from-out racket) lambda)     
             (rename-out [lambda function])))              
> (module score 'raquet                                    
    (map (function (points) (case points                   
                             [(0) "love"] [(1) "fifteen"]  
                             [(2) "thirty"] [(3) "forty"]))
         (list 0 2)))                                      
> (require 'score)                                         
'("love" "thirty")                                         
```

### 1.1. Implicit Form Bindings

If you try to remove too much from `racket` in defining your own module
language, then the resulting module will no longer work right as a
module language:

```racket
> (module just-lambda racket                               
    (provide lambda))                                      
> (module identity 'just-lambda                            
    (lambda (x) x))                                        
eval:2:0: module: no #%module-begin binding in the module's
language                                                   
  in: (module identity (quote just-lambda) (lambda (x) x)) 
```

The `#%module-begin` form is an implicit form that wraps the body of a
module. It must be provided by a module that is to be used as module
language:

```racket
> (module just-lambda racket        
    (provide lambda #%module-begin))
> (module identity 'just-lambda     
    (lambda (x) x))                 
> (require 'identity)               
#<procedure>                        
```

The other implicit forms provided by `racket/base` are `#%app` for
function calls, `#%datum` for literals, and `#%top` for identifiers that
have no binding:

```racket
> (module just-lambda racket        
    (provide lambda #%module-begin  
             ; ten needs these, too:
             #%app #%datum))        
> (module ten 'just-lambda          
    ((lambda (x) x) 10))            
> (require 'ten)                    
10                                  
```

Implicit forms such as `#%app` can be used explicitly in a module, but
they exist mainly to allow a module language to restrict or change the
meaning of implicit uses. For example, a `lambda-calculus` module
language might restrict functions to a single argument, restrict
function calls to supply a single argument, restrict the module body to
a single expression, disallow literals, and treat unbound identifiers as
uninterpreted symbols:

```racket
> (module lambda-calculus racket                             
    (provide (rename-out [1-arg-lambda lambda]               
                         [1-arg-app #%app]                   
                         [1-form-module-begin #%module-begin]
                         [no-literals #%datum]               
                         [unbound-as-quoted #%top]))         
    (define-syntax-rule (1-arg-lambda (x) expr)              
      (lambda (x) expr))                                     
    (define-syntax-rule (1-arg-app e1 e2)                    
      (#%app e1 e2))                                         
    (define-syntax-rule (1-form-module-begin e)              
      (#%module-begin e))                                    
    (define-syntax (no-literals stx)                         
      (raise-syntax-error #f "no" stx))                      
    (define-syntax-rule (unbound-as-quoted . id)             
      'id))                                                  
> (module ok 'lambda-calculus                                
    ((lambda (x) (x z))                                      
     (lambda (y) y)))                                        
> (require 'ok)                                              
'z                                                           
> (module not-ok 'lambda-calculus                            
    (lambda (x y) x))                                        
eval:4:0: lambda: use does not match pattern: (lambda (x)    
expr)                                                        
  in: (lambda (x y) x)                                       
> (module not-ok 'lambda-calculus                            
    (lambda (x) x)                                           
    (lambda (y) (y y)))                                      
eval:5:0: #%module-begin: use does not match pattern:        
(#%module-begin e)                                           
  in: (#%module-begin (lambda (x) x) (lambda (y) (y y)))     
> (module not-ok 'lambda-calculus                            
    (lambda (x) (x x x)))                                    
eval:6:0: #%app: use does not match pattern: (#%app e1 e2)   
  in: (#%app x x x)                                          
> (module not-ok 'lambda-calculus                            
    10)                                                      
eval:7:0: #%datum: no                                        
  in: (#%datum . 10)                                         
```

Module languages rarely redefine `#%app`, `#%datum`, and `#%top`, but
redefining `#%module-begin` is more frequently useful. For example, when
using modules to construct descriptions of HTML pages where a
description is exported from the module as `page`, an alternate
`#%module-begin` can help eliminate `provide` and quasiquoting
boilerplate, as in `"html.rkt"`:

`"html.rkt"`
```racket
#lang racket                                          
(require racket/date)                                 
                                                      
(provide (except-out (all-from-out racket)            
                     #%module-begin)                  
         (rename-out [module-begin #%module-begin])   
         now)                                         
                                                      
(define-syntax-rule (module-begin expr ...)           
  (#%module-begin                                     
   (define page `(html expr ...))                     
   (provide page)))                                   
                                                      
(define (now)                                         
  (parameterize ([date-display-format 'iso-8601])     
    (date->string (seconds->date (current-seconds)))))
```

Using the `"html.rkt"` module language, a simple web page can be
described without having to explicitly define or export `page` and
starting in `quasiquote`d mode instead of expression mode:

```racket
> (module lady-with-the-spinning-head "html.rkt"                
    (title "Queen of Diamonds")                                 
    (p "Updated: " ,(now)))                                     
> (require 'lady-with-the-spinning-head)                        
> page                                                          
'(html (title "Queen of Diamonds") (p "Updated: " "2019-01-21"))
```

### 1.2. Using `#lang`` ``s-exp`

Implementing a language at the level of `#lang` is more complex than
declaring a single module, because `#lang` lets programmers control
several different facets of a language. The `s-exp` language, however,
acts as a kind of meta-language for using a module language with the
`#lang` shorthand:

```racket
#lang s-exp module-name
form ...               
```

is the same as

```racket
(module name module-name
  form ...)             
```

where `name` is derived from the source file containing the `#lang`
program. The name `s-exp` is short for “S-expression,” which is a
traditional name for Racket’s reader-level lexical conventions:
parentheses, identifiers, numbers, double-quoted strings with certain
backslash escapes, and so on.

Using `#lang s-exp`, the `lady-with-the-spinning-head` example from
before can be written more compactly as:

```racket
#lang s-exp "html.rkt"     
                           
(title "Queen of Diamonds")
(p "Updated: " ,(now))     
```

Later in this guide, Defining new `#lang` Languages explains how to
define your own `#lang` language, but first we explain how you can write
reader-level extensions to Racket.

## 2. Reader Extensions

> +\[missing\] in \[missing\] provides more on reader extensions.

The reader layer of the Racket language can be extended through the
`#reader` form. A reader extension is implemented as a module that is
named after `#reader`. The module exports functions that parse raw
characters into a form to be consumed by the expander layer.

The syntax of `#reader` is

`#reader` >_module-path_< >_reader-specific_<

where >_module-path_< names a module that provides `read` and
`read-syntax` functions. The >_reader-specific_< part is a sequence of
characters that is parsed as determined by the `read` and `read-syntax`
functions from >_module-path_<.

For example, suppose that file `"five.rkt"` contains

`"five.rkt"`
```racket
#lang racket/base                                      
                                                       
(provide read read-syntax)                             
                                                       
(define (read in) (list (read-string 5 in)))           
(define (read-syntax src in) (list (read-string 5 in)))
```

Then, the program

```racket
#lang racket/base             
                              
'(1 #reader"five.rkt"234567 8)
```

is equivalent to

```racket
#lang racket/base 
                  
'(1 ("23456") 7 8)
```

because the `read` and `read-syntax` functions of `"five.rkt"` both read
five characters from the input stream and put them into a string and
then a list. The reader functions from `"five.rkt"` are not obliged to
follow Racket lexical conventions and treat the continuous sequence
`234567` as a single number. Since only the `23456` part is consumed by
`read` or `read-syntax`, the `7` remains to be parsed in the usual
Racket way. Similarly, the reader functions from `"five.rkt"` are not
obliged to ignore whitespace, and

```racket
#lang racket/base              
                               
'(1 #reader"five.rkt" 234567 8)
```

is equivalent to

```racket
#lang racket/base  
                   
'(1 (" 2345") 67 8)
```

since the first character immediately after `"five.rkt"` is a space.

A `#reader` form can be used in the REPL, too:

```racket
> '#reader"five.rkt"abcde
'("abcde")               
```

### 2.1. Source Locations

The difference between `read` and `read-syntax` is that `read` is meant
to be used for data while `read-syntax` is meant to be used to parse
programs. More precisely, the `read` function will be used when the
enclosing stream is being parsed by the Racket `read`, and `read-syntax`
is used when the enclosing stream is being parsed by the Racket
`read-syntax` function. Nothing requires `read` and `read-syntax` to
parse input in the same way, but making them different would confuse
programmers and tools.

The `read-syntax` function can return the same kind of value as `read`,
but it should normally return a syntax object that connects the parsed
expression with source locations. Unlike the `"five.rkt"` example, the
`read-syntax` function is typically implemented directly to produce
syntax objects, and then `read` can use `read-syntax` and strip away
syntax object wrappers to produce a raw result.

The following `"arith.rkt"` module implements a reader to parse simple
infix arithmetic expressions into Racket forms. For example, `1*2+3`
parses into the Racket form `(+ (* 1 2) 3)`. The supported operators are
`+`, `-`, `*`, and `/`, while operands can be unsigned integers or
single-letter variables. The implementation uses `port-next-location` to
obtain the current source location, and it uses `datum->syntax` to turn
raw values into syntax objects.

`"arith.rkt"`
```racket
#lang racket                                                
(require syntax/readerr)                                    
                                                            
(provide read read-syntax)                                  
                                                            
(define (read in)                                           
  (syntax->datum (read-syntax #f in)))                      
                                                            
(define (read-syntax src in)                                
  (skip-whitespace in)                                      
  (read-arith src in))                                      
                                                            
(define (skip-whitespace in)                                
  (regexp-match #px"^\\s*" in))                             
                                                            
(define (read-arith src in)                                 
  (define-values (line col pos) (port-next-location in))    
  (define expr-match                                        
    (regexp-match                                           
     ; Match an operand followed by any number of           
     ; operator–operand sequences, and prohibit an          
     ; additional operator from following immediately:      
     #px"^([a-z]|[0-9]+)(?:[-+*/]([a-z]|[0-9]+))*(?![-+*/])"
     in))                                                   
                                                            
  (define (to-syntax v delta span-str)                      
    (datum->syntax #f v (make-srcloc delta span-str)))      
  (define (make-srcloc delta span-str)                      
    (and line                                               
         (vector src line (+ col delta) (+ pos delta)       
                 (string-length span-str))))                
                                                            
  (define (parse-expr s delta)                              
    (match (or (regexp-match #rx"^(.*?)([+-])(.*)$" s)      
               (regexp-match #rx"^(.*?)([*/])(.*)$" s))     
      [(list _ a-str op-str b-str)                          
       (define a-len (string-length a-str))                 
       (define a (parse-expr a-str delta))                  
       (define b (parse-expr b-str (+ delta 1 a-len)))      
       (define op (to-syntax (string->symbol op-str)        
                             (+ delta a-len) op-str))       
       (to-syntax (list op a b) delta s)]                   
      [_ (to-syntax (or (string->number s)                  
                        (string->symbol s))                 
                    delta s)]))                             
                                                            
  (unless expr-match                                        
    (raise-read-error "bad arithmetic syntax"               
                      src line col pos                      
                      (and pos (- (file-position in) pos))))
  (parse-expr (bytes->string/utf-8 (car expr-match)) 0))    
```

If the `"arith.rkt"` reader is used in an expression position, then its
parse result will be treated as a Racket expression. If it is used in a
quoted form, however, then it just produces a number or a list:

```racket
> #reader"arith.rkt" 1*2+3 
5                          
> '#reader"arith.rkt" 1*2+3
'(+ (* 1 2) 3)             
```

The `"arith.rkt"` reader could also be used in positions that make no
sense. Since the `read-syntax` implementation tracks source locations,
syntax errors can at least refer to parts of the input in terms of their
original locations \(at the beginning of the error message\):

```racket
> (let #reader"arith.rkt" 1*2+3 8)                          
repl:1:27: let: bad syntax (not an identifier and expression
for a binding)                                              
  at: +                                                     
  in: (let (+ (* 1 2) 3) 8)                                 
```

### 2.2. Readtables

A reader extension’s ability to parse input characters in an arbitrary
way can be powerful, but many cases of lexical extension call for a less
general but more composable approach. In much the same way that the
expander level of Racket syntax can be extended through macros, the
reader level of Racket syntax can be composably extended through a
_readtable_.

The Racket reader is a recursive-descent parser, and the readtable maps
characters to parsing handlers. For example, the default readtable maps
`(` to a handler that recursively parses subforms until it finds a `)`.
The `current-readtable` parameter determines the readtable that is used
by `read` or `read-syntax`. Rather than parsing raw characters directly,
a reader extension can install an extended readtable and then chain to
`read` or `read-syntax`.

> +See \[missing\] for an introduction to parameters.

The `make-readtable` function constructs a new readtable as an extension
of an existing one. It accepts a sequence of specifications in terms of
a character, a type of mapping for the character, and \(for certain
types of mappings\) a parsing procedure. For example, to extend the
readtable so that `$` can be used to start and end infix expressions,
implement a `read-dollar` function and use:

```racket
(make-readtable (current-readtable)                
                #\$ 'terminating-macro read-dollar)
```

The protocol for `read-dollar` requires the function to accept different
numbers of arguments depending on whether it is being used in `read` or
`read-syntax` mode. In `read` mode, the parser function is given two
arguments: the character that triggered the parser function and the
input port that is being read. In `read-syntax` mode, the function must
accept four additional arguments that provide the source location of the
character.

The following `"dollar.rkt"` module defines a `read-dollar` function in
terms of the `read` and `read-syntax` functions provided by
`"arith.rkt"`, and it puts `read-dollar` together with new `read` and
`read-syntax` functions that install the readtable and chain to Racket’s
`read` or `read-syntax`:

`"dollar.rkt"`
```racket
#lang racket                                            
(require syntax/readerr                                 
         (prefix-in arith: "arith.rkt"))                
                                                        
(provide (rename-out [$-read read]                      
                     [$-read-syntax read-syntax]))      
                                                        
(define ($-read in)                                     
  (parameterize ([current-readtable (make-$-readtable)])
    (read in)))                                         
                                                        
(define ($-read-syntax src in)                          
  (parameterize ([current-readtable (make-$-readtable)])
    (read-syntax src in)))                              
                                                        
(define (make-$-readtable)                              
  (make-readtable (current-readtable)                   
                  #\$ 'terminating-macro read-dollar))  
                                                        
(define read-dollar                                     
  (case-lambda                                          
   [(ch in)                                             
    (check-$-after (arith:read in) in (object-name in))]
   [(ch in src line col pos)                            
    (check-$-after (arith:read-syntax src in) in src)]))
                                                        
(define (check-$-after val in src)                      
  (regexp-match #px"^\\s*" in) ; skip whitespace        
  (let ([ch (peek-char in)])                            
    (unless (equal? ch #\$) (bad-ending ch src in))     
    (read-char in))                                     
  val)                                                  
                                                        
(define (bad-ending ch src in)                          
  (let-values ([(line col pos) (port-next-location in)])
    ((if (eof-object? ch)                               
         raise-read-error                               
         raise-read-eof-error)                          
     "expected a closing `$'"                           
     src line col pos                                   
     (if (eof-object? ch) 0 1))))                       
```

With this reader extension, a single `#reader` can be used at the
beginning of an expression to enable multiple uses of `$` that switch to
infix arithmetic:

```racket
> #reader"dollar.rkt" (let ([a $1*2+3$] [b $5/6$]) $a+b$)
35/6                                                     
```

## 3. Defining new `#lang` Languages

When loading a module as a source program that starts

`#lang` `language`

the `language` determines the way that the rest of the module is parsed
at the reader level. The reader-level parse must produce a `module` form
as a syntax object. As always, the second sub-form after `module`
specifies the module language that controls the meaning of the module’s
body forms. Thus, a `language` specified after `#lang` controls both the
reader-level and expander-level parsing of a module.

    3.1 Designating a `#lang` Language          
    3.2 Using `#lang reader`                    
    3.3 Using `#lang s-exp syntax/module-reader`
    3.4 Installing a Language                   
    3.5 Source-Handling Configuration           
    3.6 Module-Handling Configuration           

### 3.1. Designating a `#lang` Language

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
one example \(in Using `#lang s-exp`\): the `s-exp` `language` allows a
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

> +Reader Extensions introduces `#reader`.

A consequence of the way that a `#lang` `language` is turned into a
module path is that the language must be installed in a collection,
similar to the way that `"racket"` or `"slideshow"` are collections that
are distributed with Racket. Again, however, there’s an escape from this
restriction: the `reader` language lets you specify a reader-level
implementation of a language using a general module path.

### 3.2. Using `#lang`` ``reader`

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

### 3.3. Using `#lang`` ``s-exp`` ``syntax/module-reader`

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
`"dollar-racket.rkt"` language uses `"dollar.rkt"` \(see Readtables\) to
build a language that is like `racket` but with a `$` escape to simple
infix arithmetic:

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

### 3.4. Installing a Language

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

### 3.5. Source-Handling Configuration

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

### 3.6. Module-Handling Configuration

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
