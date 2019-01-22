# Reflection and Dynamic Evaluation

Racket is a _dynamic_ language. It offers numerous facilities for
loading, compiling, and even constructing new code at run time.

    1 `eval`                                     
      1.1 Local Scopes                           
      1.2 Namespaces                             
      1.3 Namespaces and Modules                 
                                                 
    2 Manipulating Namespaces                    
      2.1 Creating and Installing Namespaces     
      2.2 Sharing Data and Code Across Namespaces
                                                 
    3 Scripting Evaluation and Using `load`      

## 1. `eval`

> This example will not work within a module or in DrRacket’s definitions
> window, but it will work in the interactions window, for reasons that
> are explained by the end of Namespaces.

The `eval` function takes a representation of an expression or
definition \(as a “quoted” form or syntax object\) and evaluates it:

```racket
> (eval '(+ 1 2))
3                
```

The power of `eval` is that an expression can be constructed
dynamically:

```racket
> (define (eval-formula formula)
    (eval `(let ([x 2]          
                 [y 3])         
             ,formula)))        
> (eval-formula '(+ x y))       
5                               
> (eval-formula '(+ (* x y) y)) 
9                               
```

Of course, if we just wanted to evaluate expressions with given values
for `x` and `y`, we do not need `eval`. A more direct approach is to use
first-class functions:

```racket
> (define (apply-formula formula-proc)        
    (formula-proc 2 3))                       
> (apply-formula (lambda (x y) (+ x y)))      
5                                             
> (apply-formula (lambda (x y) (+ (* x y) y)))
9                                             
```

However, if expressions like `(+ x y)` and `(+ (* x y) y)` are read from
a file supplied by a user, for example, then `eval` might be
appropriate. Similarly, the REPL reads expressions that are typed by a
user and uses `eval` to evaluate them.

Also, `eval` is often used directly or indirectly on whole modules. For
example, a program might load a module on demand using
`dynamic-require`, which is essentially a wrapper around `eval` to
dynamically load the module code.

### 1.1. Local Scopes

The `eval` function cannot see local bindings in the context where it is
called. For example, calling `eval` inside an unquoted `let` form to
evaluate a formula does not make values visible for `x` and `y`:

```racket
> (define (broken-eval-formula formula)              
    (let ([x 2]                                      
          [y 3])                                     
      (eval formula)))                               
> (broken-eval-formula '(+ x y))                     
x: undefined;                                        
 cannot reference an identifier before its definition
  in module: top-level                               
```

The `eval` function cannot see the `x` and `y` bindings precisely
because it is a function, and Racket is a lexically scoped language.
Imagine if `eval` were implemented as

```racket
(define (eval x)                   
  (eval-expanded (macro-expand x)))
```

then at the point when `eval-expanded` is called, the most recent
binding of `x` is to the expression to evaluate, not the `let` binding
in `broken-eval-formula`. Lexical scope prevents such confusing and
fragile behavior, and consequently prevents `eval` from seeing local
bindings in the context where it is called.

You might imagine that even though `eval` cannot see the local bindings
in `broken-eval-formula`, there must actually be a data structure
mapping `x` to `2` and `y` to `3`, and you would like a way to get that
data structure. In fact, no such data structure exists; the compiler is
free to replace every use of `x` with `2` at compile time, so that the
local binding of `x` does not exist in any concrete sense at run-time.
Even when variables cannot be eliminated by constant-folding, normally
the names of the variables can be eliminated, and the data structures
that hold local values do not resemble a mapping from names to values.

### 1.2. Namespaces

Since `eval` cannot see the bindings from the context where it is
called, another mechanism is needed to determine dynamically available
bindings. A _namespace_ is a first-class value that encapsulates the
bindings available for dynamic evaluation.

> Informally, the term _namespace_ is sometimes used interchangeably with
> _environment_ or _scope_. In Racket, the term _namespace_ has the more
> specific, dynamic meaning given above, and it should not be confused
> with static lexical concepts.

Some functions, such as `eval`, accept an optional namespace argument.
More often, the namespace used by a dynamic operation is the _current
namespace_ as determined by the `current-namespace` parameter.

When `eval` is used in a REPL, the current namespace is the one that the
REPL uses for evaluating expressions. That’s why the following
interaction successfully accesses `x` via `eval`:

```racket
> (define x 3)
> (eval 'x)   
3             
```

In contrast, try the following simple module and running it directly in
DrRacket or supplying the file as a command-line argument to `racket`:

```racket
#lang racket      
                  
(eval '(cons 1 2))
```

This fails because the initial current namespace is empty. When you run
`racket` in interactive mode \(see \[missing\]\), the initial namespace
is initialized with the exports of the `racket` module, but when you run
a module directly, the initial namespace starts empty.

In general, it’s a bad idea to use `eval` with whatever namespace
happens to be installed. Instead, create a namespace explicitly and
install it for the call to eval:

```racket
#lang racket                     
                                 
(define ns (make-base-namespace))
(eval '(cons 1 2) ns) ; works    
```

The `make-base-namespace` function creates a namespace that is
initialized with the exports of `racket/base`. The later section
Manipulating Namespaces provides more information on creating and
configuring namespaces.

### 1.3. Namespaces and Modules

As with `let` bindings, lexical scope means that `eval` cannot
automatically see the definitions of a `module` in which it is called.
Unlike `let` bindings, however, Racket provides a way to reflect a
module into a namespace.

The `module->namespace` function takes a quoted module path and produces
a namespace for evaluating expressions and definitions as if they
appeared in the `module` body:

```racket
> (module m racket/base              
    (define x 11))                   
> (require 'm)                       
> (define ns (module->namespace ''m))
> (eval 'x ns)                       
11                                   
```

> The double quoting in `''m` is because `'m` is a module path that refers
> to an interactively declared module, and so `''m` is the quoted form of
> the path.

The `module->namespace` function is mostly useful from outside a module,
where the module’s full name is known. Inside a `module` form, however,
the full name of a module may not be known, because it may depend on
where the module source is located when it is eventually loaded.

From within a `module`, use `define-namespace-anchor` to declare a
reflection hook on the module, and use `namespace-anchor->namespace` to
reel in the module’s namespace:

```racket
#lang racket                               
                                           
(define-namespace-anchor a)                
(define ns (namespace-anchor->namespace a))
                                           
(define x 1)                               
(define y 2)                               
                                           
(eval '(cons x y) ns) ; produces (1 . 2)   
```

## 2. Manipulating Namespaces

A namespace encapsulates two pieces of information:

* A mapping from identifiers to bindings. For example, a namespace might
  map the identifier `lambda` to the `lambda` form. An “empty” namespace
  is one that maps every identifier to an uninitialized top-level
  variable.

* A mapping from module names to module declarations and instances.
  \(The distinction between declaration and instance is discussed in
  \[missing\].\)

The first mapping is used for evaluating expressions in a top-level
context, as in `(eval '(lambda (x) (+ x 1)))`. The second mapping is
used, for example, by `dynamic-require` to locate a module. The call
`(eval '(require racket/base))` normally uses both pieces: the
identifier mapping determines the binding of `require`; if it turns out
to mean `require`, then the module mapping is used to locate the
`racket/base` module.

From the perspective of the core Racket run-time system, all evaluation
is reflective. Execution starts with an initial namespace that contains
a few primitive modules, and that is further populated by loading files
and modules as specified on the command line or as supplied in the REPL.
Top-level `require` and `define` forms adjusts the identifier mapping,
and module declarations \(typically loaded on demand for a `require`
form\) adjust the module mapping.

### 2.1. Creating and Installing Namespaces

The function `make-empty-namespace` creates a new, empty namespace.
Since the namespace is truly empty, it cannot at first be used to
evaluate any top-level expression—not even `(require racket)`. In
particular,

```racket
(parameterize ([current-namespace (make-empty-namespace)])
  (namespace-require 'racket))                            
```

fails, because the namespace does not include the primitive modules on
which `racket` is built.

To make a namespace useful, some modules must be _attached_ from an
existing namespace. Attaching a module adjusts the mapping of module
names to instances by transitively copying entries \(the module and all
its imports\) from an existing namespace’s mapping. Normally, instead of
just attaching the primitive modules—whose names and organization are
subject to change—a higher-level module is attached, such as `racket` or
`racket/base`.

The `make-base-empty-namespace` function provides a namespace that is
empty, except that `racket/base` is attached. The resulting namespace is
still “empty” in the sense that the identifiers-to-bindings part of the
namespace has no mappings; only the module mapping has been populated.
Nevertheless, with an initial module mapping, further modules can be
loaded.

A namespace created with `make-base-empty-namespace` is suitable for
many basic dynamic tasks. For example, suppose that a `my-dsl` library
implements a domain-specific language in which you want to execute
commands from a user-specified file. A namespace created with
`make-base-empty-namespace` is enough to get started:

```racket
(define (run-dsl file)                                           
  (parameterize ([current-namespace (make-base-empty-namespace)])
    (namespace-require 'my-dsl)                                  
    (load file)))                                                
```

Note that the `parameterize` of `current-namespace` does not affect the
meaning of identifiers like `namespace-require` within the
`parameterize` body. Those identifiers obtain their meaning from the
enclosing context \(probably a module\). Only expressions that are
dynamic with respect to this code, such as the content of `load`ed
files, are affected by the `parameterize`.

Another subtle point in the above example is the use of
`(namespace-require 'my-dsl)` instead of `(eval '(require my-dsl))`. The
latter would not work, because `eval` needs to obtain a meaning for
`require` in the namespace, and the namespace’s identifier mapping is
initially empty. The `namespace-require` function, in contrast, directly
imports the given module into the current namespace.  Starting with
`(namespace-require 'racket/base)` would introduce a binding for
`require` and make a subsequent `(eval '(require my-dsl))` work. The
above is better, not only because it is more compact, but also because
it avoids introducing bindings that are not part of the domain-specific
languages.

### 2.2. Sharing Data and Code Across Namespaces

Modules not attached to a new namespace will be loaded and instantiated
afresh if they are demanded by evaluation. For example, `racket/base`
does not include `racket/class`, and loading `racket/class` again will
create a distinct class datatype:

```racket
> (require racket/class)                                          
> (class? object%)                                                
#t                                                                
> (class?                                                         
   (parameterize ([current-namespace (make-base-empty-namespace)])
     (namespace-require 'racket/class) ; loads again              
     (eval 'object%)))                                            
#f                                                                
```

For cases when dynamically loaded code needs to share more code and data
with its context, use the `namespace-attach-module` function. The first
argument to `namespace-attach-module` is a source namespace from which
to draw a module instance; in some cases, the current namespace is known
to include the module that needs to be shared:

```racket
> (require racket/class)                                
> (class?                                               
   (let ([ns (make-base-empty-namespace)])              
     (namespace-attach-module (current-namespace)       
                              'racket/class             
                              ns)                       
     (parameterize ([current-namespace ns])             
       (namespace-require 'racket/class) ; uses attached
       (eval 'object%))))                               
#t                                                      
```

Within a module, however, the combination of `define-namespace-anchor`
and `namespace-anchor->empty-namespace` offers a more reliable method
for obtaining a source namespace:

```racket
#lang racket/base                                                 
                                                                  
(require racket/class)                                            
                                                                  
(define-namespace-anchor a)                                       
                                                                  
(define (load-plug-in file)                                       
  (let ([ns (make-base-empty-namespace)])                         
    (namespace-attach-module (namespace-anchor->empty-namespace a)
                             'racket/class                        
                              ns)                                 
    (parameterize ([current-namespace ns])                        
      (dynamic-require file 'plug-in%))))                         
```

The anchor bound by `namespace-attach-module` connects the run time of a
module with the namespace in which a module is loaded \(which might
differ from the current namespace\).  In the above example, since the
enclosing module requires `racket/class`, the namespace produced by
`namespace-anchor->empty-namespace` certainly contains an instance of
`racket/class`. Moreover, that instance is the same as the one imported
into the module, so the class datatype is shared.

## 3. Scripting Evaluation and Using `load`

Historically, Lisp implementations did not offer module systems.
Instead, large programs were built by essentially scripting the REPL to
evaluate program fragments in a particular order. While REPL scripting
turns out to be a bad way to structure programs and libraries, it is
still sometimes a useful capability.

> Describing a program via `load` interacts especially badly with
> macro-defined language extensions \[Flatt02\].

The `load` function runs a REPL script by `read`ing S-expressions from a
file, one by one, and passing them to `eval`. If a file `"place.rkts"`
contains

```racket
(define city "Salt Lake City")
(define state "Utah")         
(printf "~a, ~a\n" city state)
```

then it can be loaded in a REPL:

```racket
> (load "place.rkts")
Salt Lake City, Utah 
> city               
"Salt Lake City"     
```

Since `load` uses `eval`, however, a module like the following generally
will not work—for the same reasons described in Namespaces:

```racket
#lang racket           
                       
(define there "Utopia")
                       
(load "here.rkts")     
```

The current namespace for evaluating the content of `"here.rkts"` is
likely to be empty; in any case, you cannot get `there` from
`"here.rkts"`. Also, any definitions in `"here.rkts"` will not become
visible for use within the module; after all, the `load` happens
dynamically, while references to identifiers within the module are
resolved lexically, and therefore statically.

Unlike `eval`, `load` does not accept a namespace argument. To supply a
namespace to `load`, set the `current-namespace` parameter. The
following example evaluates the expressions in `"here.rkts"` using the
bindings of the `racket/base` module:

```racket
#lang racket                                             
                                                         
(parameterize ([current-namespace (make-base-namespace)])
  (load "here.rkts"))                                    
```

You can even use `namespace-anchor->namespace` to make the bindings of
the enclosing module accessible for dynamic evaluation. In the following
example, when `"here.rkts"` is `load`ed, it can refer to `there` as well
as the bindings of `racket`:

```racket
#lang racket                                                       
                                                                   
(define there "Utopia")                                            
                                                                   
(define-namespace-anchor a)                                        
(parameterize ([current-namespace (namespace-anchor->namespace a)])
  (load "here.rkts"))                                              
```

Still, if `"here.rkts"` defines any identifiers, the definitions cannot
be directly \(i.e., statically\) referenced by in the enclosing module.

The `racket/load` module language is different from `racket` or
`racket/base`. A module using `racket/load` treats all of its content as
dynamic, passing each form in the module body to `eval` \(using a
namespace that is initialized with `racket`\). As a result, uses of
`eval` and `load` in the module body see the same dynamic namespace as
immediate body forms. For example, if `"here.rkts"` contains

```racket
(define here "Morporkia")       
(define (go!) (set! here there))
```

then running

```racket
#lang racket/load      
                       
(define there "Utopia")
                       
(load "here.rkts")     
                       
(go!)                  
(printf "~a\n" here)   
```

prints “Utopia”.

Drawbacks of using `racket/load` include reduced error checking, tool
support, and performance. For example, with the program

```racket
#lang racket/load   
                    
(define good 5)     
(printf "running\n")
good                
bad                 
```

DrRacket’s Check Syntax tool cannot tell that the second `good` is a
reference to the first, and the unbound reference to `bad` is reported
only at run time instead of rejected syntactically.
