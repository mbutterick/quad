# Imports: `require`

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
in the previous section, \[missing\]\). In this case, the bindings
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
