# Identifiers and Binding

The context of an expression determines the meaning of identifiers that
appear in the expression. In particular, starting a module with the
language `racket`, as in

`#lang` `racket`

means that, within the module, the identifiers described in this guide
start with the meaning described here: `cons` refers to the function
that creates a pair, `car` refers to the function that extracts the
first element of a pair, and so on.

> +\[missing\] introduces the syntax of identifiers.

Forms like `define`, `lambda`, and `let` associate a meaning with one or
more identifiers; that is, they _bind_ identifiers. The part of the
program for which the binding applies is the _scope_ of the binding. The
set of bindings in effect for a given expression is the expression’s
_environment_.

For example, in

```racket
#lang racket    
                
(define f       
  (lambda (x)   
    (let ([y 5])
      (+ x y))))
                
(f 10)          
```

the `define` is a binding of `f`, the `lambda` has a binding for `x`,
and the `let` has a binding for `y`. The scope of the binding for `f` is
the entire module; the scope of the `x` binding is `(let ([y 5]) (+ x
y))`; and the scope of the `y` binding is just `(+ x y)`. The
environment of `(+ x y)` includes bindings for `y`, `x`, and `f`, as
well as everything in `racket`.

A module-level `define` can bind only identifiers that are not already
defined or `require`d into the module. A local `define` or other binding
forms, however, can give a new local binding for an identifier that
already has a binding; such a binding _shadows_ the existing binding.

Examples:

```racket
(define f                                    
  (lambda (append)                           
    (define cons (append "ugly" "confusing"))
    (let ([append 'this-was])                
      (list append cons))))                  
                                             
> (f list)                                   
'(this-was ("ugly" "confusing"))             
```

Similarly, a module-level `define` can shadow a binding from the
module’s language. For example, `(define cons 1)` in a `racket` module
shadows the `cons` that is provided by `racket`. Intentionally shadowing
a language binding is rarely a good idea—especially for widely used
bindings like `cons`—but shadowing relieves a programmer from having to
avoid every obscure binding that is provided by a language.

Even identifiers like `define` and `lambda` get their meanings from
bindings, though they have _transformer_ bindings \(which means that
they indicate syntactic forms\) instead of value bindings. Since
`define` has a transformer binding, the identifier `define` cannot be
used by itself to get a value. However, the normal binding for `define`
can be shadowed.

Examples:

```racket
> define                    
eval:1:0: define: bad syntax
  in: define                
> (let ([define 5]) define) 
5                           
```

Again, shadowing standard bindings in this way is rarely a good idea,
but the possibility is an inherent part of Racket’s flexibility.
