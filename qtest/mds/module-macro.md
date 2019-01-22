# Modules and Macros

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
