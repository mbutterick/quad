# Module Instantiations and Visits

Modules often contain just function and structure-type definitions, in
which case the module itself behaves in a purely functional way, and the
time when the functions are created is not observable. If a module’s
top-level expressions include side effects, however, then the timing of
the effects can matter. The distinction between module declaration and
instantiation provides some control over that timing. The concept of
module visits further explains the interaction of effects with macro
implementations.

## 1. Declaration versus Instantiation

Declaring a module does not immediately evaluate expressions in the
module’s body. For example, evaluating

```racket
> (module number-n racket/base
    (provide n)               
    (define n (random 10))    
    (printf "picked ~a\n" n)) 
```

declares the module `number-n`, but it doesn’t immediately pick a random
number for `n` or display the number. A `require` of `number-n` causes
the module to be _instantiated_ \(i.e., it triggers an
_instantiation_\), which implies that the expressions in the body of the
module are evaluated:

```racket
> (require 'number-n)
picked 5             
> n                  
5                    
```

After a module is instantiated in a particular namespace, further
`require`s of the module use the same instance, as opposed to
instantiating the module again:

```racket
> (require 'number-n)       
> n                         
5                           
> (module use-n racket/base 
    (require 'number-n)     
    (printf "still ~a\n" n))
> (require 'use-n)          
still 5                     
```

The `dynamic-require` function, like `require`, triggers instantion of a
module if it is not already instantiated, so `dynamic-require` with `#f`
as a second argument is useful to just trigger the instantion effects of
a module:

```racket
> (module use-n-again racket/base   
    (require 'number-n)             
    (printf "also still ~a\n" n))   
> (dynamic-require ''use-n-again #f)
also still 5                        
```

Instantiation of modules by `require` is transitive. That is, if
`require` of a module instantiates it, then any module `require`d by
that one is also instantiated \(if it’s not instantiated already\):

```racket
> (module number-m racket/base
    (provide m)               
    (define m (random 10))    
    (printf "picked ~a\n" m)) 
> (module use-m racket/base   
    (require 'number-m)       
    (printf "still ~a\n" m))  
> (require 'use-m)            
picked 0                      
still 0                       
```

## 2. Compile-Time Instantiation

In the same way that declaring a module does not by itself instantiate a
module, declaring a module that `require`s another module does not by
itself instantiate the `require`d module, as illustrated in the
preceding example. However, declaring a module _does_ expand and compile
the module. If a module imports another with `(require (for-syntax
....))`, then module that is imported `for-syntax` must be instantiated
during expansion:

```racket
> (module number-p racket/base               
    (provide p)                              
    (define p (random 10))                   
    (printf "picked ~a\n" p))                
> (module use-p-at-compile-time racket/base  
    (require (for-syntax racket/base         
                         'number-p))         
    (define-syntax (pm stx)                  
      #`#,p)                                 
    (printf "was ~a at compile time\n" (pm)))
picked 1                                     
```

Unlike run-time instantiation in a namespace, when a module is used
`for-syntax` for another module expansion in the same namespace, the
`for-syntax`ed module is instantiated separately for each expansion.
Continuing the previous example, if `number-p` is used a second time
`for-syntax`, then a second random number is selected for a new `p`:

```racket
> (module use-p-again-at-compile-time racket/base   
    (require (for-syntax racket/base                
                         'number-p))                
    (define-syntax (pm stx)                         
      #`#,p)                                        
    (printf "was ~a at second compile time\n" (pm)))
picked 3                                            
```

Separate compile-time instantiations of `number-p` helps prevent
accidental propagation of effects from one module’s compilation to
another module’s compilation. Preventing those effects make compilation
reliably separate and more deterministic.

The expanded forms of `use-p-at-compile-time` and
`use-p-again-at-compile-time` record the number that was selected each
time, so those two different numbers are printed when the modules are
instantiated:

```racket
> (dynamic-require ''use-p-at-compile-time #f)      
was 1 at compile time                               
> (dynamic-require ''use-p-again-at-compile-time #f)
was 3 at second compile time                        
```

A namespace’s top level behaves like a separate module, where multiple
interactions in the top level conceptually extend a single expansion of
the module. So, when using `(require (for-syntax ....))` twice in the
top level, the second use does not trigger a new compile-time instance:

```racket
> (begin (require (for-syntax 'number-p)) 'done)      
picked 4                                              
'done                                                 
> (begin (require (for-syntax 'number-p)) 'done-again)
'done-again                                           
```

However, a run-time instance of a module is kept separate from all
compile-time instances, including at the top level, so a
non-`for-syntax` use of `number-p` will pick another random number:

```racket
> (require 'number-p)
picked 5             
```

## 3. Visiting Modules

When a module `provide`s a macro for use by other modules, the other
modules use the macro by directly `require`ing the macro provider—i.e.,
without `for-syntax`. That’s because the macro is being imported for use
in a run-time position \(even though the macro’s implementation lives at
compile time\), while `for-syntax` would import a binding for use in
compile-time position.

The module implementing a macro, meanwhile, might `require` another
module `for-syntax` to implement the macro. The `for-syntax` module
needs a compile-time instantiation during any module expansion that
might use the macro. That requirement sets up a kind of transitivity
through `require` that is similar to instantiation transitivity, but
“off by one” at the point where the `for-syntax` shift occurs in the
chain.

Here’s an example to make that scenario concrete:

```racket
> (module number-q racket/base                      
    (provide q)                                     
    (define q (random 10))                          
    (printf "picked ~a\n" q))                       
> (module use-q-at-compile-time racket/base         
    (require (for-syntax racket/base                
                         'number-q))                
    (provide qm)                                    
    (define-syntax (qm stx)                         
      #`#,q)                                        
    (printf "was ~a at compile time\n" (qm)))       
picked 7                                            
> (module use-qm racket/base                        
    (require 'use-q-at-compile-time)                
    (printf "was ~a at second compile time\n" (qm)))
picked 4                                            
> (dynamic-require ''use-qm #f)                     
was 7 at compile time                               
was 4 at second compile time                        
```

In this example, when `use-q-at-compile-time` is expanded and compiled,
`number-q` is instantiated once. In this case, that instantion is needed
to expand the `(qm)` macro, but the module system would proactively
create a compile-time instantiation of `number-q` even if the `qm` macro
turned out not to be used.

Then, as `use-qm` is expanded and compiled, a second compile-time
instantiation of `number-q` is created. That compile-time instantion is
needed to expand the `(qm)` form within `use-qm`.

Instantiating `use-qm` correctly reports the number that was picked
during that second module’s compilation. First, though, the `require` of
`use-q-at-compile-time` in `use-qm` triggers a transitive instantiation
of `use-q-at-compile-time`, which correctly reports the number that was
picked in its compilation.

Overall, the example illustrates a transitive effect of `require` that
we had already seen:

* When a module is instantiated, the run-time expressions      in its
  body are evaluated.

* When a module is instantiated, then any module that it `require`s
  \(without `for-syntax`\) is also instantiated.

This rule does not explain the compile-time instantiations of
`number-q`, however. To explain that, we need a new word, _visit_, for
the concept that we saw in Compile-Time Instantiation:

* When a module is visited, the compile-time expressions      \(such as
  macro definition\) in its body are evaluated.

* As a module is expanded, it is visited.

* When a module is visited, then any module that it `require`s
  \(without `for-syntax`\) is also visited.

* When a module is visited, then any module that it `require`s
  `for-syntax` is instantiated at compile time.

Note that when visiting one module causes a compile-time instantion of
another module, the transitiveness of instantiated through regular
`require`s can trigger more compile-time instantiations. Instantiation
itself won’t trigger further visits, however, because any instantiated
module has already been expanded and compiled.

The compile-time expressions of a module that are evaluated by visiting
include both the right-hand sides of `define-syntax` forms and the body
of `begin-for-syntax` forms. That’s why a randomly selected number is
printed immediately in the following example:

```racket
> (module compile-time-number racket/base
    (require (for-syntax racket/base))   
    (begin-for-syntax                    
      (printf "picked ~a\n" (random)))   
    (printf "running\n"))                
picked 0.25549265186825576               
```

Instantiating the module evaluates only the run-time expressions, which
prints “running” but not a new random number:

```racket
> (dynamic-require ''compile-time-number #f)
running                                     
```

The description of instantiates and visit above is phrased in terms of
normal `require`s and `for-syntax` `require`s, but a more precise
specification is in terms of module phases. For example, if module `A`
has `(require (for-syntax B))` and module `B` has `(require
(for-template C))`, then module `C` is instantiated when module `A` is
instantiated, because the `for-syntax` and `for-template` shifts cancel.
We have not yet specified what happens with `for-meta 2` for when
`for-syntax`es combine; we leave that to the next section, Lazy Visits
via Available Modules.

If you think of the top-level as a kind of module that is continuously
expanded, the above rules imply that `require` of another module at the
top level both instantiates and visits the other module \(if it is not
already instantiated and visited\). That’s roughly true, but the visit
is made lazy in a way that is also explained in the next section, Lazy
Visits via Available Modules.

Meanwhile, `dynamic-require` only instantiates a module; it does not
visit the module. That simplification is why some of the preceding
examples use `dynamic-require` instead of `require`. The extra visits of
a top-level `require` would make the earlier examples less clear.

## 4. Lazy Visits via Available Modules

A top-level `require` of a module does not actually visit the module.
Instead, it makes the module _available_. An available module will be
visited when a future expression needs to be expanded in the same
context. The next expression may or may not involve some imported macro
that needs its compile-time helpers evaluated by visiting, but the
module system proactively visits the module, just in case.

In the following example, a random number is picked as a result of
visiting a module’s own body while that module is being expanded. A
`require` of the module instantiates it, printing “running”, and also
makes the module available. Evaluating any other expression implies
expanding the expression, and that expansion triggers a visit of the
available module—which picks another random number:

```racket
> (module another-compile-time-number racket/base
    (require (for-syntax racket/base))           
    (begin-for-syntax                            
      (printf "picked ~a\n" (random)))           
    (printf "running\n"))                        
picked 0.3634379786893492                        
> (require 'another-compile-time-number)         
running                                          
> 'next                                          
picked 0.5057086679589476                        
'next                                            
> 'another                                       
'another                                         
```

> Beware that the expander flattens the content of a top-level `begin`
> into the top level as soon as the `begin` is discovered. So, `(begin
> (require 'another-compile-time-number) 'next)` would still have printed
> “picked” before “next“.

The final evaluation of `'another` also visits any available modules,
but no modules were made newly available by simply evaluating `'next`.

When a module `require`s another module using `for-meta n` for some `n`
greater than 1, the `require`d module is made available at phase `n`. A
module that is available at phase `n` is visited when some expression at
phase `n`_-_1__ is expanded.

To help illustrate, the following examples use
`(variable-reference->module-base-phase (#%variable-reference))`, which
returns a number for the phase at which the enclosing module is
instantiated:

```racket
> (module show-phase racket/base                                            
    (printf "running at ~a\n"                                               
            (variable-reference->module-base-phase (#%variable-reference))))
> (require 'show-phase)                                                     
running at 0                                                                
> (module use-at-phase-1 racket/base                                        
    (require (for-syntax 'show-phase)))                                     
running at 1                                                                
> (module unused-at-phase-2 racket/base                                     
    (require (for-meta 2 'show-phase)))                                     
```

For the last module above, `show-phase` is made available at phase 2,
but no expressions within the module are ever expanded at phase 1, so
there’s no phase-2 printout. The following module includes a phase-1
expression after the phase-2 `require`, so there’s a printout:

```racket
> (module use-at-phase-2 racket/base  
    (require (for-meta 2 'show-phase) 
             (for-syntax racket/base))
    (define-syntax x 'ok))            
running at 2                          
```

If we `require` the module `use-at-phase-1` at the top level, then
`show-phase` is made available at phase 1. Evaluating another expression
causes `use-at-phase-1` to be visited, which in turn instantitates
`show-phase`:

```racket
> (require 'use-at-phase-1)
> 'next                    
running at 1               
'next                      
```

A `require` of `use-at-phase-2` is similar, except that `show-phase` is
made available at phase 2, so it is not instantiated until some
expression is expanded at phase 1:

```racket
> (require 'use-at-phase-2)            
> 'next                                
'next                                  
> (require (for-syntax racket/base))   
> (begin-for-syntax 'compile-time-next)
running at 2                           
```
