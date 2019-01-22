# General Phase Levels

A _phase_ can be thought of as a way to separate computations in a
pipeline of processes where one produces code that is used by the next.
\(E.g., a pipeline that consists of a preprocessor process, a compiler,
and an assembler.\)

Imagine starting two Racket processes for this purpose.  If you ignore
inter-process communication channels like sockets and files, the
processes will have no way to share anything other than the text that is
piped from the standard output of one process into the standard input of
the other.  Similarly, Racket effectively allows multiple invocations of
a module to exist in the same process but separated by phase.  Racket
enforces _separation_ of such phases, where different phases cannot
communicate in any way other than via the protocol of macro expansion,
where the output of one phases is the code used in the next.

## 1. Phases and Bindings

Every binding of an identifier exists in a particular phase.  The link
between a binding and its phase is represented by an integer _phase
level_.  Phase level 0 is the phase used for “plain” \(or “runtime”\)
definitions, so

`(define` `age` `5)`

adds a binding for `age` into phase level 0.  The identifier `age` can
be defined at a higher phase level using `begin-for-syntax`:

```racket
(begin-for-syntax
  (define age 5))
```

With a single `begin-for-syntax` wrapper, `age` is defined at phase
level 1.  We can easily mix these two definitions in the same module or
in a top-level namespace, and there is no clash between the two `age`s
that are defined at different phase levels:

```racket
> (define age 3)   
> (begin-for-syntax
    (define age 9))
```

The `age` binding at phase level 0 has a value of 3, and the `age`
binding at phase level 1 has a value of 9.

Syntax objects capture binding information as a first-class value. Thus,

`#'age`

is a syntax object that represents the `age` binding—but since there are
two `age`s \(one at phase level 0 and one at phase level 1\), which one
does it capture?  In fact, Racket imbues `#'age` with lexical
information for all phase levels, so the answer is that `#'age` captures
both.

The relevant binding of `age` captured by `#'age` is determined when
`#'age` is eventually used.  As an example, we bind `#'age` to a pattern
variable so we can use it in a template, and then we `eval`uate the
template: We use `eval` here to demonstrate phases, but see \[missing\]
for caveats about `eval`.

```racket
> (eval (with-syntax ([age #'age])
          #'(displayln age)))     
3                                 
```

The result is `3` because `age` is used at phase 0 level. We can try
again with the use of `age` inside `begin-for-syntax`:

```racket
> (eval (with-syntax ([age #'age])
          #'(begin-for-syntax     
              (displayln age))))  
9                                 
```

In this case, the answer is `9`, because we are using `age` at phase
level 1 instead of 0 \(i.e., `begin-for-syntax` evaluates its
expressions at phase level 1\). So, you can see that we started with the
same syntax object, `#'age`, and we were able to use it in two different
ways: at phase level 0 and at phase level 1.

A syntax object has a lexical context from the moment it first exists. A
syntax object that is provided from a module retains its lexical
context, and so it references bindings in the context of its source
module, not the context of its use.  The following example defines
`button` at phase level 0 and binds it to `0`, while `see-button` binds
the syntax object for `button` in module `a`:

```racket
> (module a racket                                           
    (define button 0)                                        
    (provide (for-syntax see-button))                        
    ; Why not (define see-button #'button)? We explain later.
    (define-for-syntax see-button #'button))                 
> (module b racket                                           
    (require 'a)                                             
    (define button 8)                                        
    (define-syntax (m stx)                                   
      see-button)                                            
    (m))                                                     
> (require 'b)                                               
0                                                            
```

The result of the `m` macro is the value of `see-button`, which is
`#'button` with the lexical context of the `a` module.  Even though
there is another `button` in `b`, the second `button` will not confuse
Racket, because the lexical context of `#'button` \(the value bound to
`see-button`\) is `a`.

Note that `see-button` is bound at phase level 1 by virtue of defining
it with `define-for-syntax`.  Phase level 1 is needed because `m` is a
macro, so its body executes at one phase higher than the context of its
definition.  Since `m` is defined at phase level 0, its body is at phase
level 1, so any bindings referenced by the body must be at phase level
1.

## 2. Phases and Modules

A phase level is a module-relative concept.  When importing from another
module via `require`, Racket lets us shift imported bindings to a phase
level that is different from the original one:

```racket
(require "a.rkt")                ; import with no phase shift
(require (for-syntax "a.rkt"))   ; shift phase by +1         
(require (for-template "a.rkt")) ; shift phase by -1         
(require (for-meta 5 "a.rkt"))   ; shift phase by +5         
```

That is, using `for-syntax` in `require` means that all of the bindings
from that module will have their phase levels increased by one.  A
binding that is `define`d at phase level 0 and imported with
`for-syntax` becomes a phase-level 1 binding:

```racket
> (module c racket                          
    (define x 0) ; defined at phase level 0 
    (provide x))                            
> (module d racket                          
    (require (for-syntax 'c))               
    ; has a binding at phase level 1, not 0:
    #'x)                                    
```

Let’s see what happens if we try to create a binding for the `#'button`
syntax object at phase level 0:

```racket
> (define button 0)           
> (define see-button #'button)
```

Now both `button` and `see-button` are defined at phase 0.  The lexical
context of `#'button` will know that there is a binding for `button` at
phase 0.  In fact, it seems like things are working just fine if we try
to `eval` `see-button`:

```racket
> (eval see-button)
0                  
```

Now, let’s use `see-button` in a macro:

```racket
> (define-syntax (m stx)                             
    see-button)                                      
> (m)                                                
see-button: undefined;                               
 cannot reference an identifier before its definition
  in module: top-level                               
```

Clearly, `see-button` is not defined at phase level 1, so we cannot
refer to it inside the macro body.  Let’s try to use `see-button` in
another module by putting the button definitions in a module and
importing it at phase level 1.  Then, we will get `see-button` at phase
level 1:

```racket
> (module a racket                                              
    (define button 0)                                           
    (define see-button #'button)                                
    (provide see-button))                                       
> (module b racket                                              
    (require (for-syntax 'a)) ; gets see-button at phase level 1
    (define-syntax (m stx)                                      
      see-button)                                               
    (m))                                                        
eval:1:0: button: unbound identifier;                           
 also, no #%top syntax transformer is bound                     
  in: button                                                    
```

Racket says that `button` is unbound now!  When `a` is imported at phase
level 1, we have the following bindings:

```racket
button     at phase level 1
see-button at phase level 1
```

So the macro `m` can see a binding for `see-button` at phase level 1 and
will return the `#'button` syntax object, which refers to `button`
binding at phase level 1.  But the use of `m` is at phase level 0, and
there is no `button` at phase level 0 in `b`.  That is why `see-button`
needs to be bound at phase level 1, as in the original `a`.  In the
original `b`, then, we have the following bindings:

```racket
button     at phase level 0
see-button at phase level 1
```

In this scenario, we can use `see-button` in the macro, since
`see-button` is bound at phase level 1.  When the macro expands, it will
refer to a `button` binding at phase level 0.

Defining `see-button` with `(define see-button #'button)` isn’t
inherently wrong; it depends on how we intend to use `see-button`.  For
example, we can arrange for `m` to sensibly use `see-button` because it
puts it in a phase level 1 context using `begin-for-syntax`:

```racket
> (module a racket                 
    (define button 0)              
    (define see-button #'button)   
    (provide see-button))          
> (module b racket                 
    (require (for-syntax 'a))      
    (define-syntax (m stx)         
      (with-syntax ([x see-button])
        #'(begin-for-syntax        
            (displayln x))))       
    (m))                           
0                                  
```

In this case, module `b` has both `button` and `see-button` bound at
phase level 1.  The expansion of the macro is

```racket
(begin-for-syntax    
  (displayln button))
```

which works, because `button` is bound at phase level 1.

Now, you might try to cheat the phase system by importing `a` at both
phase level 0 and phase level 1.  Then you would have the following
bindings

```racket
button     at phase level 0
see-button at phase level 0
button     at phase level 1
see-button at phase level 1
```

You might expect now that `see-button` in a macro would work, but it
doesn’t:

```racket
> (module a racket                         
    (define button 0)                      
    (define see-button #'button)           
    (provide see-button))                  
> (module b racket                         
    (require 'a                            
             (for-syntax 'a))              
    (define-syntax (m stx)                 
      see-button)                          
    (m))                                   
eval:1:0: button: unbound identifier;      
 also, no #%top syntax transformer is bound
  in: button                               
```

The `see-button` inside macro `m` comes from the `(for-syntax 'a)`
import.  For macro `m` to work, it needs to have `button` bound at phase
0. That binding exists—it’s implied by `(require 'a)`.  However,
`(require 'a)` and `(require (for-syntax 'a))` are _different
instantiations_ of the same module.  The `see-button` at phase 1 only
refers to the `button` at phase 1, not the `button` bound at phase 0
from a different instantiation—even from the same source module.

This kind of phase-level mismatch between instantiations can be repaired
with `syntax-shift-phase-level`. Recall that a syntax object like
`#'button` captures lexical information at _all_ phase levels. The
problem here is that `see-button` is invoked at phase 1, but needs to
return a syntax object that can be evaluated at phase 0. By default,
`see-button` is bound to `#'button` at the same phase level. But with
`syntax-shift-phase-level`, we can make `see-button` refer to `#'button`
at a different relative phase level. In this case, we use a phase shift
of `-1` to make `see-button` at phase 1 refer to `#'button` at phase 0.
\(Because the phase shift happens at every level, it will also make
`see-button` at phase 0 refer to `#'button` at phase -1.\)

Note that `syntax-shift-phase-level` merely creates a reference across
phases. To make that reference work, we still need to instantiate our
module at both phases so the reference and its target have their
bindings available. Thus, in module `'b`, we still import module `'a` at
both phase 0 and phase 1—using `(require 'a (for-syntax 'a))`—so we have
a phase-1 binding for `see-button` and a phase-0 binding for `button`.
Now macro `m` will work.

```racket
> (module a racket                                            
    (define button 0)                                         
    (define see-button (syntax-shift-phase-level #'button -1))
    (provide see-button))                                     
> (module b racket                                            
    (require 'a (for-syntax 'a))                              
    (define-syntax (m stx)                                    
      see-button)                                             
    (m))                                                      
> (require 'b)                                                
0                                                             
```

By the way, what happens to the `see-button` that’s bound at phase 0?
Its `#'button` binding has likewise been shifted, but to phase -1. Since
`button` itself isn’t bound at phase -1, if we try to evaluate
`see-button` at phase 0, we get an error. In other words, we haven’t
permanently cured our mismatch problem—we’ve just shifted it to a less
bothersome location.

```racket
> (module a racket                                            
    (define button 0)                                         
    (define see-button (syntax-shift-phase-level #'button -1))
    (provide see-button))                                     
> (module b racket                                            
    (require 'a (for-syntax 'a))                              
    (define-syntax (m stx)                                    
      see-button)                                             
    (m))                                                      
> (module b2 racket                                           
    (require 'a)                                              
    (eval see-button))                                        
> (require 'b2)                                               
button: undefined;                                            
 cannot reference an identifier before its definition         
  in module: top-level                                        
```

Mismatches like the one above can also arise when a macro tries to match
literal bindings—using `syntax-case` or `syntax-parse`.

```racket
> (module x racket                                 
    (require (for-syntax syntax/parse)             
             (for-template racket/base))           
                                                   
    (provide (all-defined-out))                    
                                                   
    (define button 0)                              
    (define (make) #'button)                       
    (define-syntax (process stx)                   
      (define-literal-set locals (button))         
      (syntax-parse stx                            
        [(_ (n (~literal button))) #'#''ok])))     
> (module y racket                                 
    (require (for-meta 1 'x)                       
             (for-meta 2 'x racket/base))          
                                                   
    (begin-for-syntax                              
      (define-syntax (m stx)                       
        (with-syntax ([out (make)])                
          #'(process (0 out)))))                   
                                                   
    (define-syntax (p stx)                         
      (m))                                         
                                                   
    (p))                                           
eval:2.0: process: expected the identifier `button'
  at: button                                       
  in: (process (0 button))                         
```

In this example, `make` is being used in `y` at phase level 2, and it
returns the `#'button` syntax object—which refers to `button` bound at
phase level 0 inside `x` and at phase level 2 in `y` from `(for-meta 2
'x)`.  The `process` macro is imported at phase level 1 from `(for-meta
1 'x)`, and it knows that `button` should be bound at phase level 1.
When the `syntax-parse` is executed inside `process`, it is looking for
`button` bound at phase level 1 but it sees only a phase level 2 binding
and doesn’t match.

To fix the example, we can provide `make` at phase level 1 relative to
`x`, and then we import it at phase level 1 in `y`:

```racket
> (module x racket                            
    (require (for-syntax syntax/parse)        
             (for-template racket/base))      
                                              
    (provide (all-defined-out))               
                                              
    (define button 0)                         
                                              
    (provide (for-syntax make))               
    (define-for-syntax (make) #'button)       
    (define-syntax (process stx)              
      (define-literal-set locals (button))    
      (syntax-parse stx                       
        [(_ (n (~literal button))) #'#''ok])))
> (module y racket                            
    (require (for-meta 1 'x)                  
             (for-meta 2 racket/base))        
                                              
    (begin-for-syntax                         
      (define-syntax (m stx)                  
        (with-syntax ([out (make)])           
          #'(process (0 out)))))              
                                              
    (define-syntax (p stx)                    
      (m))                                    
                                              
    (p))                                      
> (require 'y)                                
'ok                                           
```
