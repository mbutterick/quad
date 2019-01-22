# Assignment and Redefinition

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
