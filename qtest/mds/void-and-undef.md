# Void and Undefined

Some procedures or expression forms have no need for a result value. For
example, the `display` procedure is called only for the side-effect of
writing output. In such cases the result value is normally a special
constant that prints as `#<void>`.  When the result of an expression is
simply `#<void>`, the REPL does not print anything.

The `void` procedure takes any number of arguments and returns
`#<void>`. \(That is, the identifier `void` is bound to a procedure that
returns `#<void>`, instead of being bound directly to `#<void>`.\)

Examples:

```racket
> (void)       
> (void 1 2 3) 
> (list (void))
'(#<void>)     
```

The `undefined` constant, which prints as `#<undefined>`, is sometimes
used as the result of a reference whose value is not yet available. In
previous versions of Racket \(before version 6.1\), referencing a local
binding too early produced `#<undefined>`; too-early references now
raise an exception, instead.

> The `undefined` result can still be produced in some cases by the
> `shared` form.

```racket                        
(define (fails)                  
  (define x x)                   
  x)                             
```                              
                                 
```racket                        
> (fails)                        
x: undefined;                    
 cannot use before initialization
```                              
