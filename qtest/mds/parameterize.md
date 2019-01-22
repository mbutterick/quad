# Dynamic Binding: `parameterize`

> +\[missing\] in \[missing\] also documents `parameterize`.

The `parameterize` form associates a new value with a _parameter_ during
the evaluation of `body` expressions:

```racket
(parameterize ([parameter-expr value-expr] ...)
  body ...+)                                   
```

> The term “parameter” is sometimes used to refer to the arguments of a
> function, but “parameter” in Racket has the more specific meaning
> described here.

For example, the `error-print-width` parameter controls how many
characters of a value are printed in an error message:

```racket
> (parameterize ([error-print-width 5]) 
    (car (expt 10 1024)))               
car: contract violation                 
  expected: pair?                       
  given: 10...                          
> (parameterize ([error-print-width 10])
    (car (expt 10 1024)))               
car: contract violation                 
  expected: pair?                       
  given: 1000000...                     
```

More generally, parameters implement a kind of dynamic binding. The
`make-parameter` function takes any value and returns a new parameter
that is initialized to the given value. Applying the parameter as a
function returns its current value:

```racket
> (define location (make-parameter "here"))
> (location)                               
"here"                                     
```

In a `parameterize` form, each `parameter-expr` must produce a
parameter. During the evaluation of the `body`s, each specified
parameter is given the result of the corresponding `value-expr`. When
control leaves the `parameterize` form—either through a normal return,
an exception, or some other escape—the parameter reverts to its earlier
value:

```racket
> (parameterize ([location "there"])               
    (location))                                    
"there"                                            
> (location)                                       
"here"                                             
> (parameterize ([location "in a house"])          
    (list (location)                               
          (parameterize ([location "with a mouse"])
            (location))                            
          (location)))                             
'("in a house" "with a mouse" "in a house")        
> (parameterize ([location "in a box"])            
    (car (location)))                              
car: contract violation                            
  expected: pair?                                  
  given: "in a box"                                
> (location)                                       
"here"                                             
```

The `parameterize` form is not a binding form like `let`; each use of
`location` above refers directly to the original definition. A
`parameterize` form adjusts the value of a parameter during the whole
time that the `parameterize` body is evaluated, even for uses of the
parameter that are textually outside of the `parameterize` body:

```racket
> (define (would-you-could-you?)            
    (and (not (equal? (location) "here"))   
         (not (equal? (location) "there"))))
> (would-you-could-you?)                    
#f                                          
> (parameterize ([location "on a bus"])     
    (would-you-could-you?))                 
#t                                          
```

If a use of a parameter is textually inside the body of a `parameterize`
but not evaluated before the `parameterize` form produces a value, then
the use does not see the value installed by the `parameterize` form:

```racket
> (let ([get (parameterize ([location "with a fox"])
               (lambda () (location)))])            
    (get))                                          
"here"                                              
```

The current binding of a parameter can be adjusted imperatively by
calling the parameter as a function with a value. If a `parameterize`
has adjusted the value of the parameter, then directly applying the
parameter procedure affects only the value associated with the active
`parameterize`:

```racket
> (define (try-again! where)             
    (location where))                    
> (location)                             
"here"                                   
> (parameterize ([location "on a train"])
    (list (location)                     
          (begin (try-again! "in a boat")
                 (location))))           
'("on a train" "in a boat")              
> (location)                             
"here"                                   
```

Using `parameterize` is generally preferable to updating a parameter
value imperatively—for much the same reasons that binding a fresh
variable with `let` is preferable to using `set!`  \(see \[missing\]\).

It may seem that variables and `set!` can solve many of the same
problems that parameters solve. For example, `lokation` could be defined
as a string, and `set!` could be used to adjust its value:

```racket
> (define lokation "here")                
> (define (would-ya-could-ya?)            
    (and (not (equal? lokation "here"))   
         (not (equal? lokation "there"))))
> (set! lokation "on a bus")              
> (would-ya-could-ya?)                    
#t                                        
```

Parameters, however, offer several crucial advantages over `set!`:

* The `parameterize` form helps automatically reset the value of a
  parameter when control escapes due to an exception. Adding exception
  handlers and other forms to rewind a `set!` is relatively tedious.

* Parameters work nicely with tail calls \(see \[missing\]\). The last
  `body` in a `parameterize` form is in tail position with respect to
  the `parameterize` form.

* Parameters work properly with threads \(see \[missing\]\). The
  `parameterize` form adjusts the value of a parameter only for
  evaluation in the current thread, which avoids race conditions with
  other threads.
