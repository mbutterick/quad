# Functions \(Procedures\): `lambda`

A `lambda` expression creates a function. In the simplest case, a
`lambda` expression has the form

```racket
(lambda (arg-id ...)
  body ...+)        
```

A `lambda` form with _n_ `arg-id`s accepts _n_ arguments:

```racket
> ((lambda (x) x)                                         
   1)                                                     
1                                                         
> ((lambda (x y) (+ x y))                                 
   1 2)                                                   
3                                                         
> ((lambda (x y) (+ x y))                                 
   1)                                                     
#<procedure>: arity mismatch;                             
 the expected number of arguments does not match the given
number                                                    
  expected: 2                                             
  given: 1                                                
  arguments...:                                           
   1                                                      
```

## 1. Declaring a Rest Argument

A `lambda` expression can also have the form

```racket
(lambda rest-id
  body ...+)   
```

That is, a `lambda` expression can have a single `rest-id` that is not
surrounded by parentheses. The resulting function accepts any number of
arguments, and the arguments are put into a list bound to `rest-id`.

Examples:

```racket
> ((lambda x x)      
   1 2 3)            
'(1 2 3)             
> ((lambda x x))     
'()                  
> ((lambda x (car x))
   1 2 3)            
1                    
```

Functions with a `rest-id` often use `apply` to call another function
that accepts any number of arguments.

> +\[missing\] describes `apply`.

Examples:

```racket
(define max-mag                       
  (lambda nums                        
    (apply max (map magnitude nums))))
                                      
> (max 1 -2 0)                        
1                                     
> (max-mag 1 -2 0)                    
2                                     
```

The `lambda` form also supports required arguments combined with a
`rest-id`:

```racket
(lambda (arg-id ...+ . rest-id)
  body ...+)                   
```

The result of this form is a function that requires at least as many
arguments as `arg-id`s, and also accepts any number of additional
arguments.

Examples:

```racket
(define max-mag                                           
  (lambda (num . nums)                                    
    (apply max (map magnitude (cons num nums)))))         
                                                          
> (max-mag 1 -2 0)                                        
2                                                         
> (max-mag)                                               
max-mag: arity mismatch;                                  
 the expected number of arguments does not match the given
number                                                    
  expected: at least 1                                    
  given: 0                                                
```

A `rest-id` variable is sometimes called a _rest argument_, because it
accepts the “rest” of the function arguments.

## 2. Declaring Optional Arguments

Instead of just an identifier, an argument \(other than a rest
argument\) in a `lambda` form can be specified with an identifier and a
default value:

```racket
(lambda gen-formals                
  body ...+)                       
                                   
gen-formals = (arg ...)            
            | rest-id              
            | (arg ...+ . rest-id) 
                                   
arg         = arg-id               
            | [arg-id default-expr]
```

An argument of the form `[arg-id default-expr]` is optional. When the
argument is not supplied in an application, `default-expr` produces the
default value. The `default-expr` can refer to any preceding `arg-id`,
and every following `arg-id` must have a default as well.

Examples:

```racket
(define greet                                    
  (lambda (given [surname "Smith"])              
    (string-append "Hello, " given " " surname)))
                                                 
> (greet "John")                                 
"Hello, John Smith"                              
> (greet "John" "Doe")                           
"Hello, John Doe"                                
```

```racket                                          
(define greet                                      
  (lambda (given [surname (if (equal? given "John")
                              "Doe"                
                              "Smith")])           
    (string-append "Hello, " given " " surname)))  
```                                                
                                                   
```racket                                          
> (greet "John")                                   
"Hello, John Doe"                                  
> (greet "Adam")                                   
"Hello, Adam Smith"                                
```                                                

## 3. Declaring Keyword Arguments

A `lambda` form can declare an argument to be passed by keyword, instead
of position. Keyword arguments can be mixed with by-position arguments,
and default-value expressions can be supplied for either kind of
argument:

> +\[missing\] introduces function calls with keywords.

```racket
(lambda gen-formals                            
  body ...+)                                   
                                               
gen-formals = (arg ...)                        
            | rest-id                          
            | (arg ...+ . rest-id)             
                                               
arg         = arg-id                           
            | [arg-id default-expr]            
            | arg-keyword arg-id               
            | arg-keyword [arg-id default-expr]
```

An argument specified as `arg-keyword arg-id` is supplied by an
application using the same `arg-keyword`.  The position of the
keyword–identifier pair in the argument list does not matter for
matching with arguments in an application, because it will be matched to
an argument value by keyword instead of by position.

```racket                                        
(define greet                                    
  (lambda (given #:last surname)                 
    (string-append "Hello, " given " " surname)))
```                                              
                                                 
```racket                                        
> (greet "John" #:last "Smith")                  
"Hello, John Smith"                              
> (greet #:last "Doe" "John")                    
"Hello, John Doe"                                
```                                              

An `arg-keyword [arg-id default-expr]` argument specifies a
keyword-based argument with a default value.

Examples:

```racket
(define greet                                               
  (lambda (#:hi [hi "Hello"] given #:last [surname "Smith"])
    (string-append hi ", " given " " surname)))             
                                                            
> (greet "John")                                            
"Hello, John Smith"                                         
> (greet "Karl" #:last "Marx")                              
"Hello, Karl Marx"                                          
> (greet "John" #:hi "Howdy")                               
"Howdy, John Smith"                                         
> (greet "Karl" #:last "Marx" #:hi "Guten Tag")             
"Guten Tag, Karl Marx"                                      
```

The `lambda` form does not directly support the creation of a function
that accepts “rest” keywords. To construct a function that accepts all
keyword arguments, use `make-keyword-procedure`. The function supplied
to `make-keyword-procedure` receives keyword arguments through parallel
lists in the first two \(by-position\) arguments, and then all
by-position arguments from an application as the remaining by-position
arguments.

> +\[missing\] introduces `keyword-apply`.

Examples:

```racket
(define (trace-wrap f)                                 
  (make-keyword-procedure                              
   (lambda (kws kw-args . rest)                        
     (printf "Called with ~s ~s ~s\n" kws kw-args rest)
     (keyword-apply f kws kw-args rest))))             
                                                       
> ((trace-wrap greet) "John" #:hi "Howdy")             
Called with (#:hi) ("Howdy") ("John")                  
"Howdy, John Smith"                                    
```

> +\[missing\] in \[missing\] provides more on function expressions.

## 4. Arity-Sensitive Functions: `case-lambda`

The `case-lambda` form creates a function that can have completely
different behaviors depending on the number of arguments that are
supplied. A case-lambda expression has the form

```racket
(case-lambda                     
  [formals body ...+]            
  ...)                           
                                 
formals = (arg-id ...)           
        | rest-id                
        | (arg-id ...+ . rest-id)
```

where each `[formals body ...+]` is analogous to `(lambda formals body
...+)`. Applying a function produced by `case-lambda` is like applying a
`lambda` for the first case that matches the number of given arguments.

Examples:

```racket
(define greet                                                      
  (case-lambda                                                     
    [(name) (string-append "Hello, " name)]                        
    [(given surname) (string-append "Hello, " given " " surname)]))
                                                                   
> (greet "John")                                                   
"Hello, John"                                                      
> (greet "John" "Smith")                                           
"Hello, John Smith"                                                
> (greet)                                                          
greet: arity mismatch;                                             
 the expected number of arguments does not match the given         
number                                                             
  given: 0                                                         
```

A `case-lambda` function cannot directly support optional or keyword
arguments.
