# Function Calls \(Procedure Applications\)

An expression of the form

```racket
(proc-expr arg-expr ...)
```

is a function call—also known as a _procedure application_—when
`proc-expr` is not an identifier that is bound as a syntax transformer
\(such as `if` or `define`\).

## 1. Evaluation Order and Arity

A function call is evaluated by first evaluating the `proc-expr` and all
`arg-expr`s in order \(left to right\). Then, if `proc-expr` produces a
function that accepts as many arguments as supplied `arg-expr`s, the
function is called. Otherwise, an exception is raised.

Examples:

```racket
> (cons 1 null)                                           
'(1)                                                      
> (+ 1 2 3)                                               
6                                                         
> (cons 1 2 3)                                            
cons: arity mismatch;                                     
 the expected number of arguments does not match the given
number                                                    
  expected: 2                                             
  given: 3                                                
  arguments...:                                           
   1                                                      
   2                                                      
   3                                                      
> (1 2 3)                                                 
application: not a procedure;                             
 expected a procedure that can be applied to arguments    
  given: 1                                                
  arguments...:                                           
   2                                                      
   3                                                      
```

Some functions, such as `cons`, accept a fixed number of arguments. Some
functions, such as `+` or `list`, accept any number of arguments. Some
functions accept a range of argument counts; for example `substring`
accepts either two or three arguments. A function’s _arity_ is the
number of arguments that it accepts.

## 2. Keyword Arguments

Some functions accept _keyword arguments_ in addition to by-position
arguments. For that case, an `arg` can be an `arg-keyword arg-expr`
sequence instead of just a `arg-expr`:

> +\[missing\] introduces keywords.

```racket
(proc-expr arg ...)       
                          
arg = arg-expr            
    | arg-keyword arg-expr
```

For example,

`(go` `"super.rkt"` `#:mode` `'fast)`

calls the function bound to `go` with `"super.rkt"` as a by-position
argument, and with `'fast` as an argument associated with the `#:mode`
keyword. A keyword is implicitly paired with the expression that follows
it.

Since a keyword by itself is not an expression, then

`(go` `"super.rkt"` `#:mode` `#:fast)`

is a syntax error. The `#:mode` keyword must be followed by an
expression to produce an argument value, and `#:fast` is not an
expression.

The order of keyword `arg`s determines the order in which `arg-expr`s
are evaluated, but a function accepts keyword arguments independent of
their position in the argument list. The above call to `go` can be
equivalently written

`(go` `#:mode` `'fast` `"super.rkt")`

> +\[missing\] in \[missing\] provides more on procedure applications.

## 3. The `apply` Function

The syntax for function calls supports any number of arguments, but a
specific call always specifies a fixed number of arguments. As a result,
a function that takes a list of arguments cannot directly apply a
function like `+` to all of the items in a list:

```racket                          
(define (avg lst) ; doesn’t work...
  (/ (+ lst) (length lst)))        
```                                
                                   
```racket                          
> (avg '(1 2 3))                   
+: contract violation              
  expected: number?                
  given: '(1 2 3)                  
```                                

```racket                                                  
(define (avg lst) ; doesn’t always work...                 
  (/ (+ (list-ref lst 0) (list-ref lst 1) (list-ref lst 2))
     (length lst)))                                        
```                                                        
                                                           
```racket                                                  
> (avg '(1 2 3))                                           
2                                                          
> (avg '(1 2))                                             
list-ref: index too large for list                         
  index: 2                                                 
  in: '(1 2)                                               
```                                                        

The `apply` function offers a way around this restriction. It takes a
function and a _list_ argument, and it applies the function to the
values in the list:

```racket                        
(define (avg lst)                
  (/ (apply + lst) (length lst)))
```                              
                                 
```racket                        
> (avg '(1 2 3))                 
2                                
> (avg '(1 2))                   
3/2                              
> (avg '(1 2 3 4))               
5/2                              
```                              

As a convenience, the `apply` function accepts additional arguments
between the function and the list. The additional arguments are
effectively `cons`ed onto the argument list:

```racket             
(define (anti-sum lst)
  (apply - 0 lst))    
```                   
                      
```racket             
> (anti-sum '(1 2 3)) 
-6                    
```                   

The `apply` function accepts keyword arguments, too, and it passes them
along to the called function:

```racket
(apply go #:mode 'fast '("super.rkt"))
(apply go '("super.rkt") #:mode 'fast)
```

Keywords that are included in `apply`’s list argument do not count as
keyword arguments for the called function; instead, all arguments in
this list are treated as by-position arguments. To pass a list of
keyword arguments to a function, use the `keyword-apply` function, which
accepts a function to apply and three lists. The first two lists are in
parallel, where the first list contains keywords \(sorted by
`keyword<?`\), and the second list contains a corresponding argument for
each keyword. The third list contains by-position function arguments, as
for `apply`.

```racket
(keyword-apply go             
               '(#:mode)      
               '(fast)        
               '("super.rkt"))
```
