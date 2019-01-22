# Definitions: `define`

A basic definition has the form

```racket
(define id expr)
```

in which case `id` is bound to the result of `expr`.

Examples:

```racket
(define salutation (list-ref '("Hi" "Hello") (random 2)))
                                                         
> salutation                                             
"Hi"                                                     
```

## 1. Function Shorthand

The `define` form also supports a shorthand for function definitions:

```racket
(define (id arg ...) body ...+)
```

which is a shorthand for

`(define` `id` `(lambda` `(arg` `...)` `body` `...+))`

Examples:

```racket
(define (greet name)                   
  (string-append salutation ", " name))
                                       
> (greet "John")                       
"Hi, John"                             
```

```racket                                                   
(define (greet first [surname "Smith"] #:hi [hi salutation])
  (string-append hi ", " first " " surname))                
```                                                         
                                                            
```racket                                                   
> (greet "John")                                            
"Hi, John Smith"                                            
> (greet "John" #:hi "Hey")                                 
"Hey, John Smith"                                           
> (greet "John" "Doe")                                      
"Hi, John Doe"                                              
```                                                         

The function shorthand via `define` also supports a rest argument
\(i.e., a final argument to collect extra arguments in a list\):

```racket
(define (id arg ... . rest-id) body ...+)
```

which is a shorthand

`(define` `id` `(lambda` `(arg` `...` `. rest-id)` `body` `...+))`

Examples:

```racket
(define (avg . l)            
  (/ (apply + l) (length l)))
                             
> (avg 1 2 3)                
2                            
```

## 2. Curried Function Shorthand

Consider the following `make-add-suffix` function that takes a string
and returns another function that takes a string:

```racket                              
(define make-add-suffix                
  (lambda (s2)                         
    (lambda (s) (string-append s s2))))
```                                    
                                       
                                       

Although it’s not common, result of `make-add-suffix` could be called
directly, like this:

```racket
> ((make-add-suffix "!") "hello")
"hello!"                         
```

In a sense, `make-add-suffix` is a function takes two arguments, but it
takes them one at a time. A function that takes some of its arguments
and returns a function to consume more is sometimes called a _curried
function_.

Using the function-shorthand form of `define`, `make-add-suffix` can be
written equivalently as

```racket
(define (make-add-suffix s2)        
  (lambda (s) (string-append s s2)))
```

This shorthand reflects the shape of the function call `(make-add-suffix
"!")`. The `define` form further supports a shorthand for defining
curried functions that reflects nested function calls:

```racket                        
(define ((make-add-suffix s2) s) 
  (string-append s s2))          
```                              
                                 
```racket                        
> ((make-add-suffix "!") "hello")
"hello!"                         
```                              

```racket                               
(define louder (make-add-suffix "!"))   
(define less-sure (make-add-suffix "?"))
```                                     
                                        
```racket                               
> (less-sure "really")                  
"really?"                               
> (louder "really")                     
"really!"                               
```                                     

The full syntax of the function shorthand for `define` is as follows:

```racket
(define (head args) body ...+)
                              
head = id                     
     | (head args)            
                              
args = arg ...                
     | arg ... . rest-id      
```

The expansion of this shorthand has one nested `lambda` form for each
`head` in the definition, where the innermost `head` corresponds to the
outermost `lambda`.

## 3. Multiple Values and `define-values`

A Racket expression normally produces a single result, but some
expressions can produce multiple results. For example, `quotient` and
`remainder` each produce a single value, but `quotient/remainder`
produces the same two values at once:

```racket
> (quotient 13 3)          
4                          
> (remainder 13 3)         
1                          
> (quotient/remainder 13 3)
4                          
1                          
```

As shown above, the REPL prints each result value on its own line.

Multiple-valued functions can be implemented in terms of the `values`
function, which takes any number of values and returns them as the
results:

```racket
> (values 1 2 3)
1               
2               
3               
```

```racket                                             
(define (split-name name)                             
  (let ([parts (regexp-split " " name)])              
    (if (= (length parts) 2)                          
        (values (list-ref parts 0) (list-ref parts 1))
        (error "not a <first> <last> name"))))        
```                                                   
                                                      
```racket                                             
> (split-name "Adam Smith")                           
"Adam"                                                
"Smith"                                               
```                                                   

The `define-values` form binds multiple identifiers at once to multiple
results produced from a single expression:

```racket
(define-values (id ...) expr)
```

The number of results produced by the `expr` must match the number of
`id`s.

Examples:

```racket
(define-values (given surname) (split-name "Adam Smith"))
                                                         
> given                                                  
"Adam"                                                   
> surname                                                
"Smith"                                                  
```

A `define` form \(that is not a function shorthand\) is equivalent to a
`define-values` form with a single `id`.

> +\[missing\] in \[missing\] provides more on definitions.

## 4. Internal Definitions

When the grammar for a syntactic form specifies `body`, then the
corresponding form can be either a definition or an expression. A
definition as a `body` is an _internal definition_.

Expressions and internal definitions in a `body` sequence can be mixed,
as long as the last `body` is an expression.

For example, the syntax of `lambda` is

```racket
(lambda gen-formals
  body ...+)       
```

so the following are valid instances of the grammar:

```racket
(lambda (f)                ; no definitions 
  (printf "running\n")                      
  (f 0))                                    
                                            
(lambda (f)                ; one definition 
  (define (log-it what)                     
    (printf "~a\n" what))                   
  (log-it "running")                        
  (f 0)                                     
  (log-it "done"))                          
                                            
(lambda (f n)              ; two definitions
  (define (call n)                          
    (if (zero? n)                           
        (log-it "done")                     
        (begin                              
          (log-it "running")                
          (f n)                             
          (call (- n 1)))))                 
  (define (log-it what)                     
    (printf "~a\n" what))                   
  (call n))                                 
```

Internal definitions in a particular `body` sequence are mutually
recursive; that is, any definition can refer to any other definition—as
long as the reference isn’t actually evaluated before its definition
takes place. If a definition is referenced too early, an error occurs.

Examples:

```racket
(define (weird)                  
  (define x x)                   
  x)                             
                                 
> (weird)                        
x: undefined;                    
 cannot use before initialization
```

A sequence of internal definitions using just `define` is easily
translated to an equivalent `letrec` form \(as introduced in the next
section\). However, other definition forms can appear as a `body`,
including `define-values`, `struct` \(see \[missing\]\) or
`define-syntax` \(see \[missing\]\).

> +\[missing\] in \[missing\] documents the fine points of internal
> definitions.
