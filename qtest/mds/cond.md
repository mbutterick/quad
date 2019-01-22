# Conditionals

Most functions used for branching, such as `<` and `string?`, produce
either `#t` or `#f`. Racket’s branching forms, however, treat any value
other than `#f` as true. We say a _true value_ to mean any value other
than `#f`.

This convention for “true value” meshes well with protocols where `#f`
can serve as failure or to indicate that an optional value is not
supplied. \(Beware of overusing this trick, and remember that an
exception is usually a better mechanism to report failure.\)

For example, the `member` function serves double duty; it can be used to
find the tail of a list that starts with a particular item, or it can be
used to simply check whether an item is present in a list:

```racket
> (member "Groucho" '("Harpo" "Zeppo"))              
#f                                                   
> (member "Groucho" '("Harpo" "Groucho" "Zeppo"))    
'("Groucho" "Zeppo")                                 
> (if (member "Groucho" '("Harpo" "Zeppo"))          
      'yep                                           
      'nope)                                         
'nope                                                
> (if (member "Groucho" '("Harpo" "Groucho" "Zeppo"))
      'yep                                           
      'nope)                                         
'yep                                                 
```

## 1. Simple Branching: `if`

> +\[missing\] in \[missing\] also documents `if`.

In an `if` form,

```racket
(if test-expr then-expr else-expr)
```

the `test-expr` is always evaluated. If it produces any value other than
`#f`, then `then-expr` is evaluated. Otherwise, `else-expr` is
evaluated.

An `if` form must have both a `then-expr` and an `else-expr`; the latter
is not optional. To perform \(or skip\) side-effects based on a
`test-expr`, use `when` or `unless`, which we describe later in
\[missing\].

## 2. Combining Tests: `and` and `or`

> +\[missing\] in \[missing\] also documents `and` and `or`.

Racket’s `and` and `or` are syntactic forms, rather than functions.
Unlike a function, the `and` and `or` forms can skip evaluation of later
expressions if an earlier one determines the answer.

```racket
(and expr ...)
```

An `and` form produces `#f` if any of its `expr`s produces `#f`.
Otherwise, it produces the value of its last `expr`. As a special case,
`(and)` produces `#t`.

```racket
(or expr ...)
```

The `or` form produces `#f` if all of its `expr`s produce `#f`.
Otherwise, it produces the first non-`#f` value from its `expr`s.  As a
special case, `(or)` produces `#f`.

Examples:

```racket
> (define (got-milk? lst)                                    
    (and (not (null? lst))                                   
         (or (eq? 'milk (car lst))                           
             (got-milk? (cdr lst))))) ; recurs only if needed
> (got-milk? '(apple banana))                                
#f                                                           
> (got-milk? '(apple milk banana))                           
#t                                                           
```

If evaluation reaches the last `expr` of an `and` or `or` form, then the
`expr`’s value directly determines the `and` or `or` result. Therefore,
the last `expr` is in tail position, which means that the above
`got-milk?` function runs in constant space.

> +\[missing\] introduces tail calls and tail positions.

## 3. Chaining Tests: `cond`

The `cond` form chains a series of tests to select a result expression.
To a first approximation, the syntax of `cond` is as follows:

> +\[missing\] in \[missing\] also documents `cond`.

```racket
(cond [test-expr body ...+]
      ...)                 
```

Each `test-expr` is evaluated in order. If it produces `#f`, the
corresponding `body`s are ignored, and evaluation proceeds to the next
`test-expr`. As soon as a `test-expr` produces a true value, its `body`s
are evaluated to produce the result for the `cond` form, and no further
`test-expr`s are evaluated.

The last `test-expr` in a `cond` can be replaced by `else`. In terms of
evaluation, `else` serves as a synonym for `#t`, but it clarifies that
the last clause is meant to catch all remaining cases. If `else` is not
used, then it is possible that no `test-expr`s produce a true value; in
that case, the result of the `cond` expression is `#<void>`.

Examples:

```racket
> (cond                       
   [(= 2 3) (error "wrong!")] 
   [(= 2 2) 'ok])             
'ok                           
> (cond                       
   [(= 2 3) (error "wrong!")])
> (cond                       
   [(= 2 3) (error "wrong!")] 
   [else 'ok])                
'ok                           
```

```racket                         
(define (got-milk? lst)           
  (cond                           
    [(null? lst) #f]              
    [(eq? 'milk (car lst)) #t]    
    [else (got-milk? (cdr lst))]))
```                               
                                  
```racket                         
> (got-milk? '(apple banana))     
#f                                
> (got-milk? '(apple milk banana))
#t                                
```                               

The full syntax of `cond` includes two more kinds of clauses:

```racket
(cond cond-clause ...)                  
                                        
cond-clause = [test-expr then-body ...+]
            | [else then-body ...+]     
            | [test-expr => proc-expr]  
            | [test-expr]               
```

The `=>` variant captures the true result of its `test-expr` and passes
it to the result of the `proc-expr`, which must be a function of one
argument.

Examples:

```racket
> (define (after-groucho lst)                 
    (cond                                     
      [(member "Groucho" lst) => cdr]         
      [else (error "not there")]))            
> (after-groucho '("Harpo" "Groucho" "Zeppo"))
'("Zeppo")                                    
> (after-groucho '("Harpo" "Zeppo"))          
not there                                     
```

A clause that includes only a `test-expr` is rarely used. It captures
the true result of the `test-expr`, and simply returns the result for
the whole `cond` expression.
