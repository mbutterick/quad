# Quasiquoting: `quasiquote` and `‘`

> +\[missing\] in \[missing\] also documents `quasiquote`.

The `quasiquote` form is similar to `quote`:

```racket
(quasiquote datum)
```

However, for each `(unquote expr)` that appears within the `datum`, the
`expr` is evaluated to produce a value that takes the place of the
`unquote` sub-form.

Example:

```racket
> (quasiquote (1 2 (unquote (+ 1 2)) (unquote (- 5 1))))
'(1 2 3 4)                                              
```

This form can be used to write functions that build lists according to
certain patterns.

Examples:

```racket
> (define (deep n)                                           
    (cond                                                    
      [(zero? n) 0]                                          
      [else                                                  
       (quasiquote ((unquote n) (unquote (deep (- n 1)))))]))
> (deep 8)                                                   
'(8 (7 (6 (5 (4 (3 (2 (1 0))))))))                           
```

Or even to cheaply construct expressions programmatically. \(Of course,
9 times out of 10, you should be using a macro to do this \(the 10th
time being when you’re working through a textbook like
[PLAI](http://www.cs.brown.edu/~sk/Publications/Books/ProgLangs/)\).\)

Examples:

```racket
> (define (build-exp n)                                       
    (add-lets n (make-sum n)))                                
> (define (add-lets n body)                                   
    (cond                                                     
      [(zero? n) body]                                        
      [else                                                   
       (quasiquote                                            
        (let ([(unquote (n->var n)) (unquote n)])             
          (unquote (add-lets (- n 1) body))))]))              
> (define (make-sum n)                                        
    (cond                                                     
      [(= n 1) (n->var 1)]                                    
      [else                                                   
       (quasiquote (+ (unquote (n->var n))                    
                      (unquote (make-sum (- n 1)))))]))       
> (define (n->var n) (string->symbol (format "x~a" n)))       
> (build-exp 3)                                               
'(let ((x3 3)) (let ((x2 2)) (let ((x1 1)) (+ x3 (+ x2 x1)))))
```

The `unquote-splicing` form is similar to `unquote`, but its `expr` must
produce a list, and the `unquote-splicing` form must appear in a context
that produces either a list or a vector. As the name suggests, the
resulting list is spliced into the context of its use.

Example:

```racket
> (quasiquote (1 2 (unquote-splicing (list (+ 1 2) (- 5 1))) 5))
'(1 2 3 4 5)                                                    
```

Using splicing we can revise the construction of our example expressions
above to have just a single `let` expression and a single `+`
expression.

Examples:

```racket
> (define (build-exp n)                                          
    (add-lets                                                    
     n                                                           
     (quasiquote (+ (unquote-splicing                            
                     (build-list                                 
                      n                                          
                      (λ (x) (n->var (+ x 1)))))))))             
> (define (add-lets n body)                                      
    (quasiquote                                                  
     (let (unquote                                               
           (build-list                                           
            n                                                    
            (λ (n)                                               
              (quasiquote                                        
               [(unquote (n->var (+ n 1))) (unquote (+ n 1))]))))
       (unquote body))))                                         
> (define (n->var n) (string->symbol (format "x~a" n)))          
> (build-exp 3)                                                  
'(let ((x1 1) (x2 2) (x3 3)) (+ x1 x2 x3))                       
```

If a `quasiquote` form appears within an enclosing `quasiquote` form,
then the inner `quasiquote` effectively cancels one layer of `unquote`
and `unquote-splicing` forms, so that a second `unquote` or
`unquote-splicing` is needed.

Examples:

```racket
> (quasiquote (1 2 (quasiquote (unquote (+ 1 2)))))                            
'(1 2 (quasiquote (unquote (+ 1 2))))                                          
> (quasiquote (1 2 (quasiquote (unquote (unquote (+ 1 2))))))                  
'(1 2 (quasiquote (unquote 3)))                                                
>                                                                              
(quasiquote (1 2 (quasiquote ((unquote (+ 1 2)) (unquote (unquote (- 5 1)))))))
'(1 2 (quasiquote ((unquote (+ 1 2)) (unquote 4))))                            
```

The evaluations above will not actually print as shown. Instead, the
shorthand form of `quasiquote` and `unquote` will be used: ` \(i.e., a
backquote\) and `,` \(i.e., a comma\). The same shorthands can be used
in expressions:

Example:

```racket
> `(1 2 `(,(+ 1 2) ,,(- 5 1)))
'(1 2 `(,(+ 1 2) ,4))         
```

The shorthand form of `unquote-splicing` is `,@`:

Example:

```racket
> `(1 2 ,@(list (+ 1 2) (- 5 1)))
'(1 2 3 4)                       
```
