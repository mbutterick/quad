# Named `let`

A named `let` is an iteration and recursion form. It uses the same
syntactic keyword `let` as for local binding, but an identifier after
the `let` \(instead of an immediate open parenthesis\) triggers a
different parsing.

```racket
(let proc-id ([arg-id init-expr] ...)
  body ...+)                         
```

A named `let` form is equivalent to

```racket
(letrec ([proc-id (lambda (arg-id ...)
                     body ...+)])     
  (proc-id init-expr ...))            
```

That is, a named `let` binds a function identifier that is visible only
in the function’s body, and it implicitly calls the function with the
values of some initial expressions.

Examples:

```racket
(define (duplicate pos lst)                             
  (let dup ([i 0]                                       
            [lst lst])                                  
   (cond                                                
    [(= i pos) (cons (car lst) lst)]                    
    [else (cons (car lst) (dup (+ i 1) (cdr lst)))])))  
                                                        
> (duplicate 1 (list "apple" "cheese burger!" "banana"))
'("apple" "cheese burger!" "cheese burger!" "banana")   
```
