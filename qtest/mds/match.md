# Pattern Matching

The `match` form supports pattern matching on arbitrary Racket values,
as opposed to functions like `regexp-match` that compare regular
expressions to byte and character sequences \(see \[missing\]\).

```racket
(match target-expr        
  [pattern expr ...+] ...)
```

The `match` form takes the result of `target-expr` and tries to match
each `pattern` in order. As soon as it finds a match, it evaluates the
corresponding `expr` sequence to obtain the result for the `match` form.
If `pattern` includes _pattern variables_, they are treated like
wildcards, and each variable is bound in the `expr` to the input
fragments that it matched.

Most Racket literal expressions can be used as patterns:

```racket
> (match 2           
    [1 'one]         
    [2 'two]         
    [3 'three])      
'two                 
> (match #f          
    [#t 'yes]        
    [#f 'no])        
'no                  
> (match "apple"     
    ['apple 'symbol] 
    ["apple" 'string]
    [#f 'boolean])   
'string              
```

Constructors like `cons`, `list`, and `vector` can be used to create
patterns that match pairs, lists, and vectors:

```racket
> (match '(1 2)            
    [(list 0 1) 'one]      
    [(list 1 2) 'two])     
'two                       
> (match '(1 . 2)          
    [(list 1 2) 'list]     
    [(cons 1 2) 'pair])    
'pair                      
> (match #(1 2)            
    [(list 1 2) 'list]     
    [(vector 1 2) 'vector])
'vector                    
```

A constructor bound with `struct` also can be used as a pattern
constructor:

```racket
> (struct shoe (size color))  
> (struct hat (size style))   
> (match (hat 23 'bowler)     
   [(shoe 10 'white) "bottom"]
   [(hat 23 'bowler) "top"])  
"top"                         
```

Unquoted, non-constructor identifiers in a pattern are pattern variables
that are bound in the result expressions, except `_`, which does not
bind \(and thus is usually used as a catch-all\):

```racket
> (match '(1)                      
    [(list x) (+ x 1)]             
    [(list x y) (+ x y)])          
2                                  
> (match '(1 2)                    
    [(list x) (+ x 1)]             
    [(list x y) (+ x y)])          
3                                  
> (match (hat 23 'bowler)          
    [(shoe sz col) sz]             
    [(hat sz stl) sz])             
23                                 
> (match (hat 11 'cowboy)          
    [(shoe sz 'black) 'a-good-shoe]
    [(hat sz 'bowler) 'a-good-hat] 
    [_ 'something-else])           
'something-else                    
```

An ellipsis, written `...`, acts like a Kleene star within a list or
vector pattern: the preceding sub-pattern can be used to match any
number of times for any number of consecutive elements of the list or
vector. If a sub-pattern followed by an ellipsis includes a pattern
variable, the variable matches multiple times, and it is bound in the
result expression to a list of matches:

```racket
> (match '(1 1 1)                                  
    [(list 1 ...) 'ones]                           
    [_ 'other])                                    
'ones                                              
> (match '(1 1 2)                                  
    [(list 1 ...) 'ones]                           
    [_ 'other])                                    
'other                                             
> (match '(1 2 3 4)                                
    [(list 1 x ... 4) x])                          
'(2 3)                                             
> (match (list (hat 23 'bowler) (hat 22 'pork-pie))
    [(list (hat sz styl) ...) (apply + sz)])       
45                                                 
```

Ellipses can be nested to match nested repetitions, and in that case,
pattern variables can be bound to lists of lists of matches:

```racket
> (match '((! 1) (! 2 2) (! 3 3 3))
    [(list (list '! x ...) ...) x])
'((1) (2 2) (3 3 3))               
```

The `quasiquote` form  \(see \[missing\] for more about it\) can also be
used to build patterns. While unquoted portions of a normal quasiquoted
form mean regular racket evaluation, here unquoted portions mean go back
to regular pattern matching.

So, in the example below, the with expression is the pattern and it gets
rewritten into the application expression, using quasiquote as a pattern
in the first instance and quasiquote to build an expression in the
second.

```racket
> (match `{with {x 1} {+ x 1}}     
    [`{with {,id ,rhs} ,body}      
     `{{lambda {,id} ,body} ,rhs}])
'((lambda (x) (+ x 1)) 1)          
```

For information on many more pattern forms, see `racket/match`.

Forms like `match-let` and `match-lambda` support patterns in positions
that otherwise must be identifiers. For example, `match-let` generalizes
`let` to a destructing bind:

```racket
> (match-let ([(list x y z) '(1 2 3)])
    (list z y x))                     
'(3 2 1)                              
```

For information on these additional forms, see `racket/match`.

> +\[missing\] in \[missing\] provides more on pattern matching.
