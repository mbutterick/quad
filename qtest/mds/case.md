# Simple Dispatch: `case`

The `case` form dispatches to a clause by matching the result of an
expression to the values for the clause:

```racket
(case expr                
  [(datum ...+) body ...+]
  ...)                    
```

Each `datum` will be compared to the result of `expr` using `equal?`,
and then the corresponding `body`s are evaluated. The `case` form can
dispatch to the correct clause in _O_\(_log N_\)__ time for _N_
`datum`s.

Multiple `datum`s can be supplied for each clause, and the corresponding
`body`s are evaluated if any of the `datum`s match.

Example:

```racket
> (let ([v (random 6)])
    (printf "~a\n" v)  
    (case v            
      [(0) 'zero]      
      [(1) 'one]       
      [(2) 'two]       
      [(3 4 5) 'many]))
0                      
'zero                  
```

The last clause of a `case` form can use `else`, just like `cond`:

Example:

```racket
> (case (random 6)
    [(0) 'zero]   
    [(1) 'one]    
    [(2) 'two]    
    [else 'many]) 
'many             
```

For more general pattern matching \(but without the dispatch-time
guarantee\), use `match`, which is introduced in \[missing\].
