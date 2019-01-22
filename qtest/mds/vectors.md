# Vectors

A _vector_ is a fixed-length array of arbitrary values. Unlike a list, a
vector supports constant-time access and update of its elements.

A vector prints similar to a list—as a parenthesized sequence of its
elements—but a vector is prefixed with `#` after `'`, or it uses
`vector` if one of its elements cannot be expressed with `quote`.

For a vector as an expression, an optional length can be supplied. Also,
a vector as an expression implicitly `quote`s the forms for its content,
which means that identifiers and parenthesized forms in a vector
constant represent symbols and lists.

> +\[missing\] in \[missing\] documents the fine points of the syntax of
> vectors.

Examples:

```racket
> #("a" "b" "c")                    
'#("a" "b" "c")                     
> #(name (that tune))               
'#(name (that tune))                
> #4(baldwin bruce)                 
'#(baldwin bruce bruce bruce)       
> (vector-ref #("a" "b" "c") 1)     
"b"                                 
> (vector-ref #(name (that tune)) 1)
'(that tune)                        
```

Like strings, a vector is either mutable or immutable, and vectors
written directly as expressions are immutable.

Vectors can be converted to lists and vice versa via `vector->list` and
`list->vector`; such conversions are particularly useful in combination
with predefined procedures on lists. When allocating extra lists seems
too expensive, consider using looping forms like `for/fold`, which
recognize vectors as well as lists.

Example:

```racket
> (list->vector (map string-titlecase                          
                     (vector->list #("three" "blind" "mice"))))
'#("Three" "Blind" "Mice")                                     
```

> +\[missing\] in \[missing\] provides more on vectors and vector
> procedures.
