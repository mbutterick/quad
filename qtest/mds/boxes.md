# Boxes

A _box_ is like a single-element vector. It can print as a quoted `#&`
followed by the printed form of the boxed value. A `#&` form can also be
used as an expression, but since the resulting box is constant, it has
practically no use.

Examples:

```racket
> (define b (box "apple"))   
> b                          
'#&"apple"                   
> (unbox b)                  
"apple"                      
> (set-box! b '(banana boat))
> b                          
'#&(banana boat)             
```

> +\[missing\] in \[missing\] provides more on boxes and box procedures.
