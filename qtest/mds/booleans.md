# Booleans

Racket has two distinguished constants to represent boolean values: `#t`
for true and `#f` for false. Uppercase `#T` and `#F` are parsed as the
same values, but the lowercase forms are preferred.

The `boolean?` procedure recognizes the two boolean constants. In the
result of a test expression for `if`, `cond`, `and`, `or`, etc.,
however, any value other than `#f` counts as true.

Examples:

```racket
> (= 2 (+ 1 1))  
#t               
> (boolean? #t)  
#t               
> (boolean? #f)  
#t               
> (boolean? "no")
#f               
> (if "no" 1 0)  
1                
```
