# Quoting: `quote` and `'`

> +\[missing\] in \[missing\] also documents `quote`.

The `quote` form produces a constant:

```racket
(quote datum)
```

The syntax of a `datum` is technically specified as anything that the
`read` function parses as a single element. The value of the `quote`
form is the same value that `read` would produce given `datum`.

The `datum` can be a symbol, a boolean, a number, a \(character or
byte\) string, a character, a keyword, an empty list, a pair \(or list\)
containing more such values, a vector containing more such values, a
hash table containing more such values, or a box containing another such
value.

Examples:

```racket
> (quote apple)                       
'apple                                
> (quote #t)                          
#t                                    
> (quote 42)                          
42                                    
> (quote "hello")                     
"hello"                               
> (quote ())                          
'()                                   
> (quote ((1 2 3) #("z" x) . the-end))
'((1 2 3) #("z" x) . the-end)         
> (quote (1 2 . (3)))                 
'(1 2 3)                              
```

As the last example above shows, the `datum` does not have to match the
normalized printed form of a value. A `datum` cannot be a printed
representation that starts with `#<`, so it cannot be `#<void>`,
`#<undefined>`, or a procedure.

The `quote` form is rarely used for a `datum` that is a boolean, number,
or string by itself, since the printed forms of those values can already
be used as constants. The `quote` form is more typically used for
symbols and lists, which have other meanings \(identifiers, function
calls, etc.\) when not quoted.

An expression

```racket
'datum
```

is a shorthand for

`(quote` `datum)`

and this shorthand is almost always used instead of `quote`. The
shorthand applies even within the `datum`, so it can produce a list
containing `quote`.

> +\[missing\] in \[missing\] provides more on the `'` shorthand.

Examples:

```racket
> 'apple                  
'apple                    
> '"hello"                
"hello"                   
> '(1 2 3)                
'(1 2 3)                  
> (display '(you can 'me))
(you can (quote me))      
```
