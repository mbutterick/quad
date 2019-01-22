# Pairs, Lists, and Racket Syntax

The `cons` function actually accepts any two values, not just a list for
the second argument. When the second argument is not `empty` and not
itself produced by `cons`, the result prints in a special way. The two
values joined with `cons` are printed between parentheses, but with a
dot \(i.e., a period surrounded by whitespace\) in between:

```racket
> (cons 1 2)             
'(1 . 2)                 
> (cons "banana" "split")
'("banana" . "split")    
```

Thus, a value produced by `cons` is not always a list. In general, the
result of `cons` is a _pair_. The more traditional name for the `cons?`
function is `pair?`, and we’ll use the traditional name from now on.

The name `rest` also makes less sense for non-list pairs; the more
traditional names for `first` and `rest` are `car` and `cdr`,
respectively. \(Granted, the traditional names are also nonsense. Just
remember that “a” comes before “d,” and `cdr` is pronounced
“could-er.”\)

Examples:

```racket
> (car (cons 1 2))    
1                     
> (cdr (cons 1 2))    
2                     
> (pair? empty)       
#f                    
> (pair? (cons 1 2))  
#t                    
> (pair? (list 1 2 3))
#t                    
```

Racket’s pair datatype and its relation to lists is essentially a
historical curiosity, along with the dot notation for printing and the
funny names `car` and `cdr`. Pairs are deeply wired into to the culture,
specification, and implementation of Racket, however, so they survive in
the language.

You are perhaps most likely to encounter a non-list pair when making a
mistake, such as accidentally reversing the arguments to `cons`:

```racket
> (cons (list 2 3) 1)
'((2 3) . 1)         
> (cons 1 (list 2 3))
'(1 2 3)             
```

Non-list pairs are used intentionally, sometimes. For example, the
`make-hash` function takes a list of pairs, where the `car` of each pair
is a key and the `cdr` is an arbitrary value.

The only thing more confusing to new Racketeers than non-list pairs is
the printing convention for pairs where the second element _is_ a pair,
but _is not_ a list:

```racket
> (cons 0 (cons 1 2))
'(0 1 . 2)           
```

In general, the rule for printing a pair is as follows: use the dot
notation unless the dot is immediately followed by an open parenthesis.
In that case, remove the dot, the open parenthesis, and the matching
close parenthesis. Thus, `'(0 . (1 . 2))` becomes `'(0 1 . 2)`, and `'(1
. (2 . (3 . ())))` becomes `'(1 2 3)`.

## 1. Quoting Pairs and Symbols with `quote`

A list prints with a quote mark before it, but if an element of a list
is itself a list, then no quote mark is printed for the inner list:

```racket
> (list (list 1) (list 2 3) (list 4))
'((1) (2 3) (4))                     
```

For nested lists, especially, the `quote` form lets you write a list as
an expression in essentially the same way that the list prints:

```racket
> (quote ("red" "green" "blue"))
'("red" "green" "blue")         
> (quote ((1) (2 3) (4)))       
'((1) (2 3) (4))                
> (quote ())                    
'()                             
```

The `quote` form works with the dot notation, too, whether the quoted
form is normalized by the dot-parenthesis elimination rule or not:

```racket
> (quote (1 . 2))      
'(1 . 2)               
> (quote (0 . (1 . 2)))
'(0 1 . 2)             
```

Naturally, lists of any kind can be nested:

```racket
> (list (list 1 2 3) 5 (list "a" "b" "c"))
'((1 2 3) 5 ("a" "b" "c"))                
> (quote ((1 2 3) 5 ("a" "b" "c")))       
'((1 2 3) 5 ("a" "b" "c"))                
```

If you wrap an identifier with `quote`, then you get output that looks
like an identifier, but with a `'` prefix:

```racket
> (quote jane-doe)
'jane-doe         
```

A value that prints like a quoted identifier is a _symbol_. In the same
way that parenthesized output should not be confused with expressions, a
printed symbol should not be confused with an identifier. In particular,
the symbol `(quote map)` has nothing to do with the `map` identifier or
the predefined function that is bound to `map`, except that the symbol
and the identifier happen to be made up of the same letters.

Indeed, the intrinsic value of a symbol is nothing more than its
character content. In this sense, symbols and strings are almost the
same thing, and the main difference is how they print. The functions
`symbol->string` and `string->symbol` convert between them.

Examples:

```racket
> map                         
#<procedure:map>              
> (quote map)                 
'map                          
> (symbol? (quote map))       
#t                            
> (symbol? map)               
#f                            
> (procedure? map)            
#t                            
> (string->symbol "map")      
'map                          
> (symbol->string (quote map))
"map"                         
```

In the same way that `quote` for a list automatically applies itself to
nested lists, `quote` on a parenthesized sequence of identifiers
automatically applies itself to the identifiers to create a list of
symbols:

```racket
> (car (quote (road map)))          
'road                               
> (symbol? (car (quote (road map))))
#t                                  
```

When a symbol is inside a list that is printed with `'`, the `'` on the
symbol is omitted, since `'` is doing the job already:

```racket
> (quote (road map))
'(road map)         
```

The `quote` form has no effect on a literal expression such as a number
or string:

```racket
> (quote 42)             
42                       
> (quote "on the record")
"on the record"          
```

## 2. Abbreviating `quote` with `'`

As you may have guessed, you can abbreviate a use of `quote` by just
putting `'` in front of a form to quote:

```racket
> '(1 2 3)                     
'(1 2 3)                       
> 'road                        
'road                          
> '((1 2 3) road ("a" "b" "c"))
'((1 2 3) road ("a" "b" "c"))  
```

In the documentation, `'` within an expression is printed in green along
with the form after it, since the combination is an expression that is a
constant. In DrRacket, only the `'` is colored green. DrRacket is more
precisely correct, because the meaning of `quote` can vary depending on
the context of an expression. In the documentation, however, we
routinely assume that standard bindings are in scope, and so we paint
quoted forms in green for extra clarity.

A `'` expands to a `quote` form in quite a literal way. You can see this
if you put a `'` in front of a form that has a `'`:

```racket
> (car ''road)       
'quote               
> (car '(quote road))
'quote               
```

The `'` abbreviation works in output as well as input. The REPL’s
printer recognizes the symbol `'quote` as the first element of a
two-element list when printing output, in which case it uses `’` to
print the output:

```racket
> (quote (quote road))
”road                 
> '(quote road)       
”road                 
> ''road              
”road                 
```

## 3. Lists and Racket Syntax

Now that you know the truth about pairs and lists, and now that you’ve
seen `quote`, you’re ready to understand the main way in which we have
been simplifying Racket’s true syntax.

The syntax of Racket is not defined directly in terms of character
streams. Instead, the syntax is determined by two layers:

* a _reader_ layer, which turns a sequence of characters into lists,
  symbols, and other constants; and

* an _expander_ layer, which processes the lists, symbols, and other
  constants to parse them as an expression.

The rules for printing and reading go together. For example, a list is
printed with parentheses, and reading a pair of parentheses produces a
list. Similarly, a non-list pair is printed with the dot notation, and a
dot on input effectively runs the dot-notation rules in reverse to
obtain a pair.

One consequence of the read layer for expressions is that you can use
the dot notation in expressions that are not quoted forms:

```racket
> (+ 1 . (2))
3            
```

This works because `(+ 1 . (2))` is just another way of writing `(+ 1
2)`. It is practically never a good idea to write application
expressions using this dot notation; it’s just a consequence of the way
Racket’s syntax is defined.

Normally, `.` is allowed by the reader only with a parenthesized
sequence, and only before the last element of the sequence. However, a
pair of `.`s can also appear around a single element in a parenthesized
sequence, as long as the element is not first or last. Such a pair
triggers a reader conversion that moves the element between `.`s to the
front of the list. The conversion enables a kind of general infix
notation:

```racket
> (1 . < . 2) 
#t            
> '(1 . < . 2)
'(< 1 2)      
```

This two-dot convention is non-traditional, and it has essentially
nothing to do with the dot notation for non-list pairs. Racket
programmers use the infix convention sparingly—mostly for asymmetric
binary operators such as `<` and `is-a?`.
