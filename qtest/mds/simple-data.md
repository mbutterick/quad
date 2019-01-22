# Simple Values

Racket values include numbers, booleans, strings, and byte strings. In
DrRacket and documentation examples \(when you read the documentation in
color\), value expressions are shown in green.

_Numbers_ are written in the usual way, including fractions and
imaginary numbers:

> +\[missing\] \(later in this guide\) explains more about numbers.

```racket
1       3.14                  
1/2     6.02e+23              
1+2i    9999999999999999999999
```

_Booleans_ are `#t` for true and `#f` for false. In conditionals,
however, all non-`#f` values are treated as true.

> +\[missing\] \(later in this guide\) explains more about booleans.

_Strings_ are written between doublequotes. Within a string, backslash
is an escaping character; for example, a backslash followed by a
doublequote includes a literal doublequote in the string. Except for an
unescaped doublequote or backslash, any Unicode character can appear in
a string constant.

> +\[missing\] \(later in this guide\) explains more about strings.

```racket
"Hello, world!"            
"Benjamin \"Bugsy\" Siegel"
"λx:(μα.α→α).xx"           
```

When a constant is evaluated in the REPL, it typically prints the same
as its input syntax. In some cases, the printed form is a normalized
version of the input syntax. In documentation and in DrRacket’s REPL,
results are printed in blue instead of green to highlight the difference
between an input expression and a printed result.

Examples:

```racket
> 1.0000                         
1.0                              
> "Bugs \u0022Figaro\u0022 Bunny"
"Bugs \"Figaro\" Bunny"          
```
