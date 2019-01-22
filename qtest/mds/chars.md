# Characters

A Racket _character_ corresponds to a Unicode _scalar value_. Roughly, a
scalar value is an unsigned integer whose representation fits into 21
bits, and that maps to some notion of a natural-language character or
piece of a character. Technically, a scalar value is a simpler notion
than the concept called a “character” in the Unicode standard, but it’s
an approximation that works well for many purposes. For example, any
accented Roman letter can be represented as a scalar value, as can any
common Chinese character.

Although each Racket character corresponds to an integer, the character
datatype is separate from numbers. The `char->integer` and
`integer->char` procedures convert between scalar-value numbers and the
corresponding character.

A printable character normally prints as `#\` followed by the
represented character. An unprintable character normally prints as `#\u`
followed by the scalar value as hexadecimal number. A few characters are
printed specially; for example, the space and linefeed characters print
as `#\space` and `#\newline`, respectively.

> +\[missing\] in \[missing\] documents the fine points of the syntax of
> characters.

Examples:

```racket
> (integer->char 65)     
#\A                      
> (char->integer #\A)    
65                       
> #\λ                    
#\λ                      
> #\u03BB                
#\λ                      
> (integer->char 17)     
#\u0011                  
> (char->integer #\space)
32                       
```

The `display` procedure directly writes a character to the current
output port \(see \[missing\]\), in contrast to the character-constant
syntax used to print a character result.

Examples:

```racket
> #\A          
#\A            
> (display #\A)
A              
```

Racket provides several classification and conversion procedures on
characters. Beware, however, that conversions on some Unicode characters
work as a human would expect only when they are in a string \(e.g.,
upcasing “ß” or downcasing “Σ”\).

Examples:

```racket
> (char-alphabetic? #\A)      
#t                            
> (char-numeric? #\0)         
#t                            
> (char-whitespace? #\newline)
#t                            
> (char-downcase #\A)         
#\a                           
> (char-upcase #\ß)           
#\ß                           
```

The `char=?` procedure compares two or more characters, and `char-ci=?`
compares characters ignoring case. The `eqv?` and `equal?` procedures
behave the same as `char=?` on characters; use `char=?` when you want to
more specifically declare that the values being compared are characters.

Examples:

```racket
> (char=? #\a #\A)   
#f                   
> (char-ci=? #\a #\A)
#t                   
> (eqv? #\a #\A)     
#f                   
```

> +\[missing\] in \[missing\] provides more on characters and character
> procedures.
