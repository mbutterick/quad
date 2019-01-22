# Built-In Datatypes

The previous chapter introduced some of Racket’s built-in datatypes:
numbers, booleans, strings, lists, and procedures. This section provides
a more complete coverage of the built-in datatypes for simple forms of
data.

    1 Booleans              
                            
    2 Numbers               
                            
    3 Characters            
                            
    4 Strings \(Unicode\)   
                            
    5 Bytes and Byte Strings
                            
    6 Symbols               
                            
    7 Keywords              
                            
    8 Pairs and Lists       
                            
    9 Vectors               
                            
    10 Hash Tables          
                            
    11 Boxes                
                            
    12 Void and Undefined   

## 1. Booleans

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

## 2. Numbers

A Racket _number_ is either exact or inexact:

* An _exact_ number is either

  * an arbitrarily large or small integer, such as `5`,
    `99999999999999999`, or `-17`;

  * a rational that is exactly the ratio of two arbitrarily small or
    large integers, such as `1/2`, `99999999999999999/2`, or `-3/4`; or

  * a complex number with exact real and imaginary parts \(where the
    imaginary part is not zero\), such as `1+2i` or `1/2+3/4i`.

* An _inexact_ number is either

  * an IEEE floating-point representation of a number, such as `2.0` or
    `3.14e+87`, where the IEEE infinities and not-a-number are written
    `+inf.0`, `-inf.0`, and `+nan.0` \(or `-nan.0`\); or

  * a complex number with real and imaginary parts that are IEEE
    floating-point representations, such as `2.0+3.0i` or
    `-inf.0+nan.0i`; as a special case, an inexact complex number can
    have an exact zero real part with an inexact imaginary part.

Inexact numbers print with a decimal point or exponent specifier, and
exact numbers print as integers and fractions.  The same conventions
apply for reading number constants, but `#e` or `#i` can prefix a number
to force its parsing as an exact or inexact number. The prefixes `#b`,
`#o`, and `#x` specify binary, octal, and hexadecimal interpretation of
digits.

> +\[missing\] in \[missing\] documents the fine points of the syntax of
> numbers.

Examples:

```racket
> 0.5   
0.5     
> #e0.5 
1/2     
> #x03BB
955     
```

Computations that involve an inexact number produce inexact results, so
that inexactness acts as a kind of taint on numbers. Beware, however,
that Racket offers no “inexact booleans,” so computations that branch on
the comparison of inexact numbers can nevertheless produce exact
results. The procedures `exact->inexact` and `inexact->exact` convert
between the two types of numbers.

Examples:

```racket
> (/ 1 2)                         
1/2                               
> (/ 1 2.0)                       
0.5                               
> (if (= 3.0 2.999) 1 2)          
2                                 
> (inexact->exact 0.1)            
3602879701896397/36028797018963968
```

Inexact results are also produced by procedures such as `sqrt`, `log`,
and `sin` when an exact result would require representing real numbers
that are not rational. Racket can represent only rational numbers and
complex numbers with rational parts.

Examples:

```racket
> (sin 0)   ; rational...    
0                            
> (sin 1/2) ; not rational...
0.479425538604203            
```

In terms of performance, computations with small integers are typically
the fastest, where “small” means that the number fits into one bit less
than the machine’s word-sized representation for signed numbers.
Computation with very large exact integers or with non-integer exact
numbers can be much more expensive than computation with inexact
numbers.

```racket                                             
(define (sigma f a b)                                 
  (if (= a b)                                         
      0                                               
      (+ (f a) (sigma f (+ a 1) b))))                 
```                                                   
                                                      
```racket                                             
> (time (round (sigma (lambda (x) (/ 1 x)) 1 2000)))  
cpu time: 63 real time: 64 gc time: 0                 
8                                                     
> (time (round (sigma (lambda (x) (/ 1.0 x)) 1 2000)))
cpu time: 1 real time: 0 gc time: 0                   
8.0                                                   
```                                                   

The number categories _integer_, _rational_, _real_ \(always rational\),
and _complex_ are defined in the usual way, and are recognized by the
procedures `integer?`, `rational?`, `real?`, and `complex?`, in addition
to the generic `number?`. A few mathematical procedures accept only real
numbers, but most implement standard extensions to complex numbers.

Examples:

```racket
> (integer? 5)                        
#t                                    
> (complex? 5)                        
#t                                    
> (integer? 5.0)                      
#t                                    
> (integer? 1+2i)                     
#f                                    
> (complex? 1+2i)                     
#t                                    
> (complex? 1.0+2.0i)                 
#t                                    
> (abs -5)                            
5                                     
> (abs -5+2i)                         
abs: contract violation               
  expected: real?                     
  given: -5+2i                        
> (sin -5+2i)                         
3.6076607742131563+1.0288031496599337i
```

The `=` procedure compares numbers for numerical equality. If it is
given both inexact and exact numbers to compare, it essentially converts
the inexact numbers to exact before comparing. The `eqv?` \(and
therefore `equal?`\) procedure, in contrast, compares numbers
considering both exactness and numerical equality.

Examples:

```racket
> (= 1 1.0)   
#t            
> (eqv? 1 1.0)
#f            
```

Beware of comparisons involving inexact numbers, which by their nature
can have surprising behavior. Even apparently simple inexact numbers may
not mean what you think they mean; for example, while a base-2 IEEE
floating-point number can represent `1/2` exactly, it can only
approximate `1/10`:

Examples:

```racket
> (= 1/2 0.5)                     
#t                                
> (= 1/10 0.1)                    
#f                                
> (inexact->exact 0.1)            
3602879701896397/36028797018963968
```

> +\[missing\] in \[missing\] provides more on numbers and number
> procedures.

## 3. Characters

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

## 4. Strings \(Unicode\)

A _string_ is a fixed-length array of characters. It prints using
doublequotes, where doublequote and backslash characters within the
string are escaped with backslashes. Other common string escapes are
supported, including `\n` for a linefeed, `\r` for a carriage return,
octal escapes using `\` followed by up to three octal digits, and
hexadecimal escapes with `\u` \(up to four digits\).  Unprintable
characters in a string are normally shown with `\u` when the string is
printed.

> +\[missing\] in \[missing\] documents the fine points of the syntax of
> strings.

The `display` procedure directly writes the characters of a string to
the current output port \(see \[missing\]\), in contrast to the
string-constant syntax used to print a string result.

Examples:

```racket
> "Apple"                       
"Apple"                         
> "\u03BB"                      
"λ"                             
> (display "Apple")             
Apple                           
> (display "a \"quoted\" thing")
a "quoted" thing                
> (display "two\nlines")        
two                             
lines                           
> (display "\u03BB")            
λ                               
```

A string can be mutable or immutable; strings written directly as
expressions are immutable, but most other strings are mutable. The
`make-string` procedure creates a mutable string given a length and
optional fill character. The `string-ref` procedure accesses a character
from a string \(with 0-based indexing\); the `string-set!`  procedure
changes a character in a mutable string.

Examples:

```racket
> (string-ref "Apple" 0)        
#\A                             
> (define s (make-string 5 #\.))
> s                             
"....."                         
> (string-set! s 2 #\λ)         
> s                             
"..λ.."                         
```

String ordering and case operations are generally _locale-independent_;
that is, they work the same for all users. A few _locale-dependent_
operations are provided that allow the way that strings are case-folded
and sorted to depend on the end-user’s locale. If you’re sorting
strings, for example, use `string<?` or `string-ci<?` if the sort result
should be consistent across machines and users, but use
`string-locale<?` or `string-locale-ci<?` if the sort is purely to order
strings for an end user.

Examples:

```racket
> (string<? "apple" "Banana")         
#f                                    
> (string-ci<? "apple" "Banana")      
#t                                    
> (string-upcase "Straße")            
"STRASSE"                             
> (parameterize ([current-locale "C"])
    (string-locale-upcase "Straße"))  
"STRAßE"                              
```

For working with plain ASCII, working with raw bytes, or
encoding/decoding Unicode strings as bytes, use byte strings.

> +\[missing\] in \[missing\] provides more on strings and string
> procedures.

## 5. Bytes and Byte Strings

A _byte_ is an exact integer between `0` and `255`, inclusive. The
`byte?` predicate recognizes numbers that represent bytes.

Examples:

```racket
> (byte? 0)  
#t           
> (byte? 256)
#f           
```

A _byte string_ is similar to a string—see Strings \(Unicode\)—but its
content is a sequence of bytes instead of characters. Byte strings can
be used in applications that process pure ASCII instead of Unicode text.
The printed form of a byte string supports such uses in particular,
because a byte string prints like the ASCII decoding of the byte string,
but prefixed with a `#`. Unprintable ASCII characters or non-ASCII bytes
in the byte string are written with octal notation.

> +\[missing\] in \[missing\] documents the fine points of the syntax of
> byte strings.

Examples:

```racket
> #"Apple"                   
#"Apple"                     
> (bytes-ref #"Apple" 0)     
65                           
> (make-bytes 3 65)          
#"AAA"                       
> (define b (make-bytes 2 0))
> b                          
#"\0\0"                      
> (bytes-set! b 0 1)         
> (bytes-set! b 1 255)       
> b                          
#"\1\377"                    
```

The `display` form of a byte string writes its raw bytes to the current
output port \(see \[missing\]\). Technically, `display` of a normal
\(i.e,. character\) string prints the UTF-8 encoding of the string to
the current output port, since output is ultimately defined in terms of
bytes; `display` of a byte string, however, writes the raw bytes with no
encoding. Along the same lines, when this documentation shows output, it
technically shows the UTF-8-decoded form of the output.

Examples:

```racket
> (display #"Apple")                         
Apple                                        
> (display "\316\273")  ; same as "Î»"       
Î»                                           
> (display #"\316\273") ; UTF-8 encoding of λ
λ                                            
```

For explicitly converting between strings and byte strings, Racket
supports three kinds of encodings directly: UTF-8, Latin-1, and the
current locale’s encoding. General facilities for byte-to-byte
conversions \(especially to and from UTF-8\) fill the gap to support
arbitrary string encodings.

Examples:

```racket
> (bytes->string/utf-8 #"\316\273")                               
"λ"                                                               
> (bytes->string/latin-1 #"\316\273")                             
"Î»"                                                              
> (parameterize ([current-locale "C"])  ; C locale supports ASCII,
    (bytes->string/locale #"\316\273")) ; only, so...             
bytes->string/locale: byte string is not a valid encoding         
for the current locale                                            
  byte string: #"\316\273"                                        
> (let ([cvt (bytes-open-converter "cp1253" ; Greek code page     
                                   "UTF-8")]                      
        [dest (make-bytes 2)])                                    
    (bytes-convert cvt #"\353" 0 1 dest)                          
    (bytes-close-converter cvt)                                   
    (bytes->string/utf-8 dest))                                   
"λ"                                                               
```

> +\[missing\] in \[missing\] provides more on byte strings and
> byte-string procedures.

## 6. Symbols

A _symbol_ is an atomic value that prints like an identifier preceded
with `'`.  An expression that starts with `'` and continues with an
identifier produces a symbol value.

Examples:

```racket
> 'a          
'a            
> (symbol? 'a)
#t            
```

For any sequence of characters, exactly one corresponding symbol is
_interned_; calling the `string->symbol` procedure, or `read`ing a
syntactic identifier, produces an interned symbol. Since interned
symbols can be cheaply compared with `eq?` \(and thus `eqv?` or
`equal?`\), they serve as a convenient values to use for tags and
enumerations.

Symbols are case-sensitive. By using a `#ci` prefix or in other ways,
the reader can be made to case-fold character sequences to arrive at a
symbol, but the reader preserves case by default.

Examples:

```racket
> (eq? 'a 'a)                  
#t                             
> (eq? 'a (string->symbol "a"))
#t                             
> (eq? 'a 'b)                  
#f                             
> (eq? 'a 'A)                  
#f                             
> #ci'A                        
'a                             
```

Any string \(i.e., any character sequence\) can be supplied to
`string->symbol` to obtain the corresponding symbol. For reader input,
any character can appear directly in an identifier, except for
whitespace and the following special characters:

   `(` `)` `[` `]` `{` `}` `"` `,` `'` ` `;` `#` `|` `\`

Actually, `#` is disallowed only at the beginning of a symbol, and then
only if not followed by `%`; otherwise, `#` is allowed, too. Also, `.`
by itself is not a symbol.

Whitespace or special characters can be included in an identifier by
quoting them with `|` or `\`. These quoting mechanisms are used in the
printed form of identifiers that contain special characters or that
might otherwise look like numbers.

Examples:

```racket
> (string->symbol "one, two")
'|one, two|                  
> (string->symbol "6")       
'|6|                         
```

> +\[missing\] in \[missing\] documents the fine points of the syntax of
> symbols.

The `write` function prints a symbol without a `'` prefix. The `display`
form of a symbol is the same as the corresponding string.

Examples:

```racket
> (write 'Apple)  
Apple             
> (display 'Apple)
Apple             
> (write '|6|)    
|6|               
> (display '|6|)  
6                 
```

The `gensym` and `string->uninterned-symbol` procedures generate fresh
_uninterned_ symbols that are not equal \(according to `eq?`\) to any
previously interned or uninterned symbol. Uninterned symbols are useful
as fresh tags that cannot be confused with any other value.

Examples:

```racket
> (define s (gensym))                     
> s                                       
'g42                                      
> (eq? s 'g42)                            
#f                                        
> (eq? 'a (string->uninterned-symbol "a"))
#f                                        
```

> +\[missing\] in \[missing\] provides more on symbols.

## 7. Keywords

A _keyword_ value is similar to a symbol \(see Symbols\), but its
printed form is prefixed with `#:`.

> +\[missing\] in \[missing\] documents the fine points of the syntax of
> keywords.

Examples:

```racket
> (string->keyword "apple")               
'#:apple                                  
> '#:apple                                
'#:apple                                  
> (eq? '#:apple (string->keyword "apple"))
#t                                        
```

More precisely, a keyword is analogous to an identifier; in the same way
that an identifier can be quoted to produce a symbol, a keyword can be
quoted to produce a value. The same term “keyword” is used in both
cases, but we sometimes use _keyword value_ to refer more specifically
to the result of a quote-keyword expression or of `string->keyword`. An
unquoted keyword is not an expression, just as an unquoted identifier
does not produce a symbol:

Examples:

```racket
> not-a-symbol-expression                            
not-a-symbol-expression: undefined;                  
 cannot reference an identifier before its definition
  in module: top-level                               
> #:not-a-keyword-expression                         
eval:2:0: #%datum: keyword misused as an expression  
  at: #:not-a-keyword-expression                     
```

Despite their similarities, keywords are used in a different way than
identifiers or symbols. Keywords are intended for use \(unquoted\) as
special markers in argument lists and in certain syntactic forms.  For
run-time flags and enumerations, use symbols instead of keywords.  The
example below illustrates the distinct roles of keywords and symbols.

Examples:

```racket
> (define dir (find-system-path 'temp-dir)) ; not '#:temp-dir   
> (with-output-to-file (build-path dir "stuff.txt")             
    (lambda () (printf "example\n"))                            
    ; optional #:mode argument can be 'text or 'binary          
    #:mode 'text                                                
    ; optional #:exists argument can be 'replace, 'truncate, ...
    #:exists 'replace)                                          
```

## 8. Pairs and Lists

A _pair_ joins two arbitrary values. The `cons` procedure constructs
pairs, and the `car` and `cdr` procedures extract the first and second
elements of the pair, respectively. The `pair?` predicate recognizes
pairs.

Some pairs print by wrapping parentheses around the printed forms of the
two pair elements, putting a `'` at the beginning and a `.` between the
elements.

Examples:

```racket
> (cons 1 2)         
'(1 . 2)             
> (cons (cons 1 2) 3)
'((1 . 2) . 3)       
> (car (cons 1 2))   
1                    
> (cdr (cons 1 2))   
2                    
> (pair? (cons 1 2)) 
#t                   
```

A _list_ is a combination of pairs that creates a linked list. More
precisely, a list is either the empty list `null`, or it is a pair whose
first element is a list element and whose second element is a list. The
`list?` predicate recognizes lists. The `null?`  predicate recognizes
the empty list.

A list normally prints as a `'` followed by a pair of parentheses
wrapped around the list elements.

Examples:

```racket
> null                           
'()                              
> (cons 0 (cons 1 (cons 2 null)))
'(0 1 2)                         
> (list? null)                   
#t                               
> (list? (cons 1 (cons 2 null))) 
#t                               
> (list? (cons 1 2))             
#f                               
```

A list or pair prints using `list` or `cons` when one of its elements
cannot be written as a `quote`d value. For example, a value constructed
with `srcloc` cannot be written using `quote`, and it prints using
`srcloc`:

```racket
> (srcloc "file.rkt" 1 0 1 (+ 4 4))              
(srcloc "file.rkt" 1 0 1 8)                      
> (list 'here (srcloc "file.rkt" 1 0 1 8) 'there)
(list 'here (srcloc "file.rkt" 1 0 1 8) 'there)  
> (cons 1 (srcloc "file.rkt" 1 0 1 8))           
(cons 1 (srcloc "file.rkt" 1 0 1 8))             
> (cons 1 (cons 2 (srcloc "file.rkt" 1 0 1 8)))  
(list* 1 2 (srcloc "file.rkt" 1 0 1 8))          
```

> See also `list*`.

As shown in the last example, `list*` is used to abbreviate a series of
`cons`es that cannot be abbreviated using `list`.

The `write` and `display` functions print a pair or list without a
leading `'`, `cons`, `list`, or `list*`. There is no difference between
`write` and `display` for a pair or list, except as they apply to
elements of the list:

Examples:

```racket
> (write (cons 1 2))      
(1 . 2)                   
> (display (cons 1 2))    
(1 . 2)                   
> (write null)            
()                        
> (display null)          
()                        
> (write (list 1 2 "3"))  
(1 2 "3")                 
> (display (list 1 2 "3"))
(1 2 3)                   
```

Among the most important predefined procedures on lists are those that
iterate through the list’s elements:

```racket
> (map (lambda (i) (/ 1 i))                                
       '(1 2 3))                                           
'(1 1/2 1/3)                                               
> (andmap (lambda (i) (i . < . 3))                         
         '(1 2 3))                                         
#f                                                         
> (ormap (lambda (i) (i . < . 3))                          
         '(1 2 3))                                         
#t                                                         
> (filter (lambda (i) (i . < . 3))                         
          '(1 2 3))                                        
'(1 2)                                                     
> (foldl (lambda (v i) (+ v i))                            
         10                                                
         '(1 2 3))                                         
16                                                         
> (for-each (lambda (i) (display i))                       
            '(1 2 3))                                      
123                                                        
> (member "Keys"                                           
          '("Florida" "Keys" "U.S.A."))                    
'("Keys" "U.S.A.")                                         
> (assoc 'where                                            
         '((when "3:30") (where "Florida") (who "Mickey")))
'(where "Florida")                                         
```

> +\[missing\] in \[missing\] provides more on pairs and lists.

Pairs are immutable \(contrary to Lisp tradition\), and `pair?` and
`list?` recognize immutable pairs and lists, only. The `mcons` procedure
creates a _mutable pair_, which works with `set-mcar!` and `set-mcdr!`,
as well as `mcar` and `mcdr`. A mutable pair prints using `mcons`, while
`write` and `display` print mutable pairs with `{` and `}`:

Examples:

```racket
> (define p (mcons 1 2))
> p                     
(mcons 1 2)             
> (pair? p)             
#f                      
> (mpair? p)            
#t                      
> (set-mcar! p 0)       
> p                     
(mcons 0 2)             
> (write p)             
{0 . 2}                 
```

> +\[missing\] in \[missing\] provides more on mutable pairs.

## 9. Vectors

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

## 10. Hash Tables

A _hash table_ implements a mapping from keys to values, where both keys
and values can be arbitrary Racket values, and access and update to the
table are normally constant-time operations. Keys are compared using
`equal?`, `eqv?`, or `eq?`, depending on whether the hash table is
created with `make-hash`, `make-hasheqv`, or `make-hasheq`.

Examples:

```racket
> (define ht (make-hash))               
> (hash-set! ht "apple" '(red round))   
> (hash-set! ht "banana" '(yellow long))
> (hash-ref ht "apple")                 
'(red round)                            
> (hash-ref ht "coconut")               
hash-ref: no value found for key        
  key: "coconut"                        
> (hash-ref ht "coconut" "not there")   
"not there"                             
```

The `hash`, `hasheqv`, and `hasheq` functions create immutable hash
tables from an initial set of keys and values, in which each value is
provided as an argument after its key. Immutable hash tables can be
extended with `hash-set`, which produces a new immutable hash table in
constant time.

Examples:

```racket
> (define ht (hash "apple" 'red "banana" 'yellow))
> (hash-ref ht "apple")                           
'red                                              
> (define ht2 (hash-set ht "coconut" 'brown))     
> (hash-ref ht "coconut")                         
hash-ref: no value found for key                  
  key: "coconut"                                  
> (hash-ref ht2 "coconut")                        
'brown                                            
```

A literal immutable hash table can be written as an expression by using
`#hash` \(for an `equal?`-based table\), `#hasheqv` \(for an
`eqv?`-based table\), or `#hasheq` \(for an `eq?`-based table\). A
parenthesized sequence must immediately follow `#hash`, `#hasheq`, or
`#hasheqv`, where each element is a dotted key–value pair. The `#hash`,
etc. forms implicitly `quote` their key and value sub-forms.

Examples:

```racket
> (define ht #hash(("apple" . red)      
                   ("banana" . yellow)))
> (hash-ref ht "apple")                 
'red                                    
```

> +\[missing\] in \[missing\] documents the fine points of the syntax of
> hash table literals.

Both mutable and immutable hash tables print like immutable hash tables,
using a quoted `#hash`, `#hasheqv`, or `#hasheq` form if all keys and
values can be expressed with `quote` or using `hash`, `hasheq`, or
`hasheqv` otherwise:

Examples:

```racket
> #hash(("apple" . red)                     
        ("banana" . yellow))                
'#hash(("banana" . yellow) ("apple" . red)) 
> (hash 1 (srcloc "file.rkt" 1 0 1 (+ 4 4)))
(hash 1 (srcloc "file.rkt" 1 0 1 8))        
```

A mutable hash table can optionally retain its keys _weakly_, so each
mapping is retained only so long as the key is retained elsewhere.

Examples:

```racket
> (define ht (make-weak-hasheq))           
> (hash-set! ht (gensym) "can you see me?")
> (collect-garbage)                        
> (hash-count ht)                          
0                                          
```

Beware that even a weak hash table retains its values strongly, as long
as the corresponding key is accessible. This creates a catch-22
dependency when a value refers back to its key, so that the mapping is
retained permanently. To break the cycle, map the key to an _ephemeron_
that pairs the value with its key \(in addition to the implicit pairing
of the hash table\).

> +\[missing\] in \[missing\] documents the fine points of using
> ephemerons.

Examples:

```racket
> (define ht (make-weak-hasheq))
> (let ([g (gensym)])           
    (hash-set! ht g (list g)))  
> (collect-garbage)             
> (hash-count ht)               
1                               
```

```racket
> (define ht (make-weak-hasheq))                 
> (let ([g (gensym)])                            
    (hash-set! ht g (make-ephemeron g (list g))))
> (collect-garbage)                              
> (hash-count ht)                                
0                                                
```

> +\[missing\] in \[missing\] provides more on hash tables and hash-table
> procedures.

## 11. Boxes

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

## 12. Void and Undefined

Some procedures or expression forms have no need for a result value. For
example, the `display` procedure is called only for the side-effect of
writing output. In such cases the result value is normally a special
constant that prints as `#<void>`.  When the result of an expression is
simply `#<void>`, the REPL does not print anything.

The `void` procedure takes any number of arguments and returns
`#<void>`. \(That is, the identifier `void` is bound to a procedure that
returns `#<void>`, instead of being bound directly to `#<void>`.\)

Examples:

```racket
> (void)       
> (void 1 2 3) 
> (list (void))
'(#<void>)     
```

The `undefined` constant, which prints as `#<undefined>`, is sometimes
used as the result of a reference whose value is not yet available. In
previous versions of Racket \(before version 6.1\), referencing a local
binding too early produced `#<undefined>`; too-early references now
raise an exception, instead.

> The `undefined` result can still be produced in some cases by the
> `shared` form.

```racket                        
(define (fails)                  
  (define x x)                   
  x)                             
```                              
                                 
```racket                        
> (fails)                        
x: undefined;                    
 cannot use before initialization
```                              
