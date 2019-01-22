# Bytes and Byte Strings

A _byte_ is an exact integer between `0` and `255`, inclusive. The
`byte?` predicate recognizes numbers that represent bytes.

Examples:

```racket
> (byte? 0)  
#t           
> (byte? 256)
#f           
```

A _byte string_ is similar to a string—see \[missing\]—but its content
is a sequence of bytes instead of characters. Byte strings can be used
in applications that process pure ASCII instead of Unicode text. The
printed form of a byte string supports such uses in particular, because
a byte string prints like the ASCII decoding of the byte string, but
prefixed with a `#`. Unprintable ASCII characters or non-ASCII bytes in
the byte string are written with octal notation.

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
