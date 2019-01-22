# Keywords

A _keyword_ value is similar to a symbol \(see \[missing\]\), but its
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
