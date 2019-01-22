# Reader Extensions

> +\[missing\] in \[missing\] provides more on reader extensions.

The reader layer of the Racket language can be extended through the
`#reader` form. A reader extension is implemented as a module that is
named after `#reader`. The module exports functions that parse raw
characters into a form to be consumed by the expander layer.

The syntax of `#reader` is

`#reader` >_module-path_< >_reader-specific_<

where >_module-path_< names a module that provides `read` and
`read-syntax` functions. The >_reader-specific_< part is a sequence of
characters that is parsed as determined by the `read` and `read-syntax`
functions from >_module-path_<.

For example, suppose that file `"five.rkt"` contains

`"five.rkt"`
```racket
#lang racket/base                                      
                                                       
(provide read read-syntax)                             
                                                       
(define (read in) (list (read-string 5 in)))           
(define (read-syntax src in) (list (read-string 5 in)))
```

Then, the program

```racket
#lang racket/base             
                              
'(1 #reader"five.rkt"234567 8)
```

is equivalent to

```racket
#lang racket/base 
                  
'(1 ("23456") 7 8)
```

because the `read` and `read-syntax` functions of `"five.rkt"` both read
five characters from the input stream and put them into a string and
then a list. The reader functions from `"five.rkt"` are not obliged to
follow Racket lexical conventions and treat the continuous sequence
`234567` as a single number. Since only the `23456` part is consumed by
`read` or `read-syntax`, the `7` remains to be parsed in the usual
Racket way. Similarly, the reader functions from `"five.rkt"` are not
obliged to ignore whitespace, and

```racket
#lang racket/base              
                               
'(1 #reader"five.rkt" 234567 8)
```

is equivalent to

```racket
#lang racket/base  
                   
'(1 (" 2345") 67 8)
```

since the first character immediately after `"five.rkt"` is a space.

A `#reader` form can be used in the REPL, too:

```racket
> '#reader"five.rkt"abcde
'("abcde")               
```

## 1. Source Locations

The difference between `read` and `read-syntax` is that `read` is meant
to be used for data while `read-syntax` is meant to be used to parse
programs. More precisely, the `read` function will be used when the
enclosing stream is being parsed by the Racket `read`, and `read-syntax`
is used when the enclosing stream is being parsed by the Racket
`read-syntax` function. Nothing requires `read` and `read-syntax` to
parse input in the same way, but making them different would confuse
programmers and tools.

The `read-syntax` function can return the same kind of value as `read`,
but it should normally return a syntax object that connects the parsed
expression with source locations. Unlike the `"five.rkt"` example, the
`read-syntax` function is typically implemented directly to produce
syntax objects, and then `read` can use `read-syntax` and strip away
syntax object wrappers to produce a raw result.

The following `"arith.rkt"` module implements a reader to parse simple
infix arithmetic expressions into Racket forms. For example, `1*2+3`
parses into the Racket form `(+ (* 1 2) 3)`. The supported operators are
`+`, `-`, `*`, and `/`, while operands can be unsigned integers or
single-letter variables. The implementation uses `port-next-location` to
obtain the current source location, and it uses `datum->syntax` to turn
raw values into syntax objects.

`"arith.rkt"`
```racket
#lang racket                                                
(require syntax/readerr)                                    
                                                            
(provide read read-syntax)                                  
                                                            
(define (read in)                                           
  (syntax->datum (read-syntax #f in)))                      
                                                            
(define (read-syntax src in)                                
  (skip-whitespace in)                                      
  (read-arith src in))                                      
                                                            
(define (skip-whitespace in)                                
  (regexp-match #px"^\\s*" in))                             
                                                            
(define (read-arith src in)                                 
  (define-values (line col pos) (port-next-location in))    
  (define expr-match                                        
    (regexp-match                                           
     ; Match an operand followed by any number of           
     ; operator–operand sequences, and prohibit an          
     ; additional operator from following immediately:      
     #px"^([a-z]|[0-9]+)(?:[-+*/]([a-z]|[0-9]+))*(?![-+*/])"
     in))                                                   
                                                            
  (define (to-syntax v delta span-str)                      
    (datum->syntax #f v (make-srcloc delta span-str)))      
  (define (make-srcloc delta span-str)                      
    (and line                                               
         (vector src line (+ col delta) (+ pos delta)       
                 (string-length span-str))))                
                                                            
  (define (parse-expr s delta)                              
    (match (or (regexp-match #rx"^(.*?)([+-])(.*)$" s)      
               (regexp-match #rx"^(.*?)([*/])(.*)$" s))     
      [(list _ a-str op-str b-str)                          
       (define a-len (string-length a-str))                 
       (define a (parse-expr a-str delta))                  
       (define b (parse-expr b-str (+ delta 1 a-len)))      
       (define op (to-syntax (string->symbol op-str)        
                             (+ delta a-len) op-str))       
       (to-syntax (list op a b) delta s)]                   
      [_ (to-syntax (or (string->number s)                  
                        (string->symbol s))                 
                    delta s)]))                             
                                                            
  (unless expr-match                                        
    (raise-read-error "bad arithmetic syntax"               
                      src line col pos                      
                      (and pos (- (file-position in) pos))))
  (parse-expr (bytes->string/utf-8 (car expr-match)) 0))    
```

If the `"arith.rkt"` reader is used in an expression position, then its
parse result will be treated as a Racket expression. If it is used in a
quoted form, however, then it just produces a number or a list:

```racket
> #reader"arith.rkt" 1*2+3 
5                          
> '#reader"arith.rkt" 1*2+3
'(+ (* 1 2) 3)             
```

The `"arith.rkt"` reader could also be used in positions that make no
sense. Since the `read-syntax` implementation tracks source locations,
syntax errors can at least refer to parts of the input in terms of their
original locations \(at the beginning of the error message\):

```racket
> (let #reader"arith.rkt" 1*2+3 8)                          
repl:1:27: let: bad syntax (not an identifier and expression
for a binding)                                              
  at: +                                                     
  in: (let (+ (* 1 2) 3) 8)                                 
```

## 2. Readtables

A reader extension’s ability to parse input characters in an arbitrary
way can be powerful, but many cases of lexical extension call for a less
general but more composable approach. In much the same way that the
expander level of Racket syntax can be extended through macros, the
reader level of Racket syntax can be composably extended through a
_readtable_.

The Racket reader is a recursive-descent parser, and the readtable maps
characters to parsing handlers. For example, the default readtable maps
`(` to a handler that recursively parses subforms until it finds a `)`.
The `current-readtable` parameter determines the readtable that is used
by `read` or `read-syntax`. Rather than parsing raw characters directly,
a reader extension can install an extended readtable and then chain to
`read` or `read-syntax`.

> +See \[missing\] for an introduction to parameters.

The `make-readtable` function constructs a new readtable as an extension
of an existing one. It accepts a sequence of specifications in terms of
a character, a type of mapping for the character, and \(for certain
types of mappings\) a parsing procedure. For example, to extend the
readtable so that `$` can be used to start and end infix expressions,
implement a `read-dollar` function and use:

```racket
(make-readtable (current-readtable)                
                #\$ 'terminating-macro read-dollar)
```

The protocol for `read-dollar` requires the function to accept different
numbers of arguments depending on whether it is being used in `read` or
`read-syntax` mode. In `read` mode, the parser function is given two
arguments: the character that triggered the parser function and the
input port that is being read. In `read-syntax` mode, the function must
accept four additional arguments that provide the source location of the
character.

The following `"dollar.rkt"` module defines a `read-dollar` function in
terms of the `read` and `read-syntax` functions provided by
`"arith.rkt"`, and it puts `read-dollar` together with new `read` and
`read-syntax` functions that install the readtable and chain to Racket’s
`read` or `read-syntax`:

`"dollar.rkt"`
```racket
#lang racket                                            
(require syntax/readerr                                 
         (prefix-in arith: "arith.rkt"))                
                                                        
(provide (rename-out [$-read read]                      
                     [$-read-syntax read-syntax]))      
                                                        
(define ($-read in)                                     
  (parameterize ([current-readtable (make-$-readtable)])
    (read in)))                                         
                                                        
(define ($-read-syntax src in)                          
  (parameterize ([current-readtable (make-$-readtable)])
    (read-syntax src in)))                              
                                                        
(define (make-$-readtable)                              
  (make-readtable (current-readtable)                   
                  #\$ 'terminating-macro read-dollar))  
                                                        
(define read-dollar                                     
  (case-lambda                                          
   [(ch in)                                             
    (check-$-after (arith:read in) in (object-name in))]
   [(ch in src line col pos)                            
    (check-$-after (arith:read-syntax src in) in src)]))
                                                        
(define (check-$-after val in src)                      
  (regexp-match #px"^\\s*" in) ; skip whitespace        
  (let ([ch (peek-char in)])                            
    (unless (equal? ch #\$) (bad-ending ch src in))     
    (read-char in))                                     
  val)                                                  
                                                        
(define (bad-ending ch src in)                          
  (let-values ([(line col pos) (port-next-location in)])
    ((if (eof-object? ch)                               
         raise-read-error                               
         raise-read-eof-error)                          
     "expected a closing `$'"                           
     src line col pos                                   
     (if (eof-object? ch) 0 1))))                       
```

With this reader extension, a single `#reader` can be used at the
beginning of an expression to enable multiple uses of `$` that switch to
infix arithmetic:

```racket
> #reader"dollar.rkt" (let ([a $1*2+3$] [b $5/6$]) $a+b$)
35/6                                                     
```
