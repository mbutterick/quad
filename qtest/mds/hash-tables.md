# Hash Tables

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
