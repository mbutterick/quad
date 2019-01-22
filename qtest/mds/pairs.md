# Pairs and Lists

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
