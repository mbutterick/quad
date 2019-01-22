# Lists, Iteration, and Recursion

Racket is a dialect of the language Lisp, whose name originally stood
for “LISt Processor.” The built-in list datatype remains a prominent
feature of the language.

The `list` function takes any number of values and returns a list
containing the values:

```racket
> (list "red" "green" "blue")
'("red" "green" "blue")      
> (list 1 2 3 4 5)           
'(1 2 3 4 5)                 
```

> A list usually prints with `'`, but the printed form of a list depends
> on its content. See \[missing\] for more information.

As you can see, a list result prints in the REPL as a quote `'` and then
a pair of parentheses wrapped around the printed form of the list
elements. There’s an opportunity for confusion here, because parentheses
are used for both expressions, such as `(list "red" "green" "blue")`,
and printed results, such as `'("red" "green" "blue")`. In addition to
the quote, parentheses for results are printed in blue in the
documentation and in DrRacket, whereas parentheses for expressions are
brown.

Many predefined functions operate on lists. Here are a few examples:

```racket
> (length (list "hop" "skip" "jump"))        ; count the elements  
3                                                                  
> (list-ref (list "hop" "skip" "jump") 0)    ; extract by position 
"hop"                                                              
> (list-ref (list "hop" "skip" "jump") 1)                          
"skip"                                                             
> (append (list "hop" "skip") (list "jump")) ; combine lists       
'("hop" "skip" "jump")                                             
> (reverse (list "hop" "skip" "jump"))       ; reverse order       
'("jump" "skip" "hop")                                             
> (member "fall" (list "hop" "skip" "jump")) ; check for an element
#f                                                                 
```

## 1. Predefined List Loops

In addition to simple operations like `append`, Racket includes
functions that iterate over the elements of a list. These iteration
functions play a role similar to `for` in Java, Racket, and other
languages. The body of a Racket iteration is packaged into a function to
be applied to each element, so the `lambda` form becomes particularly
handy in combination with iteration functions.

Different list-iteration functions combine iteration results in
different ways. The `map` function uses the per-element results to
create a new list:

```racket
> (map sqrt (list 1 4 9 16))                    
'(1 2 3 4)                                      
> (map (lambda (i)                              
         (string-append i "!"))                 
       (list "peanuts" "popcorn" "crackerjack"))
'("peanuts!" "popcorn!" "crackerjack!")         
```

The `andmap` and `ormap` functions combine the results by `and`ing or
`or`ing:

```racket
> (andmap string? (list "a" "b" "c"))
#t                                   
> (andmap string? (list "a" "b" 6))  
#f                                   
> (ormap number? (list "a" "b" 6))   
#t                                   
```

The `map`, `andmap`, and `ormap` functions can all handle multiple
lists, instead of just a single list. The lists must all have the same
length, and the given function must accept one argument for each list:

```racket
> (map (lambda (s n) (substring s 0 n))        
       (list "peanuts" "popcorn" "crackerjack")
       (list 6 3 7))                           
'("peanut" "pop" "cracker")                    
```

The `filter` function keeps elements for which the body result is true,
and discards elements for which it is `#f`:

```racket
> (filter string? (list "a" "b" 6))   
'("a" "b")                            
> (filter positive? (list 1 -2 6 7 0))
'(1 6 7)                              
```

The `foldl` function generalizes some iteration functions. It uses the
per-element function to both process an element and combine it with the
“current” value, so the per-element function takes an extra first
argument. Also, a starting “current” value must be provided before the
lists:

```racket
> (foldl (lambda (elem v)      
           (+ v (* elem elem)))
         0                     
         '(1 2 3))             
14                             
```

Despite its generality, `foldl` is not as popular as the other
functions. One reason is that `map`, `ormap`, `andmap`, and `filter`
cover the most common kinds of list loops.

Racket provides a general _list comprehension_ form `for/list`, which
builds a list by iterating through _sequences_. List comprehensions and
related iteration forms are described in \[missing\].

## 2. List Iteration from Scratch

Although `map` and other iteration functions are predefined, they are
not primitive in any interesting sense. You can write equivalent
iterations using a handful of list primitives.

Since a Racket list is a linked list, the two core operations on a
non-empty list are

* `first`: get the first thing in the list; and

* `rest`: get the rest of the list.

Examples:

```racket
> (first (list 1 2 3))
1                     
> (rest (list 1 2 3)) 
'(2 3)                
```

To create a new node for a linked list—that is, to add to the front of
the list—use the `cons` function, which is short for “construct.” To get
an empty list to start with, use the `empty` constant:

```racket
> empty                            
'()                                
> (cons "head" empty)              
'("head")                          
> (cons "dead" (cons "head" empty))
'("dead" "head")                   
```

To process a list, you need to be able to distinguish empty lists from
non-empty lists, because `first` and `rest` work only on non-empty
lists. The `empty?` function detects empty lists, and `cons?` detects
non-empty lists:

```racket
> (empty? empty)              
#t                            
> (empty? (cons "head" empty))
#f                            
> (cons? empty)               
#f                            
> (cons? (cons "head" empty)) 
#t                            
```

With these pieces, you can write your own versions of the `length`
function, `map` function, and more.

Examples:

```racket
(define (my-length lst)                 
  (cond                                 
   [(empty? lst) 0]                     
   [else (+ 1 (my-length (rest lst)))]))
                                        
> (my-length empty)                     
0                                       
> (my-length (list "a" "b" "c"))        
3                                       
```

```racket                                         
(define (my-map f lst)                            
  (cond                                           
   [(empty? lst) empty]                           
   [else (cons (f (first lst))                    
               (my-map f (rest lst)))]))          
```                                               
                                                  
```racket                                         
> (my-map string-upcase (list "ready" "set" "go"))
'("READY" "SET" "GO")                             
```                                               

If the derivation of the above definitions is mysterious to you,
consider reading _[How to Design Programs](http://www.htdp.org)_. If you
are merely suspicious of the use of recursive calls instead of a looping
construct, then read on.

## 3. Tail Recursion

Both the `my-length` and `my-map` functions run in _O_\(_n_\)__ space
for a list of length _n_. This is easy to see by imagining how
`(my-length (list "a" "b" "c"))` must evaluate:

```racket
(my-length (list "a" "b" "c"))        
= (+ 1 (my-length (list "b" "c")))    
= (+ 1 (+ 1 (my-length (list "c"))))  
= (+ 1 (+ 1 (+ 1 (my-length (list)))))
= (+ 1 (+ 1 (+ 1 0)))                 
= (+ 1 (+ 1 1))                       
= (+ 1 2)                             
= 3                                   
```

For a list with _n_ elements, evaluation will stack up _n_ `(+ 1 ...)`
additions, and then finally add them up when the list is exhausted.

You can avoid piling up additions by adding along the way. To accumulate
a length this way, we need a function that takes both a list and the
length of the list seen so far; the code below uses a local function
`iter` that accumulates the length in an argument `len`:

```racket
(define (my-length lst)                  
  ; local function iter:                 
  (define (iter lst len)                 
    (cond                                
     [(empty? lst) len]                  
     [else (iter (rest lst) (+ len 1))]))
  ; body of my-length calls iter:        
  (iter lst 0))                          
```

Now evaluation looks like this:

```racket
(my-length (list "a" "b" "c"))
= (iter (list "a" "b" "c") 0) 
= (iter (list "b" "c") 1)     
= (iter (list "c") 2)         
= (iter (list) 3)             
3                             
```

The revised `my-length` runs in constant space, just as the evaluation
steps above suggest. That is, when the result of a function call, like
`(iter (list "b" "c") 1)`, is exactly the result of some other function
call, like `(iter (list "c") 2)`, then the first one doesn’t have to
wait around for the second one, because that takes up space for no good
reason.

This evaluation behavior is sometimes called _tail-call optimization_,
but it’s not merely an “optimization” in Racket; it’s a guarantee about
the way the code will run. More precisely, an expression in _tail
position_ with respect to another expression does not take extra
computation space over the other expression.

In the case of `my-map`, _O_\(_n_\)__ space complexity is reasonable,
since it has to generate a result of size _O_\(_n_\)__. Nevertheless,
you can reduce the constant factor by accumulating the result list. The
only catch is that the accumulated list will be backwards, so you’ll
have to reverse it at the very end:

> Attempting to reduce a constant factor like this is usually not
> worthwhile, as discussed below.

```racket
(define (my-map f lst)                       
  (define (iter lst backward-result)         
    (cond                                    
     [(empty? lst) (reverse backward-result)]
     [else (iter (rest lst)                  
                 (cons (f (first lst))       
                       backward-result))]))  
  (iter lst empty))                          
```

It turns out that if you write

```racket
(define (my-map f lst)
  (for/list ([i lst]) 
    (f i)))           
```

then the `for/list` form in the function is expanded to essentially the
same code as the `iter` local definition and use. The difference is
merely syntactic convenience.

## 4. Recursion versus Iteration

The `my-length` and `my-map` examples demonstrate that iteration is just
a special case of recursion. In many languages, it’s important to try to
fit as many computations as possible into iteration form. Otherwise,
performance will be bad, and moderately large inputs can lead to stack
overflow.  Similarly, in Racket, it is sometimes important to make sure
that tail recursion is used to avoid _O_\(_n_\)__ space consumption when
the computation is easily performed in constant space.

At the same time, recursion does not lead to particularly bad
performance in Racket, and there is no such thing as stack overflow; you
can run out of memory if a computation involves too much context, but
exhausting memory typically requires orders of magnitude deeper
recursion than would trigger a stack overflow in other languages. These
considerations, combined with the fact that tail-recursive programs
automatically run the same as a loop, lead Racket programmers to embrace
recursive forms rather than avoid them.

Suppose, for example, that you want to remove consecutive duplicates
from a list. While such a function can be written as a loop that
remembers the previous element for each iteration, a Racket programmer
would more likely just write the following:

```racket                                     
(define (remove-dups l)                       
  (cond                                       
   [(empty? l) empty]                         
   [(empty? (rest l)) l]                      
   [else                                      
    (let ([i (first l)])                      
      (if (equal? i (first (rest l)))         
          (remove-dups (rest l))              
          (cons i (remove-dups (rest l)))))]))
```                                           
                                              
```racket                                     
> (remove-dups (list "a" "b" "b" "b" "c" "c"))
'("a" "b" "c")                                
```                                           

In general, this function consumes _O_\(_n_\)__ space for an input list
of length _n_, but that’s fine, since it produces an _O_\(_n_\)__
result. If the input list happens to be mostly consecutive duplicates,
then the resulting list can be much smaller than _O_\(_n_\)__—and
`remove-dups` will also use much less than _O_\(_n_\)__ space! The
reason is that when the function discards duplicates, it returns the
result of a `remove-dups` call directly, so the tail-call “optimization”
kicks in:

```racket
(remove-dups (list "a" "b" "b" "b" "b" "b"))         
= (cons "a" (remove-dups (list "b" "b" "b" "b" "b")))
= (cons "a" (remove-dups (list "b" "b" "b" "b")))    
= (cons "a" (remove-dups (list "b" "b" "b")))        
= (cons "a" (remove-dups (list "b" "b")))            
= (cons "a" (remove-dups (list "b")))                
= (cons "a" (list "b"))                              
= (list "a" "b")                                     
```
