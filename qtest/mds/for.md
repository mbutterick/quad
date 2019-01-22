# Iterations and Comprehensions

The `for` family of syntactic forms support iteration over _sequences_.
Lists, vectors, strings, byte strings, input ports, and hash tables can
all be used as sequences, and constructors like `in-range` offer even
more kinds of sequences.

Variants of `for` accumulate iteration results in different ways, but
they all have the same syntactic shape. Simplifying for now, the syntax
of `for` is

```racket
(for ([id sequence-expr] ...)
  body ...+)                 
```

A `for` loop iterates through the sequence produced by the
`sequence-expr`. For each element of the sequence, `for` binds the
element to `id`, and then it evaluates the `body`s for side effects.

Examples:

```racket
> (for ([i '(1 2 3)])  
    (display i))       
123                    
> (for ([i "abc"])     
    (printf "~a..." i))
a...b...c...           
> (for ([i 4])         
    (display i))       
0123                   
```

The `for/list` variant of `for` is more Racket-like. It accumulates
`body` results into a list, instead of evaluating `body` only for side
effects. In more technical terms, `for/list` implements a _list
comprehension_.

Examples:

```racket
> (for/list ([i '(1 2 3)])
    (* i i))              
'(1 4 9)                  
> (for/list ([i "abc"])   
    i)                    
'(#\a #\b #\c)            
> (for/list ([i 4])       
    i)                    
'(0 1 2 3)                
```

The full syntax of `for` accommodates multiple sequences to iterate in
parallel, and the `for*` variant nests the iterations instead of running
them in parallel. More variants of `for` and `for*` accumulate `body`
results in different ways.  In all of these variants, predicates that
prune iterations can be included along with bindings.

Before details on the variations of `for`, though, it’s best to see the
kinds of sequence generators that make interesting examples.

## 1. Sequence Constructors

The `in-range` function generates a sequence of numbers, given an
optional starting number \(which defaults to `0`\), a number before
which the sequence ends, and an optional step \(which defaults to `1`\).
Using a non-negative integer `k` directly as a sequence is a shorthand
for `(in-range k)`.

Examples:

```racket
> (for ([i 3])                 
    (display i))               
012                            
> (for ([i (in-range 3)])      
    (display i))               
012                            
> (for ([i (in-range 1 4)])    
    (display i))               
123                            
> (for ([i (in-range 1 4 2)])  
    (display i))               
13                             
> (for ([i (in-range 4 1 -1)]) 
    (display i))               
432                            
> (for ([i (in-range 1 4 1/2)])
    (printf " ~a " i))         
 1  3/2  2  5/2  3  7/2        
```

The `in-naturals` function is similar, except that the starting number
must be an exact non-negative integer \(which defaults to `0`\), the
step is always `1`, and there is no upper limit. A `for` loop using just
`in-naturals` will never terminate unless a body expression raises an
exception or otherwise escapes.

Example:

```racket
> (for ([i (in-naturals)]) 
    (if (= i 10)           
        (error "too much!")
        (display i)))      
0123456789                 
too much!                  
```

The `stop-before` and `stop-after` functions construct a new sequence
given a sequence and a predicate. The new sequence is like the given
sequence, but truncated either immediately before or immediately after
the first element for which the predicate returns true.

Example:

```racket
> (for ([i (stop-before "abc def"          
                        char-whitespace?)])
    (display i))                           
abc                                        
```

Sequence constructors like `in-list`, `in-vector` and `in-string` simply
make explicit the use of a list, vector, or string as a sequence. Along
with `in-range`, these constructors raise an exception when given the
wrong kind of value, and since they otherwise avoid a run-time dispatch
to determine the sequence type, they enable more efficient code
generation; see Iteration Performance for more information.

Examples:

```racket
> (for ([i (in-string "abc")])   
    (display i))                 
abc                              
> (for ([i (in-string '(1 2 3))])
    (display i))                 
in-string: contract violation    
  expected: string               
  given: '(1 2 3)                
```

> +\[missing\] in \[missing\] provides more on sequences.

## 2. `for` and `for*`

A more complete syntax of `for` is

```racket
(for (clause ...)             
  body ...+)                  
                              
clause = [id sequence-expr]   
       | #:when boolean-expr  
       | #:unless boolean-expr
```

When multiple `[id sequence-expr]` clauses are provided in a `for` form,
the corresponding sequences are traversed in parallel:

```racket
> (for ([i (in-range 1 4)]                          
        [chapter '("Intro" "Details" "Conclusion")])
    (printf "Chapter ~a. ~a\n" i chapter))          
Chapter 1. Intro                                    
Chapter 2. Details                                  
Chapter 3. Conclusion                               
```

With parallel sequences, the `for` expression stops iterating when any
sequence ends. This behavior allows `in-naturals`, which creates an
infinite sequence of numbers, to be used for indexing:

```racket
> (for ([i (in-naturals 1)]                         
        [chapter '("Intro" "Details" "Conclusion")])
    (printf "Chapter ~a. ~a\n" i chapter))          
Chapter 1. Intro                                    
Chapter 2. Details                                  
Chapter 3. Conclusion                               
```

The `for*` form, which has the same syntax as `for`, nests multiple
sequences instead of running them in parallel:

```racket
> (for* ([book '("Guide" "Reference")]               
         [chapter '("Intro" "Details" "Conclusion")])
    (printf "~a ~a\n" book chapter))                 
Guide Intro                                          
Guide Details                                        
Guide Conclusion                                     
Reference Intro                                      
Reference Details                                    
Reference Conclusion                                 
```

Thus, `for*` is a shorthand for nested `for`s in the same way that
`let*` is a shorthand for nested `let`s.

The `#:when boolean-expr` form of a `clause` is another shorthand. It
allows the `body`s to evaluate only when the `boolean-expr` produces a
true value:

```racket
> (for* ([book '("Guide" "Reference")]              
         [chapter '("Intro" "Details" "Conclusion")]
         #:when (not (equal? chapter "Details")))   
    (printf "~a ~a\n" book chapter))                
Guide Intro                                         
Guide Conclusion                                    
Reference Intro                                     
Reference Conclusion                                
```

A `boolean-expr` with `#:when` can refer to any of the preceding
iteration bindings. In a `for` form, this scoping makes sense only if
the test is nested in the iteration of the preceding bindings; thus,
bindings separated by `#:when` are mutually nested, instead of in
parallel, even with `for`.

```racket
> (for ([book '("Guide" "Reference" "Notes")]              
        #:when (not (equal? book "Notes"))                 
        [i (in-naturals 1)]                                
        [chapter '("Intro" "Details" "Conclusion" "Index")]
        #:when (not (equal? chapter "Index")))             
    (printf "~a Chapter ~a. ~a\n" book i chapter))         
Guide Chapter 1. Intro                                     
Guide Chapter 2. Details                                   
Guide Chapter 3. Conclusion                                
Reference Chapter 1. Intro                                 
Reference Chapter 2. Details                               
Reference Chapter 3. Conclusion                            
```

An `#:unless` clause is analogus to a `#:when` clause, but the `body`s
evaluate only when the `boolean-expr` produces a false value.

## 3. `for/list` and `for*/list`

The `for/list` form, which has the same syntax as `for`, evaluates the
`body`s to obtain values that go into a newly constructed list:

```racket
> (for/list ([i (in-naturals 1)]                         
             [chapter '("Intro" "Details" "Conclusion")])
    (string-append (number->string i) ". " chapter))     
'("1. Intro" "2. Details" "3. Conclusion")               
```

A `#:when` clause in a `for-list` form prunes the result list along with
evaluations of the `body`s:

```racket
> (for/list ([i (in-naturals 1)]                        
             [chapter '("Intro" "Details" "Conclusion")]
             #:when (odd? i))                           
    chapter)                                            
'("Intro" "Conclusion")                                 
```

This pruning behavior of `#:when` is more useful with `for/list` than
`for`. Whereas a plain `when` form normally suffices with `for`, a
`when` expression form in a `for/list` would cause the result list to
contain `#<void>`s instead of omitting list elements.

The `for*/list` form is like `for*`, nesting multiple iterations:

```racket
> (for*/list ([book '("Guide" "Ref.")]                      
              [chapter '("Intro" "Details")])               
    (string-append book " " chapter))                       
'("Guide Intro" "Guide Details" "Ref. Intro" "Ref. Details")
```

A `for*/list` form is not quite the same thing as nested `for/list`
forms. Nested `for/list`s would produce a list of lists, instead of one
flattened list. Much like `#:when`, then, the nesting of `for*/list` is
more useful than the nesting of `for*`.

## 4. `for/vector` and `for*/vector`

The `for/vector` form can be used with the same syntax as the `for/list`
form, but the evaluated `body`s go into a newly-constructed vector
instead of a list:

```racket
> (for/vector ([i (in-naturals 1)]                         
               [chapter '("Intro" "Details" "Conclusion")])
    (string-append (number->string i) ". " chapter))       
'#("1. Intro" "2. Details" "3. Conclusion")                
```

The `for*/vector` form behaves similarly, but the iterations are nested
as in `for*`.

The `for/vector` and `for*/vector` forms also allow the length of the
vector to be constructed to be supplied in advance.  The resulting
iteration can be performed more efficiently than plain `for/vector` or
`for*/vector`:

```racket
> (let ([chapters '("Intro" "Details" "Conclusion")])          
    (for/vector #:length (length chapters) ([i (in-naturals 1)]
                                            [chapter chapters])
      (string-append (number->string i) ". " chapter)))        
'#("1. Intro" "2. Details" "3. Conclusion")                    
```

If a length is provided, the iteration stops when the vector is filled
or the requested iterations are complete, whichever comes first.  If the
provided length exceeds the requested number of iterations, then the
remaining slots in the vector are initialized to the default argument of
`make-vector`.

## 5. `for/and` and `for/or`

The `for/and` form combines iteration results with `and`, stopping as
soon as it encounters `#f`:

```racket
> (for/and ([chapter '("Intro" "Details" "Conclusion")])
    (equal? chapter "Intro"))                           
#f                                                      
```

The `for/or` form combines iteration results with `or`, stopping as soon
as it encounters a true value:

```racket
> (for/or ([chapter '("Intro" "Details" "Conclusion")])
    (equal? chapter "Intro"))                          
#t                                                     
```

As usual, the `for*/and` and `for*/or` forms provide the same facility
with nested iterations.

## 6. `for/first` and `for/last`

The `for/first` form returns the result of the first time that the
`body`s are evaluated, skipping further iterations. This form is most
useful with a `#:when` clause.

```racket
> (for/first ([chapter '("Intro" "Details" "Conclusion" "Index")]
              #:when (not (equal? chapter "Intro")))             
    chapter)                                                     
"Details"                                                        
```

If the `body`s are evaluated zero times, then the result is `#f`.

The `for/last` form runs all iterations, returning the value of the last
iteration \(or `#f` if no iterations are run\):

```racket
> (for/last ([chapter '("Intro" "Details" "Conclusion" "Index")]
              #:when (not (equal? chapter "Index")))            
    chapter)                                                    
"Conclusion"                                                    
```

As usual, the `for*/first` and `for*/last` forms provide the same
facility with nested iterations:

```racket
> (for*/first ([book '("Guide" "Reference")]                      
               [chapter '("Intro" "Details" "Conclusion" "Index")]
               #:when (not (equal? chapter "Intro")))             
    (list book chapter))                                          
'("Guide" "Details")                                              
> (for*/last ([book '("Guide" "Reference")]                       
              [chapter '("Intro" "Details" "Conclusion" "Index")] 
              #:when (not (equal? chapter "Index")))              
    (list book chapter))                                          
'("Reference" "Conclusion")                                       
```

## 7. `for/fold` and `for*/fold`

The `for/fold` form is a very general way to combine iteration results.
Its syntax is slightly different than the syntax of `for`, because
accumulation variables must be declared at the beginning:

```racket
(for/fold ([accum-id init-expr] ...)
          (clause ...)              
  body ...+)                        
```

In the simple case, only one `[accum-id init-expr]` is provided, and the
result of the `for/fold` is the final value for `accum-id`, which starts
out with the value of `init-expr`. In the `clause`s and `body`s,
`accum-id` can be referenced to get its current value, and the last
`body` provides the value of `accum-id` for the next iteration.

Examples:

```racket
> (for/fold ([len 0])                                             
            ([chapter '("Intro" "Conclusion")])                   
    (+ len (string-length chapter)))                              
15                                                                
> (for/fold ([prev #f])                                           
            ([i (in-naturals 1)]                                  
             [chapter '("Intro" "Details" "Details" "Conclusion")]
             #:when (not (equal? chapter prev)))                  
    (printf "~a. ~a\n" i chapter)                                 
    chapter)                                                      
1. Intro                                                          
2. Details                                                        
4. Conclusion                                                     
"Conclusion"                                                      
```

When multiple `accum-id`s are specified, then the last `body` must
produce multiple values, one for each `accum-id`. The `for/fold`
expression itself produces multiple values for the results.

Example:

```racket
> (for/fold ([prev #f]                                            
             [counter 1])                                         
            ([chapter '("Intro" "Details" "Details" "Conclusion")]
             #:when (not (equal? chapter prev)))                  
    (printf "~a. ~a\n" counter chapter)                           
    (values chapter                                               
            (add1 counter)))                                      
1. Intro                                                          
2. Details                                                        
3. Conclusion                                                     
"Conclusion"                                                      
4                                                                 
```

## 8. Multiple-Valued Sequences

In the same way that a function or expression can produce multiple
values, individual iterations of a sequence can produce multiple
elements.  For example, a hash table as a sequence generates two values
for each iteration: a key and a value.

In the same way that `let-values` binds multiple results to multiple
identifiers, `for` can bind multiple sequence elements to multiple
iteration identifiers:

> While `let` must be changed to `let-values` to bind multiple
> identifiers, `for` simply allows a parenthesized list of identifiers
> instead of a single identifier in any clause.

```racket
> (for ([(k v) #hash(("apple" . 1) ("banana" . 3))])
    (printf "~a count: ~a\n" k v))                  
banana count: 3                                     
apple count: 1                                      
```

This extension to multiple-value bindings works for all `for` variants.
For example, `for*/list` nests iterations, builds a list, and also works
with multiple-valued sequences:

```racket
> (for*/list ([(k v) #hash(("apple" . 1) ("banana" . 3))]
              [(i) (in-range v)])                        
    k)                                                   
'("banana" "banana" "banana" "apple")                    
```

## 9. Breaking an Iteration

An even more complete syntax of `for` is

```racket
(for (clause ...)                    
  body-or-break ... body)            
                                     
clause        = [id sequence-expr]   
              | #:when boolean-expr  
              | #:unless boolean-expr
              | break                
                                     
body-or-break = body                 
              | break                
                                     
break         = #:break boolean-expr 
              | #:final boolean-expr 
```

That is, a `#:break` or `#:final` clause can be included among the
binding clauses and body of the iteration. Among the binding clauses,
`#:break` is like `#:unless` but when its `boolean-expr` is true, all
sequences within the `for` are stopped. Among the `body`s, `#:break` has
the same effect on sequences when its `boolean-expr` is true, and it
also prevents later `body`s from evaluation in the current iteration.

For example, while using `#:unless` between clauses effectively skips
later sequences as well as the body,

```racket
> (for ([book '("Guide" "Story" "Reference")]       
        #:unless (equal? book "Story")              
        [chapter '("Intro" "Details" "Conclusion")])
    (printf "~a ~a\n" book chapter))                
Guide Intro                                         
Guide Details                                       
Guide Conclusion                                    
Reference Intro                                     
Reference Details                                   
Reference Conclusion                                
```

using `#:break` causes the entire `for` iteration to terminate:

```racket
> (for ([book '("Guide" "Story" "Reference")]        
        #:break (equal? book "Story")                
        [chapter '("Intro" "Details" "Conclusion")]) 
    (printf "~a ~a\n" book chapter))                 
Guide Intro                                          
Guide Details                                        
Guide Conclusion                                     
> (for* ([book '("Guide" "Story" "Reference")]       
         [chapter '("Intro" "Details" "Conclusion")])
    #:break (and (equal? book "Story")               
                 (equal? chapter "Conclusion"))      
    (printf "~a ~a\n" book chapter))                 
Guide Intro                                          
Guide Details                                        
Guide Conclusion                                     
Story Intro                                          
Story Details                                        
```

A `#:final` clause is similar to `#:break`, but it does not immediately
terminate the iteration. Instead, it allows at most one more element to
be drawn for each sequence and at most one more evaluation of the
`body`s.

```racket
> (for* ([book '("Guide" "Story" "Reference")]       
         [chapter '("Intro" "Details" "Conclusion")])
    #:final (and (equal? book "Story")               
                 (equal? chapter "Conclusion"))      
    (printf "~a ~a\n" book chapter))                 
Guide Intro                                          
Guide Details                                        
Guide Conclusion                                     
Story Intro                                          
Story Details                                        
Story Conclusion                                     
> (for ([book '("Guide" "Story" "Reference")]        
        #:final (equal? book "Story")                
        [chapter '("Intro" "Details" "Conclusion")]) 
    (printf "~a ~a\n" book chapter))                 
Guide Intro                                          
Guide Details                                        
Guide Conclusion                                     
Story Intro                                          
```

## 10. Iteration Performance

Ideally, a `for` iteration should run as fast as a loop that you write
by hand as a recursive-function invocation. A hand-written loop,
however, is normally specific to a particular kind of data, such as
lists. In that case, the hand-written loop uses selectors like `car` and
`cdr` directly, instead of handling all forms of sequences and
dispatching to an appropriate iterator.

The `for` forms can provide the performance of hand-written loops when
enough information is apparent about the sequences to iterate.
Specifically, the clause should have one of the following `fast-clause`
forms:

  `fast-clause`` `=` ``[id` `fast-seq]`                 
` `            ` `|` ``[(id)` `fast-seq]`               
` `            ` `|` ``[(id` `id)` `fast-indexed-seq]`  
` `            ` `|` ``[(id` `...)` `fast-parallel-seq]`

  `fast-seq`` `=` ``(in-range` `expr)`                        
` `         ` `|` ``(in-range` `expr` `expr)`                 
` `         ` `|` ``(in-range` `expr` `expr` `expr)`          
` `         ` `|` ``(in-naturals)`                            
` `         ` `|` ``(in-naturals` `expr)`                     
` `         ` `|` ``(in-list` `expr)`                         
` `         ` `|` ``(in-vector` `expr)`                       
` `         ` `|` ``(in-string` `expr)`                       
` `         ` `|` ``(in-bytes` `expr)`                        
` `         ` `|` ``(in-value` `expr)`                        
` `         ` `|` ``(stop-before` `fast-seq` `predicate-expr)`
` `         ` `|` ``(stop-after` `fast-seq` `predicate-expr)` 

  `fast-indexed-seq`` `=` ``(in-indexed` `fast-seq)`                          
` `                 ` `|` ``(stop-before` `fast-indexed-seq` `predicate-expr)`
` `                 ` `|` ``(stop-after` `fast-indexed-seq` `predicate-expr)` 

  `fast-parallel-seq`` `=` ``(in-parallel` `fast-seq` `...)`                    
` `                  ` `|` ``(stop-before` `fast-parallel-seq` `predicate-expr)`
` `                  ` `|` ``(stop-after` `fast-parallel-seq` `predicate-expr)` 

Examples:

```racket
> (time (for ([i (in-range 100000)])                         
          (for ([elem (in-list '(a b c d e f g h))]) ; fast  
            (void))))                                        
cpu time: 2 real time: 2 gc time: 0                          
> (time (for ([i (in-range 100000)])                         
          (for ([elem '(a b c d e f g h)])           ; slower
            (void))))                                        
cpu time: 38 real time: 39 gc time: 0                        
> (time (let ([seq (in-list '(a b c d e f g h))])            
          (for ([i (in-range 100000)])                       
            (for ([elem seq])                        ; slower
              (void)))))                                     
cpu time: 45 real time: 44 gc time: 0                        
```

The grammars above are not complete, because the set of syntactic
patterns that provide good performance is extensible, just like the set
of sequence values. The documentation for a sequence constructor should
indicate the performance benefits of using it directly in a `for`
`clause`.

> +\[missing\] in \[missing\] provides more on iterations and
> comprehensions.
