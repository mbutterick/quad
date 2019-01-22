# Pattern-Based Macros

A _pattern-based macro_ replaces any code that matches a pattern to an
expansion that uses parts of the original syntax that match parts of the
pattern.

## 1. `define-syntax-rule`

The simplest way to create a macro is to use `define-syntax-rule`:

```racket
(define-syntax-rule pattern template)
```

As a running example, consider the `swap` macro, which swaps the values
stored in two variables. It can be implemented using
`define-syntax-rule` as follows:

> The macro is “un-Rackety” in the sense that it involves side effects on
> variables—but the point of macros is to let you add syntactic forms that
> some other language designer might not approve.

```racket
(define-syntax-rule (swap x y)
  (let ([tmp x])              
    (set! x y)                
    (set! y tmp)))            
```

The `define-syntax-rule` form binds a macro that matches a single
pattern. The pattern must always start with an open parenthesis followed
by an identifier, which is `swap` in this case. After the initial
identifier, other identifiers are _macro pattern variables_ that can
match anything in a use of the macro. Thus, this macro matches the form
`(swap form1 form2)` for any `form1` and `form2`.

> Macro pattern variables are similar to pattern variables for `match`.
> See \[missing\].

After the pattern in `define-syntax-rule` is the _template_. The
template is used in place of a form that matches the pattern, except
that each instance of a pattern variable in the template is replaced
with the part of the macro use the pattern variable matched. For
example, in

`(swap` `first` `last)`

the pattern variable `x` matches `first` and `y` matches `last`, so that
the expansion is

```racket
(let ([tmp first]) 
  (set! first last)
  (set! last tmp)) 
```

## 2. Lexical Scope

Suppose that we use the `swap` macro to swap variables named `tmp` and
`other`:

```racket
(let ([tmp 5]      
      [other 6])   
  (swap tmp other) 
  (list tmp other))
```

The result of the above expression should be `(6 5)`. The naive
expansion of this use of `swap`, however, is

```racket
(let ([tmp 5]        
      [other 6])     
  (let ([tmp tmp])   
    (set! tmp other) 
    (set! other tmp))
  (list tmp other))  
```

whose result is `(5 6)`. The problem is that the naive expansion
confuses the `tmp` in the context where `swap` is used with the `tmp`
that is in the macro template.

Racket doesn’t produce the naive expansion for the above use of `swap`.
Instead, it produces

```racket
(let ([tmp 5]          
      [other 6])       
  (let ([tmp_1 tmp])   
    (set! tmp other)   
    (set! other tmp_1))
  (list tmp other))    
```

with the correct result in `(6 5)`. Similarly, in the example

```racket
(let ([set! 5]      
      [other 6])    
  (swap set! other) 
  (list set! other))
```

the expansion is

```racket
(let ([set!_1 5]       
      [other 6])       
  (let ([tmp_1 set!_1])
    (set! set!_1 other)
    (set! other tmp_1))
  (list set!_1 other)) 
```

so that the local `set!` binding doesn’t interfere with the assignments
introduced by the macro template.

In other words, Racket’s pattern-based macros automatically maintain
lexical scope, so macro implementors can reason about variable reference
in macros and macro uses in the same way as for functions and function
calls.

## 3. `define-syntax` and `syntax-rules`

The `define-syntax-rule` form binds a macro that matches a single
pattern, but Racket’s macro system supports transformers that match
multiple patterns starting with the same identifier. To write such
macros, the programmer must use the more general `define-syntax` form
along with the `syntax-rules` transformer form:

```racket
(define-syntax id               
  (syntax-rules (literal-id ...)
    [pattern template]          
    ...))                       
```

> The `define-syntax-rule` form is itself a macro that expands into
> `define-syntax` with a `syntax-rules` form that contains only one
> pattern and template.

For example, suppose we would like a `rotate` macro that generalizes
`swap` to work on either two or three identifiers, so that

```racket
(let ([red 1] [green 2] [blue 3])       
  (rotate red green)      ; swaps       
  (rotate red green blue) ; rotates left
  (list red green blue))                
```

produces `(1 3 2)`. We can implement `rotate` using `syntax-rules`:

```racket
(define-syntax rotate              
  (syntax-rules ()                 
    [(rotate a b) (swap a b)]      
    [(rotate a b c) (begin         
                     (swap a b)    
                     (swap b c))]))
```

The expression `(rotate red green)` matches the first pattern in the
`syntax-rules` form, so it expands to `(swap red green)`. The expression
`(rotate red green blue)` matches the second pattern, so it expands to
`(begin (swap red green) (swap green blue))`.

## 4. Matching Sequences

A better `rotate` macro would allow any number of identifiers, instead
of just two or three. To match a use of `rotate` with any number of
identifiers, we need a pattern form that has something like a Kleene
star. In a Racket macro pattern, a star is written as `...`.

To implement `rotate` with `...`, we need a base case to handle a single
identifier, and an inductive case to handle more than one identifier:

```racket
(define-syntax rotate                         
  (syntax-rules ()                            
    [(rotate a) (void)]                       
    [(rotate a b c ...) (begin                
                          (swap a b)          
                          (rotate b c ...))]))
```

When a pattern variable like `c` is followed by `...` in a pattern, then
it must be followed by `...` in a template, too. The pattern variable
effectively matches a sequence of zero or more forms, and it is replaced
in the template by the same sequence.

Both versions of `rotate` so far are a bit inefficient, since pairwise
swapping keeps moving the value from the first variable into every
variable in the sequence until it arrives at the last one. A more
efficient `rotate` would move the first value directly to the last
variable. We can use `...` patterns to implement the more efficient
variant using a helper macro:

```racket
(define-syntax rotate                        
  (syntax-rules ()                           
    [(rotate a c ...)                        
     (shift-to (c ... a) (a c ...))]))       
                                             
(define-syntax shift-to                      
  (syntax-rules ()                           
    [(shift-to (from0 from ...) (to0 to ...))
     (let ([tmp from0])                      
       (set! to from) ...                    
       (set! to0 tmp))]))                    
```

In the `shift-to` macro, `...` in the template follows `(set! to from)`,
which causes the `(set! to from)` expression to be duplicated as many
times as necessary to use each identifier matched in the `to` and `from`
sequences. \(The number of `to` and `from` matches must be the same,
otherwise the macro expansion fails with an error.\)

## 5. Identifier Macros

Given our macro definitions, the `swap` or `rotate` identifiers must be
used after an open parenthesis, otherwise a syntax error is reported:

```racket
> (+ swap 3)              
eval:2:0: swap: bad syntax
  in: swap                
```

An _identifier macro_ is a pattern-matching macro that works when used
by itself without parentheses. For example, we can define `val` as an
identifier macro that expands to `(get-val)`, so `(+ val 3)` would
expand to `(+ (get-val) 3)`.

```racket
> (define-syntax val                                          
    (lambda (stx)                                             
      (syntax-case stx ()                                     
        [val (identifier? (syntax val)) (syntax (get-val))])))
> (define-values (get-val put-val!)                           
    (let ([private-val 0])                                    
      (values (lambda () private-val)                         
              (lambda (v) (set! private-val v)))))            
> val                                                         
0                                                             
> (+ val 3)                                                   
3                                                             
```

The `val` macro uses `syntax-case`, which enables defining more powerful
macros and will be explained in the \[missing\] section. For now it is
sufficient to know that to define a macro, `syntax-case` is used in a
`lambda`, and its templates must be wrapped with an explicit `syntax`
constructor. Finally, `syntax-case` clauses may specify additional guard
conditions after the pattern.

Our `val` macro uses an `identifier?` condition to ensure that `val`
_must not_ be used with parentheses. Instead, the macro raises a syntax
error:

```racket
> (val)                  
eval:8:0: val: bad syntax
  in: (val)              
```

## 6. `set!` Transformers

With the above `val` macro, we still must call `put-val!` to change the
stored value. It would be more convenient, however, to use `set!`
directly on `val`. To invoke the macro when `val` is used with `set!`,
we create an assignment transformer with `make-set!-transformer`. We
must also declare `set!` as a literal in the `syntax-case` literal list.

```racket
> (define-syntax val2                                         
    (make-set!-transformer                                    
     (lambda (stx)                                            
       (syntax-case stx (set!)                                
         [val2 (identifier? (syntax val2)) (syntax (get-val))]
         [(set! val2 e) (syntax (put-val! e))]))))            
> val2                                                        
0                                                             
> (+ val2 3)                                                  
3                                                             
> (set! val2 10)                                              
> val2                                                        
10                                                            
```

## 7. Macro-Generating Macros

Suppose that we have many identifiers like `val` and `val2` that we’d
like to redirect to accessor and mutator functions like `get-val` and
`put-val!`. We’d like to be able to just write:

`(define-get/put-id` `val` `get-val` `put-val!)`

Naturally, we can implement `define-get/put-id` as a macro:

```racket
> (define-syntax-rule (define-get/put-id id get put!)   
    (define-syntax id                                   
      (make-set!-transformer                            
       (lambda (stx)                                    
         (syntax-case stx (set!)                        
           [id (identifier? (syntax id)) (syntax (get))]
           [(set! id e) (syntax (put! e))])))))         
> (define-get/put-id val3 get-val put-val!)             
> (set! val3 11)                                        
> val3                                                  
11                                                      
```

The `define-get/put-id` macro is a _macro-generating macro_.

## 8. Extended Example: Call-by-Reference Functions

We can use pattern-matching macros to add a form to Racket for defining
first-order _call-by-reference_ functions. When a call-by-reference
function body mutates its formal argument, the mutation applies to
variables that are supplied as actual arguments in a call to the
function.

For example, if `define-cbr` is like `define` except that it defines a
call-by-reference function, then

```racket
(define-cbr (f a b)
  (swap a b))      
                   
(let ([x 1] [y 2]) 
  (f x y)          
  (list x y))      
```

produces `(2 1)`.

We will implement call-by-reference functions by having function calls
supply accessor and mutators for the arguments, instead of supplying
argument values directly. In particular, for the function `f` above,
we’ll generate

```racket
(define (do-f get-a get-b put-a! put-b!)
  (define-get/put-id a get-a put-a!)    
  (define-get/put-id b get-b put-b!)    
  (swap a b))                           
```

and redirect a function call `(f x y)` to

```racket
(do-f (lambda () x)           
      (lambda () y)           
      (lambda (v) (set! x v)) 
      (lambda (v) (set! y v)))
```

Clearly, then `define-cbr` is a macro-generating macro, which binds `f`
to a macro that expands to a call of `do-f`. That is, `(define-cbr (f a
b) (swap a b))` needs to generate the definition

```racket
(define-syntax f             
  (syntax-rules ()           
    [(id actual ...)         
     (do-f (lambda () actual)
           ...               
           (lambda (v)       
             (set! actual v))
           ...)]))           
```

At the same time, `define-cbr` needs to define `do-f` using the body of
`f`, this second part is slightly more complex, so we defer most of it
to a `define-for-cbr` helper module, which lets us write `define-cbr`
easily enough:

```racket
(define-syntax-rule (define-cbr (id arg ...) body)
  (begin                                          
    (define-syntax id                             
      (syntax-rules ()                            
        [(id actual (... ...))                    
         (do-f (lambda () actual)                 
               (... ...)                          
               (lambda (v)                        
                 (set! actual v))                 
               (... ...))]))                      
    (define-for-cbr do-f (arg ...)                
      () ; explained below...                     
      body)))                                     
```

Our remaining task is to define `define-for-cbr` so that it converts

`(define-for-cbr` `do-f` `(a` `b)` `()` `(swap` `a` `b))`

to the function definition `do-f` above. Most of the work is generating
a `define-get/put-id` declaration for each argument, `a` and `b`, and
putting them before the body. Normally, that’s an easy task for `...` in
a pattern and template, but this time there’s a catch: we need to
generate the names `get-a` and `put-a!` as well as `get-b` and `put-b!`,
and the pattern language provides no way to synthesize identifiers based
on existing identifiers.

As it turns out, lexical scope gives us a way around this problem. The
trick is to iterate expansions of `define-for-cbr` once for each
argument in the function, and that’s why `define-for-cbr` starts with an
apparently useless `()` after the argument list. We need to keep track
of all the arguments seen so far and the `get` and `put` names generated
for each, in addition to the arguments left to process. After we’ve
processed all the identifiers, then we have all the names we need.

Here is the definition of `define-for-cbr`:

```racket
(define-syntax define-for-cbr            
  (syntax-rules ()                       
    [(define-for-cbr do-f (id0 id ...)   
       (gens ...) body)                  
     (define-for-cbr do-f (id ...)       
       (gens ... (id0 get put)) body)]   
    [(define-for-cbr do-f ()             
       ((id get put) ...) body)          
     (define (do-f get ... put ...)      
       (define-get/put-id id get put) ...
       body)]))                          
```

Step-by-step, expansion proceeds as follows:

```racket
(define-for-cbr do-f (a b)                        
  () (swap a b))                                  
=> (define-for-cbr do-f (b)                       
     ([a get_1 put_1]) (swap a b))                
=> (define-for-cbr do-f ()                        
     ([a get_1 put_1] [b get_2 put_2]) (swap a b))
=> (define (do-f get_1 get_2 put_1 put_2)         
     (define-get/put-id a get_1 put_1)            
     (define-get/put-id b get_2 put_2)            
     (swap a b))                                  
```

The “subscripts” on `get_1`, `get_2`, `put_1`, and `put_2` are inserted
by the macro expander to preserve lexical scope, since the `get`
generated by each iteration of `define-for-cbr` should not bind the
`get` generated by a different iteration. In other words, we are
essentially tricking the macro expander into generating fresh names for
us, but the technique illustrates some of the surprising power of
pattern-based macros with automatic lexical scope.

The last expression eventually expands to just

```racket
(define (do-f get_1 get_2 put_1 put_2)
  (let ([tmp (get_1)])                
    (put_1 (get_2))                   
    (put_2 tmp)))                     
```

which implements the call-by-name function `f`.

To summarize, then, we can add call-by-reference functions to Racket
with just three small pattern-based macros: `define-cbr`,
`define-for-cbr`, and `define-get/put-id`.
