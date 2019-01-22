# Expressions and Definitions

The \[missing\] chapter introduced some of Racket’s syntactic forms:
definitions, procedure applications, conditionals, and so on. This
section provides more details on those forms, plus a few additional
basic forms.

    1 Notation                                                         
                                                                       
    2 Identifiers and Binding                                          
                                                                       
    3 Function Calls \(Procedure Applications\)                        
      3.1 Evaluation Order and Arity                                   
      3.2 Keyword Arguments                                            
      3.3 The `apply` Function                                         
                                                                       
    4 Functions \(Procedures\): `lambda`                               
      4.1 Declaring a Rest Argument                                    
      4.2 Declaring Optional Arguments                                 
      4.3 Declaring Keyword Arguments                                  
      4.4 Arity-Sensitive Functions: `case-lambda`                     
                                                                       
    5 Definitions: `define`                                            
      5.1 Function Shorthand                                           
      5.2 Curried Function Shorthand                                   
      5.3 Multiple Values and `define-values`                          
      5.4 Internal Definitions                                         
                                                                       
    6 Local Binding                                                    
      6.1 Parallel Binding: `let`                                      
      6.2 Sequential Binding: `let*`                                   
      6.3 Recursive Binding: `letrec`                                  
      6.4 Named `let`                                                  
      6.5 Multiple Values: `let-values`, `let*-values`, `letrec-values`
                                                                       
    7 Conditionals                                                     
      7.1 Simple Branching: `if`                                       
      7.2 Combining Tests: `and` and `or`                              
      7.3 Chaining Tests: `cond`                                       
                                                                       
    8 Sequencing                                                       
      8.1 Effects Before: `begin`                                      
      8.2 Effects After: `begin0`                                      
      8.3 Effects If...: `when` and `unless`                           
                                                                       
    9 Assignment: `set!`                                               
      9.1 Guidelines for Using Assignment                              
      9.2 Multiple Values: `set!-values`                               
                                                                       
    10 Quoting: `quote` and `'`                                        
                                                                       
    11 Quasiquoting: `quasiquote` and `‘`                              
                                                                       
    12 Simple Dispatch: `case`                                         
                                                                       
    13 Dynamic Binding: `parameterize`                                 

## 1. Notation

This chapter \(and the rest of the documentation\) uses a slightly
different notation than the character-based grammars of the \[missing\]
chapter. The grammar for a use of a syntactic form `something` is shown
like this:

```racket
(something [id ...+] an-expr ...)
```

The italicized meta-variables in this specification, such as `id` and
`an-expr`, use the syntax of Racket identifiers, so `an-expr` is one
meta-variable. A naming convention implicitly defines the meaning of
many meta-variables:

* A meta-variable that ends in `id` stands for an identifier, such as
  `x` or `my-favorite-martian`.

* A meta-identifier that ends in `keyword` stands for a keyword, such as
  `#:tag`.

* A meta-identifier that ends with `expr` stands for any sub-form, and
  it will be parsed as an expression.

* A meta-identifier that ends with `body` stands for any sub-form; it
  will be parsed as either a local definition or an expression. A `body`
  can parse as a definition only if it is not preceded by any
  expression, and the last `body` must be an expression; see also
  Internal Definitions.

Square brackets in the grammar indicate a parenthesized sequence of
forms, where square brackets are normally used \(by convention\). That
is, square brackets _do not_ mean optional parts of the syntactic form.

A `...` indicates zero or more repetitions of the preceding form, and
`...+` indicates one or more repetitions of the preceding datum.
Otherwise, non-italicized identifiers stand for themselves.

Based on the above grammar, then, here are a few conforming uses of
`something`:

```racket
(something [x])                                 
(something [x] (+ 1 2))                         
(something [x my-favorite-martian x] (+ 1 2) #f)
```

Some syntactic-form specifications refer to meta-variables that are not
implicitly defined and not previously defined. Such meta-variables are
defined after the main form, using a BNF-like format for alternatives:

```racket
(something-else [thing ...+] an-expr ...)
                                         
thing = thing-id                         
      | thing-keyword                    
```

The above example says that, within a `something-else` form, a `thing`
is either an identifier or a keyword.

## 2. Identifiers and Binding

The context of an expression determines the meaning of identifiers that
appear in the expression. In particular, starting a module with the
language `racket`, as in

`#lang` `racket`

means that, within the module, the identifiers described in this guide
start with the meaning described here: `cons` refers to the function
that creates a pair, `car` refers to the function that extracts the
first element of a pair, and so on.

> +\[missing\] introduces the syntax of identifiers.

Forms like `define`, `lambda`, and `let` associate a meaning with one or
more identifiers; that is, they _bind_ identifiers. The part of the
program for which the binding applies is the _scope_ of the binding. The
set of bindings in effect for a given expression is the expression’s
_environment_.

For example, in

```racket
#lang racket    
                
(define f       
  (lambda (x)   
    (let ([y 5])
      (+ x y))))
                
(f 10)          
```

the `define` is a binding of `f`, the `lambda` has a binding for `x`,
and the `let` has a binding for `y`. The scope of the binding for `f` is
the entire module; the scope of the `x` binding is `(let ([y 5]) (+ x
y))`; and the scope of the `y` binding is just `(+ x y)`. The
environment of `(+ x y)` includes bindings for `y`, `x`, and `f`, as
well as everything in `racket`.

A module-level `define` can bind only identifiers that are not already
defined or `require`d into the module. A local `define` or other binding
forms, however, can give a new local binding for an identifier that
already has a binding; such a binding _shadows_ the existing binding.

Examples:

```racket
(define f                                    
  (lambda (append)                           
    (define cons (append "ugly" "confusing"))
    (let ([append 'this-was])                
      (list append cons))))                  
                                             
> (f list)                                   
'(this-was ("ugly" "confusing"))             
```

Similarly, a module-level `define` can shadow a binding from the
module’s language. For example, `(define cons 1)` in a `racket` module
shadows the `cons` that is provided by `racket`. Intentionally shadowing
a language binding is rarely a good idea—especially for widely used
bindings like `cons`—but shadowing relieves a programmer from having to
avoid every obscure binding that is provided by a language.

Even identifiers like `define` and `lambda` get their meanings from
bindings, though they have _transformer_ bindings \(which means that
they indicate syntactic forms\) instead of value bindings. Since
`define` has a transformer binding, the identifier `define` cannot be
used by itself to get a value. However, the normal binding for `define`
can be shadowed.

Examples:

```racket
> define                    
eval:1:0: define: bad syntax
  in: define                
> (let ([define 5]) define) 
5                           
```

Again, shadowing standard bindings in this way is rarely a good idea,
but the possibility is an inherent part of Racket’s flexibility.

## 3. Function Calls \(Procedure Applications\)

An expression of the form

```racket
(proc-expr arg-expr ...)
```

is a function call—also known as a _procedure application_—when
`proc-expr` is not an identifier that is bound as a syntax transformer
\(such as `if` or `define`\).

### 3.1. Evaluation Order and Arity

A function call is evaluated by first evaluating the `proc-expr` and all
`arg-expr`s in order \(left to right\). Then, if `proc-expr` produces a
function that accepts as many arguments as supplied `arg-expr`s, the
function is called. Otherwise, an exception is raised.

Examples:

```racket
> (cons 1 null)                                           
'(1)                                                      
> (+ 1 2 3)                                               
6                                                         
> (cons 1 2 3)                                            
cons: arity mismatch;                                     
 the expected number of arguments does not match the given
number                                                    
  expected: 2                                             
  given: 3                                                
  arguments...:                                           
   1                                                      
   2                                                      
   3                                                      
> (1 2 3)                                                 
application: not a procedure;                             
 expected a procedure that can be applied to arguments    
  given: 1                                                
  arguments...:                                           
   2                                                      
   3                                                      
```

Some functions, such as `cons`, accept a fixed number of arguments. Some
functions, such as `+` or `list`, accept any number of arguments. Some
functions accept a range of argument counts; for example `substring`
accepts either two or three arguments. A function’s _arity_ is the
number of arguments that it accepts.

### 3.2. Keyword Arguments

Some functions accept _keyword arguments_ in addition to by-position
arguments. For that case, an `arg` can be an `arg-keyword arg-expr`
sequence instead of just a `arg-expr`:

> +\[missing\] introduces keywords.

```racket
(proc-expr arg ...)       
                          
arg = arg-expr            
    | arg-keyword arg-expr
```

For example,

`(go` `"super.rkt"` `#:mode` `'fast)`

calls the function bound to `go` with `"super.rkt"` as a by-position
argument, and with `'fast` as an argument associated with the `#:mode`
keyword. A keyword is implicitly paired with the expression that follows
it.

Since a keyword by itself is not an expression, then

`(go` `"super.rkt"` `#:mode` `#:fast)`

is a syntax error. The `#:mode` keyword must be followed by an
expression to produce an argument value, and `#:fast` is not an
expression.

The order of keyword `arg`s determines the order in which `arg-expr`s
are evaluated, but a function accepts keyword arguments independent of
their position in the argument list. The above call to `go` can be
equivalently written

`(go` `#:mode` `'fast` `"super.rkt")`

> +\[missing\] in \[missing\] provides more on procedure applications.

### 3.3. The `apply` Function

The syntax for function calls supports any number of arguments, but a
specific call always specifies a fixed number of arguments. As a result,
a function that takes a list of arguments cannot directly apply a
function like `+` to all of the items in a list:

```racket                          
(define (avg lst) ; doesn’t work...
  (/ (+ lst) (length lst)))        
```                                
                                   
```racket                          
> (avg '(1 2 3))                   
+: contract violation              
  expected: number?                
  given: '(1 2 3)                  
```                                

```racket                                                  
(define (avg lst) ; doesn’t always work...                 
  (/ (+ (list-ref lst 0) (list-ref lst 1) (list-ref lst 2))
     (length lst)))                                        
```                                                        
                                                           
```racket                                                  
> (avg '(1 2 3))                                           
2                                                          
> (avg '(1 2))                                             
list-ref: index too large for list                         
  index: 2                                                 
  in: '(1 2)                                               
```                                                        

The `apply` function offers a way around this restriction. It takes a
function and a _list_ argument, and it applies the function to the
values in the list:

```racket                        
(define (avg lst)                
  (/ (apply + lst) (length lst)))
```                              
                                 
```racket                        
> (avg '(1 2 3))                 
2                                
> (avg '(1 2))                   
3/2                              
> (avg '(1 2 3 4))               
5/2                              
```                              

As a convenience, the `apply` function accepts additional arguments
between the function and the list. The additional arguments are
effectively `cons`ed onto the argument list:

```racket             
(define (anti-sum lst)
  (apply - 0 lst))    
```                   
                      
```racket             
> (anti-sum '(1 2 3)) 
-6                    
```                   

The `apply` function accepts keyword arguments, too, and it passes them
along to the called function:

```racket
(apply go #:mode 'fast '("super.rkt"))
(apply go '("super.rkt") #:mode 'fast)
```

Keywords that are included in `apply`’s list argument do not count as
keyword arguments for the called function; instead, all arguments in
this list are treated as by-position arguments. To pass a list of
keyword arguments to a function, use the `keyword-apply` function, which
accepts a function to apply and three lists. The first two lists are in
parallel, where the first list contains keywords \(sorted by
`keyword<?`\), and the second list contains a corresponding argument for
each keyword. The third list contains by-position function arguments, as
for `apply`.

```racket
(keyword-apply go             
               '(#:mode)      
               '(fast)        
               '("super.rkt"))
```

## 4. Functions \(Procedures\): `lambda`

A `lambda` expression creates a function. In the simplest case, a
`lambda` expression has the form

```racket
(lambda (arg-id ...)
  body ...+)        
```

A `lambda` form with _n_ `arg-id`s accepts _n_ arguments:

```racket
> ((lambda (x) x)                                         
   1)                                                     
1                                                         
> ((lambda (x y) (+ x y))                                 
   1 2)                                                   
3                                                         
> ((lambda (x y) (+ x y))                                 
   1)                                                     
#<procedure>: arity mismatch;                             
 the expected number of arguments does not match the given
number                                                    
  expected: 2                                             
  given: 1                                                
  arguments...:                                           
   1                                                      
```

### 4.1. Declaring a Rest Argument

A `lambda` expression can also have the form

```racket
(lambda rest-id
  body ...+)   
```

That is, a `lambda` expression can have a single `rest-id` that is not
surrounded by parentheses. The resulting function accepts any number of
arguments, and the arguments are put into a list bound to `rest-id`.

Examples:

```racket
> ((lambda x x)      
   1 2 3)            
'(1 2 3)             
> ((lambda x x))     
'()                  
> ((lambda x (car x))
   1 2 3)            
1                    
```

Functions with a `rest-id` often use `apply` to call another function
that accepts any number of arguments.

> +The `apply` Function describes `apply`.

Examples:

```racket
(define max-mag                       
  (lambda nums                        
    (apply max (map magnitude nums))))
                                      
> (max 1 -2 0)                        
1                                     
> (max-mag 1 -2 0)                    
2                                     
```

The `lambda` form also supports required arguments combined with a
`rest-id`:

```racket
(lambda (arg-id ...+ . rest-id)
  body ...+)                   
```

The result of this form is a function that requires at least as many
arguments as `arg-id`s, and also accepts any number of additional
arguments.

Examples:

```racket
(define max-mag                                           
  (lambda (num . nums)                                    
    (apply max (map magnitude (cons num nums)))))         
                                                          
> (max-mag 1 -2 0)                                        
2                                                         
> (max-mag)                                               
max-mag: arity mismatch;                                  
 the expected number of arguments does not match the given
number                                                    
  expected: at least 1                                    
  given: 0                                                
```

A `rest-id` variable is sometimes called a _rest argument_, because it
accepts the “rest” of the function arguments.

### 4.2. Declaring Optional Arguments

Instead of just an identifier, an argument \(other than a rest
argument\) in a `lambda` form can be specified with an identifier and a
default value:

```racket
(lambda gen-formals                
  body ...+)                       
                                   
gen-formals = (arg ...)            
            | rest-id              
            | (arg ...+ . rest-id) 
                                   
arg         = arg-id               
            | [arg-id default-expr]
```

An argument of the form `[arg-id default-expr]` is optional. When the
argument is not supplied in an application, `default-expr` produces the
default value. The `default-expr` can refer to any preceding `arg-id`,
and every following `arg-id` must have a default as well.

Examples:

```racket
(define greet                                    
  (lambda (given [surname "Smith"])              
    (string-append "Hello, " given " " surname)))
                                                 
> (greet "John")                                 
"Hello, John Smith"                              
> (greet "John" "Doe")                           
"Hello, John Doe"                                
```

```racket                                          
(define greet                                      
  (lambda (given [surname (if (equal? given "John")
                              "Doe"                
                              "Smith")])           
    (string-append "Hello, " given " " surname)))  
```                                                
                                                   
```racket                                          
> (greet "John")                                   
"Hello, John Doe"                                  
> (greet "Adam")                                   
"Hello, Adam Smith"                                
```                                                

### 4.3. Declaring Keyword Arguments

A `lambda` form can declare an argument to be passed by keyword, instead
of position. Keyword arguments can be mixed with by-position arguments,
and default-value expressions can be supplied for either kind of
argument:

> +Keyword Arguments introduces function calls with keywords.

```racket
(lambda gen-formals                            
  body ...+)                                   
                                               
gen-formals = (arg ...)                        
            | rest-id                          
            | (arg ...+ . rest-id)             
                                               
arg         = arg-id                           
            | [arg-id default-expr]            
            | arg-keyword arg-id               
            | arg-keyword [arg-id default-expr]
```

An argument specified as `arg-keyword arg-id` is supplied by an
application using the same `arg-keyword`.  The position of the
keyword–identifier pair in the argument list does not matter for
matching with arguments in an application, because it will be matched to
an argument value by keyword instead of by position.

```racket                                        
(define greet                                    
  (lambda (given #:last surname)                 
    (string-append "Hello, " given " " surname)))
```                                              
                                                 
```racket                                        
> (greet "John" #:last "Smith")                  
"Hello, John Smith"                              
> (greet #:last "Doe" "John")                    
"Hello, John Doe"                                
```                                              

An `arg-keyword [arg-id default-expr]` argument specifies a
keyword-based argument with a default value.

Examples:

```racket
(define greet                                               
  (lambda (#:hi [hi "Hello"] given #:last [surname "Smith"])
    (string-append hi ", " given " " surname)))             
                                                            
> (greet "John")                                            
"Hello, John Smith"                                         
> (greet "Karl" #:last "Marx")                              
"Hello, Karl Marx"                                          
> (greet "John" #:hi "Howdy")                               
"Howdy, John Smith"                                         
> (greet "Karl" #:last "Marx" #:hi "Guten Tag")             
"Guten Tag, Karl Marx"                                      
```

The `lambda` form does not directly support the creation of a function
that accepts “rest” keywords. To construct a function that accepts all
keyword arguments, use `make-keyword-procedure`. The function supplied
to `make-keyword-procedure` receives keyword arguments through parallel
lists in the first two \(by-position\) arguments, and then all
by-position arguments from an application as the remaining by-position
arguments.

> +The `apply` Function introduces `keyword-apply`.

Examples:

```racket
(define (trace-wrap f)                                 
  (make-keyword-procedure                              
   (lambda (kws kw-args . rest)                        
     (printf "Called with ~s ~s ~s\n" kws kw-args rest)
     (keyword-apply f kws kw-args rest))))             
                                                       
> ((trace-wrap greet) "John" #:hi "Howdy")             
Called with (#:hi) ("Howdy") ("John")                  
"Howdy, John Smith"                                    
```

> +\[missing\] in \[missing\] provides more on function expressions.

### 4.4. Arity-Sensitive Functions: `case-lambda`

The `case-lambda` form creates a function that can have completely
different behaviors depending on the number of arguments that are
supplied. A case-lambda expression has the form

```racket
(case-lambda                     
  [formals body ...+]            
  ...)                           
                                 
formals = (arg-id ...)           
        | rest-id                
        | (arg-id ...+ . rest-id)
```

where each `[formals body ...+]` is analogous to `(lambda formals body
...+)`. Applying a function produced by `case-lambda` is like applying a
`lambda` for the first case that matches the number of given arguments.

Examples:

```racket
(define greet                                                      
  (case-lambda                                                     
    [(name) (string-append "Hello, " name)]                        
    [(given surname) (string-append "Hello, " given " " surname)]))
                                                                   
> (greet "John")                                                   
"Hello, John"                                                      
> (greet "John" "Smith")                                           
"Hello, John Smith"                                                
> (greet)                                                          
greet: arity mismatch;                                             
 the expected number of arguments does not match the given         
number                                                             
  given: 0                                                         
```

A `case-lambda` function cannot directly support optional or keyword
arguments.

## 5. Definitions: `define`

A basic definition has the form

```racket
(define id expr)
```

in which case `id` is bound to the result of `expr`.

Examples:

```racket
(define salutation (list-ref '("Hi" "Hello") (random 2)))
                                                         
> salutation                                             
"Hello"                                                  
```

### 5.1. Function Shorthand

The `define` form also supports a shorthand for function definitions:

```racket
(define (id arg ...) body ...+)
```

which is a shorthand for

`(define` `id` `(lambda` `(arg` `...)` `body` `...+))`

Examples:

```racket
(define (greet name)                   
  (string-append salutation ", " name))
                                       
> (greet "John")                       
"Hello, John"                          
```

```racket                                                   
(define (greet first [surname "Smith"] #:hi [hi salutation])
  (string-append hi ", " first " " surname))                
```                                                         
                                                            
```racket                                                   
> (greet "John")                                            
"Hello, John Smith"                                         
> (greet "John" #:hi "Hey")                                 
"Hey, John Smith"                                           
> (greet "John" "Doe")                                      
"Hello, John Doe"                                           
```                                                         

The function shorthand via `define` also supports a rest argument
\(i.e., a final argument to collect extra arguments in a list\):

```racket
(define (id arg ... . rest-id) body ...+)
```

which is a shorthand

`(define` `id` `(lambda` `(arg` `...` `. rest-id)` `body` `...+))`

Examples:

```racket
(define (avg . l)            
  (/ (apply + l) (length l)))
                             
> (avg 1 2 3)                
2                            
```

### 5.2. Curried Function Shorthand

Consider the following `make-add-suffix` function that takes a string
and returns another function that takes a string:

```racket                              
(define make-add-suffix                
  (lambda (s2)                         
    (lambda (s) (string-append s s2))))
```                                    
                                       
                                       

Although it’s not common, result of `make-add-suffix` could be called
directly, like this:

```racket
> ((make-add-suffix "!") "hello")
"hello!"                         
```

In a sense, `make-add-suffix` is a function takes two arguments, but it
takes them one at a time. A function that takes some of its arguments
and returns a function to consume more is sometimes called a _curried
function_.

Using the function-shorthand form of `define`, `make-add-suffix` can be
written equivalently as

```racket
(define (make-add-suffix s2)        
  (lambda (s) (string-append s s2)))
```

This shorthand reflects the shape of the function call `(make-add-suffix
"!")`. The `define` form further supports a shorthand for defining
curried functions that reflects nested function calls:

```racket                        
(define ((make-add-suffix s2) s) 
  (string-append s s2))          
```                              
                                 
```racket                        
> ((make-add-suffix "!") "hello")
"hello!"                         
```                              

```racket                               
(define louder (make-add-suffix "!"))   
(define less-sure (make-add-suffix "?"))
```                                     
                                        
```racket                               
> (less-sure "really")                  
"really?"                               
> (louder "really")                     
"really!"                               
```                                     

The full syntax of the function shorthand for `define` is as follows:

```racket
(define (head args) body ...+)
                              
head = id                     
     | (head args)            
                              
args = arg ...                
     | arg ... . rest-id      
```

The expansion of this shorthand has one nested `lambda` form for each
`head` in the definition, where the innermost `head` corresponds to the
outermost `lambda`.

### 5.3. Multiple Values and `define-values`

A Racket expression normally produces a single result, but some
expressions can produce multiple results. For example, `quotient` and
`remainder` each produce a single value, but `quotient/remainder`
produces the same two values at once:

```racket
> (quotient 13 3)          
4                          
> (remainder 13 3)         
1                          
> (quotient/remainder 13 3)
4                          
1                          
```

As shown above, the REPL prints each result value on its own line.

Multiple-valued functions can be implemented in terms of the `values`
function, which takes any number of values and returns them as the
results:

```racket
> (values 1 2 3)
1               
2               
3               
```

```racket                                             
(define (split-name name)                             
  (let ([parts (regexp-split " " name)])              
    (if (= (length parts) 2)                          
        (values (list-ref parts 0) (list-ref parts 1))
        (error "not a <first> <last> name"))))        
```                                                   
                                                      
```racket                                             
> (split-name "Adam Smith")                           
"Adam"                                                
"Smith"                                               
```                                                   

The `define-values` form binds multiple identifiers at once to multiple
results produced from a single expression:

```racket
(define-values (id ...) expr)
```

The number of results produced by the `expr` must match the number of
`id`s.

Examples:

```racket
(define-values (given surname) (split-name "Adam Smith"))
                                                         
> given                                                  
"Adam"                                                   
> surname                                                
"Smith"                                                  
```

A `define` form \(that is not a function shorthand\) is equivalent to a
`define-values` form with a single `id`.

> +\[missing\] in \[missing\] provides more on definitions.

### 5.4. Internal Definitions

When the grammar for a syntactic form specifies `body`, then the
corresponding form can be either a definition or an expression. A
definition as a `body` is an _internal definition_.

Expressions and internal definitions in a `body` sequence can be mixed,
as long as the last `body` is an expression.

For example, the syntax of `lambda` is

```racket
(lambda gen-formals
  body ...+)       
```

so the following are valid instances of the grammar:

```racket
(lambda (f)                ; no definitions 
  (printf "running\n")                      
  (f 0))                                    
                                            
(lambda (f)                ; one definition 
  (define (log-it what)                     
    (printf "~a\n" what))                   
  (log-it "running")                        
  (f 0)                                     
  (log-it "done"))                          
                                            
(lambda (f n)              ; two definitions
  (define (call n)                          
    (if (zero? n)                           
        (log-it "done")                     
        (begin                              
          (log-it "running")                
          (f n)                             
          (call (- n 1)))))                 
  (define (log-it what)                     
    (printf "~a\n" what))                   
  (call n))                                 
```

Internal definitions in a particular `body` sequence are mutually
recursive; that is, any definition can refer to any other definition—as
long as the reference isn’t actually evaluated before its definition
takes place. If a definition is referenced too early, an error occurs.

Examples:

```racket
(define (weird)                  
  (define x x)                   
  x)                             
                                 
> (weird)                        
x: undefined;                    
 cannot use before initialization
```

A sequence of internal definitions using just `define` is easily
translated to an equivalent `letrec` form \(as introduced in the next
section\). However, other definition forms can appear as a `body`,
including `define-values`, `struct` \(see \[missing\]\) or
`define-syntax` \(see \[missing\]\).

> +\[missing\] in \[missing\] documents the fine points of internal
> definitions.

## 6. Local Binding

Although internal `define`s can be used for local binding, Racket
provides three forms that give the programmer more control over
bindings: `let`, `let*`, and `letrec`.

### 6.1. Parallel Binding: `let`

> +\[missing\] in \[missing\] also documents `let`.

A `let` form binds a set of identifiers, each to the result of some
expression, for use in the `let` body:

```racket
(let ([id expr] ...) body ...+)
```

The `id`s are bound “in parallel.” That is, no `id` is bound in the
right-hand side `expr` for any `id`, but all are available in the
`body`. The `id`s must be different from each other.

Examples:

```racket
> (let ([me "Bob"])                      
    me)                                  
"Bob"                                    
> (let ([me "Bob"]                       
        [myself "Robert"]                
        [I "Bobby"])                     
    (list me myself I))                  
'("Bob" "Robert" "Bobby")                
> (let ([me "Bob"]                       
        [me "Robert"])                   
    me)                                  
eval:3:0: let: duplicate identifier      
  at: me                                 
  in: (let ((me "Bob") (me "Robert")) me)
```

The fact that an `id`’s `expr` does not see its own binding is often
useful for wrappers that must refer back to the old value:

```racket
> (let ([+ (lambda (x y)                     
             (if (string? x)                 
                 (string-append x y)         
                 (+ x y)))]) ; use original +
    (list (+ 1 2)                            
          (+ "see" "saw")))                  
'(3 "seesaw")                                
```

Occasionally, the parallel nature of `let` bindings is convenient for
swapping or rearranging a set of bindings:

```racket
> (let ([me "Tarzan"]
        [you "Jane"])
    (let ([me you]   
          [you me])  
      (list me you)))
'("Jane" "Tarzan")   
```

The characterization of `let` bindings as “parallel” is not meant to
imply concurrent evaluation. The `expr`s are evaluated in order, even
though the bindings are delayed until all `expr`s are evaluated.

### 6.2. Sequential Binding: `let*`

> +\[missing\] in \[missing\] also documents `let*`.

The syntax of `let*` is the same as `let`:

```racket
(let* ([id expr] ...) body ...+)
```

The difference is that each `id` is available for use in later `expr`s,
as well as in the `body`. Furthermore, the `id`s need not be distinct,
and the most recent binding is the visible one.

Examples:

```racket
> (let* ([x (list "Burroughs")]                                   
         [y (cons "Rice" x)]                                      
         [z (cons "Edgar" y)])                                    
    (list x y z))                                                 
'(("Burroughs") ("Rice" "Burroughs") ("Edgar" "Rice" "Burroughs"))
> (let* ([name (list "Burroughs")]                                
         [name (cons "Rice" name)]                                
         [name (cons "Edgar" name)])                              
    name)                                                         
'("Edgar" "Rice" "Burroughs")                                     
```

In other words, a `let*` form is equivalent to nested `let` forms, each
with a single binding:

```racket
> (let ([name (list "Burroughs")])     
    (let ([name (cons "Rice" name)])   
      (let ([name (cons "Edgar" name)])
        name)))                        
'("Edgar" "Rice" "Burroughs")          
```

### 6.3. Recursive Binding: `letrec`

> +\[missing\] in \[missing\] also documents `letrec`.

The syntax of `letrec` is also the same as `let`:

```racket
(letrec ([id expr] ...) body ...+)
```

While `let` makes its bindings available only in the `body`s, and `let*`
makes its bindings available to any later binding `expr`, `letrec` makes
its bindings available to all other `expr`s—even earlier ones. In other
words, `letrec` bindings are recursive.

The `expr`s in a `letrec` form are most often `lambda` forms for
recursive and mutually recursive functions:

```racket
> (letrec ([swing                               
            (lambda (t)                         
              (if (eq? (car t) 'tarzan)         
                  (cons 'vine                   
                        (cons 'tarzan (cddr t)))
                  (cons (car t)                 
                        (swing (cdr t)))))])    
    (swing '(vine tarzan vine vine)))           
'(vine vine tarzan vine)                        
```

```racket
> (letrec ([tarzan-near-top-of-tree?                                     
            (lambda (name path depth)                                    
              (or (equal? name "tarzan")                                 
                  (and (directory-exists? path)                          
                       (tarzan-in-directory? path depth))))]             
           [tarzan-in-directory?                                         
            (lambda (dir depth)                                          
              (cond                                                      
                [(zero? depth) #f]                                       
                [else                                                    
                 (ormap                                                  
                  (λ (elem)                                              
                    (tarzan-near-top-of-tree? (path-element->string elem)
                                              (build-path dir elem)      
                                              (- depth 1)))              
                  (directory-list dir))]))])                             
    (tarzan-near-top-of-tree? "tmp"                                      
                              (find-system-path 'temp-dir)               
                              4))                                        
#f                                                                       
```

While the `expr`s of a `letrec` form are typically `lambda` expressions,
they can be any expression. The expressions are evaluated in order, and
after each value is obtained, it is immediately associated with its
corresponding `id`. If an `id` is referenced before its value is ready,
an error is raised, just as for internal definitions.

```racket
> (letrec ([quicksand quicksand])
    quicksand)                   
quicksand: undefined;            
 cannot use before initialization
```

### 6.4. Named `let`

A named `let` is an iteration and recursion form. It uses the same
syntactic keyword `let` as for local binding, but an identifier after
the `let` \(instead of an immediate open parenthesis\) triggers a
different parsing.

```racket
(let proc-id ([arg-id init-expr] ...)
  body ...+)                         
```

A named `let` form is equivalent to

```racket
(letrec ([proc-id (lambda (arg-id ...)
                     body ...+)])     
  (proc-id init-expr ...))            
```

That is, a named `let` binds a function identifier that is visible only
in the function’s body, and it implicitly calls the function with the
values of some initial expressions.

Examples:

```racket
(define (duplicate pos lst)                             
  (let dup ([i 0]                                       
            [lst lst])                                  
   (cond                                                
    [(= i pos) (cons (car lst) lst)]                    
    [else (cons (car lst) (dup (+ i 1) (cdr lst)))])))  
                                                        
> (duplicate 1 (list "apple" "cheese burger!" "banana"))
'("apple" "cheese burger!" "cheese burger!" "banana")   
```

### 6.5. Multiple Values: `let-values`, `let*-values`, `letrec-values`

> +\[missing\] in \[missing\] also documents multiple-value binding forms.

In the same way that `define-values` binds multiple results in a
definition \(see Multiple Values and `define-values`\), `let-values`,
`let*-values`, and `letrec-values` bind multiple results locally.

```racket
(let-values ([(id ...) expr] ...)
  body ...+)                     
```

```racket
(let*-values ([(id ...) expr] ...)
  body ...+)                      
```

```racket
(letrec-values ([(id ...) expr] ...)
  body ...+)                        
```

Each `expr` must produce as many values as corresponding `id`s. The
binding rules are the same for the forms without `-values` forms: the
`id`s of `let-values` are bound only in the `body`s, the `id`s of
`let*-values`s are bound in `expr`s of later clauses, and the `id`s of
`letrec-value`s are bound for all `expr`s.

Example:

```racket
> (let-values ([(q r) (quotient/remainder 14 3)])
    (list q r))                                  
'(4 2)                                           
```

## 7. Conditionals

Most functions used for branching, such as `<` and `string?`, produce
either `#t` or `#f`. Racket’s branching forms, however, treat any value
other than `#f` as true. We say a _true value_ to mean any value other
than `#f`.

This convention for “true value” meshes well with protocols where `#f`
can serve as failure or to indicate that an optional value is not
supplied. \(Beware of overusing this trick, and remember that an
exception is usually a better mechanism to report failure.\)

For example, the `member` function serves double duty; it can be used to
find the tail of a list that starts with a particular item, or it can be
used to simply check whether an item is present in a list:

```racket
> (member "Groucho" '("Harpo" "Zeppo"))              
#f                                                   
> (member "Groucho" '("Harpo" "Groucho" "Zeppo"))    
'("Groucho" "Zeppo")                                 
> (if (member "Groucho" '("Harpo" "Zeppo"))          
      'yep                                           
      'nope)                                         
'nope                                                
> (if (member "Groucho" '("Harpo" "Groucho" "Zeppo"))
      'yep                                           
      'nope)                                         
'yep                                                 
```

### 7.1. Simple Branching: `if`

> +\[missing\] in \[missing\] also documents `if`.

In an `if` form,

```racket
(if test-expr then-expr else-expr)
```

the `test-expr` is always evaluated. If it produces any value other than
`#f`, then `then-expr` is evaluated. Otherwise, `else-expr` is
evaluated.

An `if` form must have both a `then-expr` and an `else-expr`; the latter
is not optional. To perform \(or skip\) side-effects based on a
`test-expr`, use `when` or `unless`, which we describe later in
Sequencing.

### 7.2. Combining Tests: `and` and `or`

> +\[missing\] in \[missing\] also documents `and` and `or`.

Racket’s `and` and `or` are syntactic forms, rather than functions.
Unlike a function, the `and` and `or` forms can skip evaluation of later
expressions if an earlier one determines the answer.

```racket
(and expr ...)
```

An `and` form produces `#f` if any of its `expr`s produces `#f`.
Otherwise, it produces the value of its last `expr`. As a special case,
`(and)` produces `#t`.

```racket
(or expr ...)
```

The `or` form produces `#f` if all of its `expr`s produce `#f`.
Otherwise, it produces the first non-`#f` value from its `expr`s.  As a
special case, `(or)` produces `#f`.

Examples:

```racket
> (define (got-milk? lst)                                    
    (and (not (null? lst))                                   
         (or (eq? 'milk (car lst))                           
             (got-milk? (cdr lst))))) ; recurs only if needed
> (got-milk? '(apple banana))                                
#f                                                           
> (got-milk? '(apple milk banana))                           
#t                                                           
```

If evaluation reaches the last `expr` of an `and` or `or` form, then the
`expr`’s value directly determines the `and` or `or` result. Therefore,
the last `expr` is in tail position, which means that the above
`got-milk?` function runs in constant space.

> +\[missing\] introduces tail calls and tail positions.

### 7.3. Chaining Tests: `cond`

The `cond` form chains a series of tests to select a result expression.
To a first approximation, the syntax of `cond` is as follows:

> +\[missing\] in \[missing\] also documents `cond`.

```racket
(cond [test-expr body ...+]
      ...)                 
```

Each `test-expr` is evaluated in order. If it produces `#f`, the
corresponding `body`s are ignored, and evaluation proceeds to the next
`test-expr`. As soon as a `test-expr` produces a true value, its `body`s
are evaluated to produce the result for the `cond` form, and no further
`test-expr`s are evaluated.

The last `test-expr` in a `cond` can be replaced by `else`. In terms of
evaluation, `else` serves as a synonym for `#t`, but it clarifies that
the last clause is meant to catch all remaining cases. If `else` is not
used, then it is possible that no `test-expr`s produce a true value; in
that case, the result of the `cond` expression is `#<void>`.

Examples:

```racket
> (cond                       
   [(= 2 3) (error "wrong!")] 
   [(= 2 2) 'ok])             
'ok                           
> (cond                       
   [(= 2 3) (error "wrong!")])
> (cond                       
   [(= 2 3) (error "wrong!")] 
   [else 'ok])                
'ok                           
```

```racket                         
(define (got-milk? lst)           
  (cond                           
    [(null? lst) #f]              
    [(eq? 'milk (car lst)) #t]    
    [else (got-milk? (cdr lst))]))
```                               
                                  
```racket                         
> (got-milk? '(apple banana))     
#f                                
> (got-milk? '(apple milk banana))
#t                                
```                               

The full syntax of `cond` includes two more kinds of clauses:

```racket
(cond cond-clause ...)                  
                                        
cond-clause = [test-expr then-body ...+]
            | [else then-body ...+]     
            | [test-expr => proc-expr]  
            | [test-expr]               
```

The `=>` variant captures the true result of its `test-expr` and passes
it to the result of the `proc-expr`, which must be a function of one
argument.

Examples:

```racket
> (define (after-groucho lst)                 
    (cond                                     
      [(member "Groucho" lst) => cdr]         
      [else (error "not there")]))            
> (after-groucho '("Harpo" "Groucho" "Zeppo"))
'("Zeppo")                                    
> (after-groucho '("Harpo" "Zeppo"))          
not there                                     
```

A clause that includes only a `test-expr` is rarely used. It captures
the true result of the `test-expr`, and simply returns the result for
the whole `cond` expression.

## 8. Sequencing

Racket programmers prefer to write programs with as few side-effects as
possible, since purely functional code is more easily tested and
composed into larger programs. Interaction with the external
environment, however, requires sequencing, such as when writing to a
display, opening a graphical window, or manipulating a file on disk.

### 8.1. Effects Before: `begin`

> +\[missing\] in \[missing\] also documents `begin`.

A `begin` expression sequences expressions:

```racket
(begin expr ...+)
```

The `expr`s are evaluated in order, and the result of all but the last
`expr` is ignored. The result from the last `expr` is the result of the
`begin` form, and it is in tail position with respect to the `begin`
form.

Examples:

```racket
(define (print-triangle height)           
  (if (zero? height)                      
      (void)                              
      (begin                              
        (display (make-string height #\*))
        (newline)                         
        (print-triangle (sub1 height))))) 
                                          
> (print-triangle 4)                      
****                                      
***                                       
**                                        
*                                         
```

Many forms, such as `lambda` or `cond` support a sequence of expressions
even without a `begin`. Such positions are sometimes said to have an
_implicit begin_.

Examples:

```racket
(define (print-triangle height)        
  (cond                                
    [(positive? height)                
     (display (make-string height #\*))
     (newline)                         
     (print-triangle (sub1 height))])) 
                                       
> (print-triangle 4)                   
****                                   
***                                    
**                                     
*                                      
```

The `begin` form is special at the top level, at module level, or as a
`body` after only internal definitions. In those positions, instead of
forming an expression, the content of `begin` is spliced into the
surrounding context.

Example:

```racket
> (let ([curly 0])             
    (begin                     
      (define moe (+ 1 curly)) 
      (define larry (+ 1 moe)))
    (list larry curly moe))    
'(2 0 1)                       
```

This splicing behavior is mainly useful for macros, as we discuss later
in \[missing\].

### 8.2. Effects After: `begin0`

> +\[missing\] in \[missing\] also documents `begin0`.

A `begin0` expression has the same syntax as a `begin` expression:

```racket
(begin0 expr ...+)
```

The difference is that `begin0` returns the result of the first `expr`,
instead of the result of the last `expr`. The `begin0` form is useful
for implementing side-effects that happen after a computation,
especially in the case where the computation produces an unknown number
of results.

Examples:

```racket
(define (log-times thunk)                                  
  (printf "Start: ~s\n" (current-inexact-milliseconds))    
  (begin0                                                  
    (thunk)                                                
    (printf "End..: ~s\n" (current-inexact-milliseconds))))
                                                           
> (log-times (lambda () (sleep 0.1) 0))                    
Start: 1548117365804.718                                   
End..: 1548117365906.647                                   
0                                                          
> (log-times (lambda () (values 1 2)))                     
Start: 1548117365907.178                                   
End..: 1548117365907.202                                   
1                                                          
2                                                          
```

### 8.3. Effects If...: `when` and `unless`

> +\[missing\] in \[missing\] also documents `when` and `unless`.

The `when` form combines an `if`-style conditional with sequencing for
the “then” clause and no “else” clause:

```racket
(when test-expr then-body ...+)
```

If `test-expr` produces a true value, then all of the `then-body`s are
evaluated. The result of the last `then-body` is the result of the
`when` form. Otherwise, no `then-body`s are evaluated and the result is
`#<void>`.

The `unless` form is similar:

```racket
(unless test-expr then-body ...+)
```

The difference is that the `test-expr` result is inverted: the
`then-body`s are evaluated only if the `test-expr` result is `#f`.

Examples:

```racket
(define (enumerate lst)               
  (if (null? (cdr lst))               
      (printf "~a.\n" (car lst))      
      (begin                          
        (printf "~a, " (car lst))     
        (when (null? (cdr (cdr lst))) 
          (printf "and "))            
        (enumerate (cdr lst)))))      
                                      
> (enumerate '("Larry" "Curly" "Moe"))
Larry, Curly, and Moe.                
```

```racket                             
(define (print-triangle height)       
  (unless (zero? height)              
    (display (make-string height #\*))
    (newline)                         
    (print-triangle (sub1 height))))  
```                                   
                                      
```racket                             
> (print-triangle 4)                  
****                                  
***                                   
**                                    
*                                     
```                                   

## 9. Assignment: `set!`

> +\[missing\] in \[missing\] also documents `set!`.

Assign to a variable using `set!`:

```racket
(set! id expr)
```

A `set!` expression evaluates `expr` and changes `id` \(which must be
bound in the enclosing environment\) to the resulting value. The result
of the `set!`  expression itself is `#<void>`.

Examples:

```racket
(define greeted null)               
                                    
(define (greet name)                
  (set! greeted (cons name greeted))
  (string-append "Hello, " name))   
                                    
> (greet "Athos")                   
"Hello, Athos"                      
> (greet "Porthos")                 
"Hello, Porthos"                    
> (greet "Aramis")                  
"Hello, Aramis"                     
> greeted                           
'("Aramis" "Porthos" "Athos")       
```

```racket                         
(define (make-running-total)      
  (let ([n 0])                    
    (lambda ()                    
      (set! n (+ n 1))            
      n)))                        
(define win (make-running-total)) 
(define lose (make-running-total))
```                               
                                  
```racket                         
> (win)                           
1                                 
> (win)                           
2                                 
> (lose)                          
1                                 
> (win)                           
3                                 
```                               

### 9.1. Guidelines for Using Assignment

Although using `set!` is sometimes appropriate, Racket style generally
discourages the use of `set!`. The following guidelines may help explain
when using `set!` is appropriate.

* As in any modern language, assigning to a shared identifier is no
  substitute for passing an argument to a procedure or getting  its
  result.

  **_Really awful_** example:

  ```racket                                      
  (define name "unknown")                        
  (define result "unknown")                      
  (define (greet)                                
    (set! result (string-append "Hello, " name)))
  ```                                            
                                                 
  ```racket                                      
  > (set! name "John")                           
  > (greet)                                      
  > result                                       
  "Hello, John"                                  
  ```                                            

  Ok example:

  ```racket                        
  (define (greet name)             
    (string-append "Hello, " name))
  ```                              
                                   
  ```racket                        
  > (greet "John")                 
  "Hello, John"                    
  > (greet "Anna")                 
  "Hello, Anna"                    
  ```                              

* A sequence of assignments to a local variable is far inferior to
  nested bindings.

  **Bad** example:

  ```racket
> (let ([tree 0])                           
      (set! tree (list tree 1 tree))          
      (set! tree (list tree 2 tree))          
      (set! tree (list tree 3 tree))          
      tree)                                   
  '(((0 1 0) 2 (0 1 0)) 3 ((0 1 0) 2 (0 1 0)))
```

  Ok example:

  ```racket
> (let* ([tree 0]                           
           [tree (list tree 1 tree)]          
           [tree (list tree 2 tree)]          
           [tree (list tree 3 tree)])         
      tree)                                   
  '(((0 1 0) 2 (0 1 0)) 3 ((0 1 0) 2 (0 1 0)))
```

* Using assignment to accumulate results from an iteration is bad style.
  Accumulating through a loop argument is better.

  Somewhat bad example:

  ```racket                                  
  (define (sum lst)                          
    (let ([s 0])                             
      (for-each (lambda (i) (set! s (+ i s)))
                lst)                         
      s))                                    
  ```                                        
                                             
  ```racket                                  
  > (sum '(1 2 3))                           
  6                                          
  ```                                        

  Ok example:

  ```racket                                  
  (define (sum lst)                          
    (let loop ([lst lst] [s 0])              
      (if (null? lst)                        
          s                                  
          (loop (cdr lst) (+ s (car lst))))))
  ```                                        
                                             
  ```racket                                  
  > (sum '(1 2 3))                           
  6                                          
  ```                                        

  Better \(use an existing function\) example:

  ```racket        
  (define (sum lst)
    (apply + lst)) 
  ```              
                   
  ```racket        
  > (sum '(1 2 3)) 
  6                
  ```              

  Good \(a general approach\) example:

  ```racket                      
  (define (sum lst)              
    (for/fold ([s 0])            
              ([i (in-list lst)])
      (+ s i)))                  
  ```                            
                                 
  ```racket                      
  > (sum '(1 2 3))               
  6                              
  ```                            

* For cases where stateful objects are necessary or appropriate, then
  implementing the object’s state with `set!` is fine.

  Ok example:

  ```racket              
  (define next-number!   
    (let ([n 0])         
      (lambda ()         
        (set! n (add1 n))
        n)))             
  ```                    
                         
  ```racket              
  > (next-number!)       
  1                      
  > (next-number!)       
  2                      
  > (next-number!)       
  3                      
  ```                    

All else being equal, a program that uses no assignments or mutation is
always preferable to one that uses assignments or mutation. While side
effects are to be avoided, however, they should be used if the resulting
code is significantly more readable or if it implements a significantly
better algorithm.

The use of mutable values, such as vectors and hash tables, raises fewer
suspicions about the style of a program than using `set!` directly.
Nevertheless, simply replacing `set!`s in a program with `vector-set!`s
obviously does not improve the style of the program.

### 9.2. Multiple Values: `set!-values`

> +\[missing\] in \[missing\] also documents `set!-values`.

The `set!-values` form assigns to multiple variables at once, given an
expression that produces an appropriate number of values:

```racket
(set!-values (id ...) expr)
```

This form is equivalent to using `let-values` to receive multiple
results from `expr`, and then assigning the results individually to the
`id`s using `set!`.

Examples:

```racket
(define game                                
  (let ([w 0]                               
        [l 0])                              
    (lambda (win?)                          
      (if win?                              
          (set! w (+ w 1))                  
          (set! l (+ l 1)))                 
      (begin0                               
        (values w l)                        
        ; swap sides...                     
        (set!-values (w l) (values l w))))))
                                            
> (game #t)                                 
1                                           
0                                           
> (game #t)                                 
1                                           
1                                           
> (game #f)                                 
1                                           
2                                           
```

## 10. Quoting: `quote` and `'`

> +\[missing\] in \[missing\] also documents `quote`.

The `quote` form produces a constant:

```racket
(quote datum)
```

The syntax of a `datum` is technically specified as anything that the
`read` function parses as a single element. The value of the `quote`
form is the same value that `read` would produce given `datum`.

The `datum` can be a symbol, a boolean, a number, a \(character or
byte\) string, a character, a keyword, an empty list, a pair \(or list\)
containing more such values, a vector containing more such values, a
hash table containing more such values, or a box containing another such
value.

Examples:

```racket
> (quote apple)                       
'apple                                
> (quote #t)                          
#t                                    
> (quote 42)                          
42                                    
> (quote "hello")                     
"hello"                               
> (quote ())                          
'()                                   
> (quote ((1 2 3) #("z" x) . the-end))
'((1 2 3) #("z" x) . the-end)         
> (quote (1 2 . (3)))                 
'(1 2 3)                              
```

As the last example above shows, the `datum` does not have to match the
normalized printed form of a value. A `datum` cannot be a printed
representation that starts with `#<`, so it cannot be `#<void>`,
`#<undefined>`, or a procedure.

The `quote` form is rarely used for a `datum` that is a boolean, number,
or string by itself, since the printed forms of those values can already
be used as constants. The `quote` form is more typically used for
symbols and lists, which have other meanings \(identifiers, function
calls, etc.\) when not quoted.

An expression

```racket
'datum
```

is a shorthand for

`(quote` `datum)`

and this shorthand is almost always used instead of `quote`. The
shorthand applies even within the `datum`, so it can produce a list
containing `quote`.

> +\[missing\] in \[missing\] provides more on the `'` shorthand.

Examples:

```racket
> 'apple                  
'apple                    
> '"hello"                
"hello"                   
> '(1 2 3)                
'(1 2 3)                  
> (display '(you can 'me))
(you can (quote me))      
```

## 11. Quasiquoting: `quasiquote` and `‘`

> +\[missing\] in \[missing\] also documents `quasiquote`.

The `quasiquote` form is similar to `quote`:

```racket
(quasiquote datum)
```

However, for each `(unquote expr)` that appears within the `datum`, the
`expr` is evaluated to produce a value that takes the place of the
`unquote` sub-form.

Example:

```racket
> (quasiquote (1 2 (unquote (+ 1 2)) (unquote (- 5 1))))
'(1 2 3 4)                                              
```

This form can be used to write functions that build lists according to
certain patterns.

Examples:

```racket
> (define (deep n)                                           
    (cond                                                    
      [(zero? n) 0]                                          
      [else                                                  
       (quasiquote ((unquote n) (unquote (deep (- n 1)))))]))
> (deep 8)                                                   
'(8 (7 (6 (5 (4 (3 (2 (1 0))))))))                           
```

Or even to cheaply construct expressions programmatically. \(Of course,
9 times out of 10, you should be using a macro to do this \(the 10th
time being when you’re working through a textbook like
[PLAI](http://www.cs.brown.edu/~sk/Publications/Books/ProgLangs/)\).\)

Examples:

```racket
> (define (build-exp n)                                       
    (add-lets n (make-sum n)))                                
> (define (add-lets n body)                                   
    (cond                                                     
      [(zero? n) body]                                        
      [else                                                   
       (quasiquote                                            
        (let ([(unquote (n->var n)) (unquote n)])             
          (unquote (add-lets (- n 1) body))))]))              
> (define (make-sum n)                                        
    (cond                                                     
      [(= n 1) (n->var 1)]                                    
      [else                                                   
       (quasiquote (+ (unquote (n->var n))                    
                      (unquote (make-sum (- n 1)))))]))       
> (define (n->var n) (string->symbol (format "x~a" n)))       
> (build-exp 3)                                               
'(let ((x3 3)) (let ((x2 2)) (let ((x1 1)) (+ x3 (+ x2 x1)))))
```

The `unquote-splicing` form is similar to `unquote`, but its `expr` must
produce a list, and the `unquote-splicing` form must appear in a context
that produces either a list or a vector. As the name suggests, the
resulting list is spliced into the context of its use.

Example:

```racket
> (quasiquote (1 2 (unquote-splicing (list (+ 1 2) (- 5 1))) 5))
'(1 2 3 4 5)                                                    
```

Using splicing we can revise the construction of our example expressions
above to have just a single `let` expression and a single `+`
expression.

Examples:

```racket
> (define (build-exp n)                                          
    (add-lets                                                    
     n                                                           
     (quasiquote (+ (unquote-splicing                            
                     (build-list                                 
                      n                                          
                      (λ (x) (n->var (+ x 1)))))))))             
> (define (add-lets n body)                                      
    (quasiquote                                                  
     (let (unquote                                               
           (build-list                                           
            n                                                    
            (λ (n)                                               
              (quasiquote                                        
               [(unquote (n->var (+ n 1))) (unquote (+ n 1))]))))
       (unquote body))))                                         
> (define (n->var n) (string->symbol (format "x~a" n)))          
> (build-exp 3)                                                  
'(let ((x1 1) (x2 2) (x3 3)) (+ x1 x2 x3))                       
```

If a `quasiquote` form appears within an enclosing `quasiquote` form,
then the inner `quasiquote` effectively cancels one layer of `unquote`
and `unquote-splicing` forms, so that a second `unquote` or
`unquote-splicing` is needed.

Examples:

```racket
> (quasiquote (1 2 (quasiquote (unquote (+ 1 2)))))                            
'(1 2 (quasiquote (unquote (+ 1 2))))                                          
> (quasiquote (1 2 (quasiquote (unquote (unquote (+ 1 2))))))                  
'(1 2 (quasiquote (unquote 3)))                                                
>                                                                              
(quasiquote (1 2 (quasiquote ((unquote (+ 1 2)) (unquote (unquote (- 5 1)))))))
'(1 2 (quasiquote ((unquote (+ 1 2)) (unquote 4))))                            
```

The evaluations above will not actually print as shown. Instead, the
shorthand form of `quasiquote` and `unquote` will be used: ` \(i.e., a
backquote\) and `,` \(i.e., a comma\). The same shorthands can be used
in expressions:

Example:

```racket
> `(1 2 `(,(+ 1 2) ,,(- 5 1)))
'(1 2 `(,(+ 1 2) ,4))         
```

The shorthand form of `unquote-splicing` is `,@`:

Example:

```racket
> `(1 2 ,@(list (+ 1 2) (- 5 1)))
'(1 2 3 4)                       
```

## 12. Simple Dispatch: `case`

The `case` form dispatches to a clause by matching the result of an
expression to the values for the clause:

```racket
(case expr                
  [(datum ...+) body ...+]
  ...)                    
```

Each `datum` will be compared to the result of `expr` using `equal?`,
and then the corresponding `body`s are evaluated. The `case` form can
dispatch to the correct clause in _O_\(_log N_\)__ time for _N_
`datum`s.

Multiple `datum`s can be supplied for each clause, and the corresponding
`body`s are evaluated if any of the `datum`s match.

Example:

```racket
> (let ([v (random 6)])
    (printf "~a\n" v)  
    (case v            
      [(0) 'zero]      
      [(1) 'one]       
      [(2) 'two]       
      [(3 4 5) 'many]))
2                      
'two                   
```

The last clause of a `case` form can use `else`, just like `cond`:

Example:

```racket
> (case (random 6)
    [(0) 'zero]   
    [(1) 'one]    
    [(2) 'two]    
    [else 'many]) 
'two              
```

For more general pattern matching \(but without the dispatch-time
guarantee\), use `match`, which is introduced in \[missing\].

## 13. Dynamic Binding: `parameterize`

> +\[missing\] in \[missing\] also documents `parameterize`.

The `parameterize` form associates a new value with a _parameter_ during
the evaluation of `body` expressions:

```racket
(parameterize ([parameter-expr value-expr] ...)
  body ...+)                                   
```

> The term “parameter” is sometimes used to refer to the arguments of a
> function, but “parameter” in Racket has the more specific meaning
> described here.

For example, the `error-print-width` parameter controls how many
characters of a value are printed in an error message:

```racket
> (parameterize ([error-print-width 5]) 
    (car (expt 10 1024)))               
car: contract violation                 
  expected: pair?                       
  given: 10...                          
> (parameterize ([error-print-width 10])
    (car (expt 10 1024)))               
car: contract violation                 
  expected: pair?                       
  given: 1000000...                     
```

More generally, parameters implement a kind of dynamic binding. The
`make-parameter` function takes any value and returns a new parameter
that is initialized to the given value. Applying the parameter as a
function returns its current value:

```racket
> (define location (make-parameter "here"))
> (location)                               
"here"                                     
```

In a `parameterize` form, each `parameter-expr` must produce a
parameter. During the evaluation of the `body`s, each specified
parameter is given the result of the corresponding `value-expr`. When
control leaves the `parameterize` form—either through a normal return,
an exception, or some other escape—the parameter reverts to its earlier
value:

```racket
> (parameterize ([location "there"])               
    (location))                                    
"there"                                            
> (location)                                       
"here"                                             
> (parameterize ([location "in a house"])          
    (list (location)                               
          (parameterize ([location "with a mouse"])
            (location))                            
          (location)))                             
'("in a house" "with a mouse" "in a house")        
> (parameterize ([location "in a box"])            
    (car (location)))                              
car: contract violation                            
  expected: pair?                                  
  given: "in a box"                                
> (location)                                       
"here"                                             
```

The `parameterize` form is not a binding form like `let`; each use of
`location` above refers directly to the original definition. A
`parameterize` form adjusts the value of a parameter during the whole
time that the `parameterize` body is evaluated, even for uses of the
parameter that are textually outside of the `parameterize` body:

```racket
> (define (would-you-could-you?)            
    (and (not (equal? (location) "here"))   
         (not (equal? (location) "there"))))
> (would-you-could-you?)                    
#f                                          
> (parameterize ([location "on a bus"])     
    (would-you-could-you?))                 
#t                                          
```

If a use of a parameter is textually inside the body of a `parameterize`
but not evaluated before the `parameterize` form produces a value, then
the use does not see the value installed by the `parameterize` form:

```racket
> (let ([get (parameterize ([location "with a fox"])
               (lambda () (location)))])            
    (get))                                          
"here"                                              
```

The current binding of a parameter can be adjusted imperatively by
calling the parameter as a function with a value. If a `parameterize`
has adjusted the value of the parameter, then directly applying the
parameter procedure affects only the value associated with the active
`parameterize`:

```racket
> (define (try-again! where)             
    (location where))                    
> (location)                             
"here"                                   
> (parameterize ([location "on a train"])
    (list (location)                     
          (begin (try-again! "in a boat")
                 (location))))           
'("on a train" "in a boat")              
> (location)                             
"here"                                   
```

Using `parameterize` is generally preferable to updating a parameter
value imperatively—for much the same reasons that binding a fresh
variable with `let` is preferable to using `set!`  \(see Assignment:
`set!`\).

It may seem that variables and `set!` can solve many of the same
problems that parameters solve. For example, `lokation` could be defined
as a string, and `set!` could be used to adjust its value:

```racket
> (define lokation "here")                
> (define (would-ya-could-ya?)            
    (and (not (equal? lokation "here"))   
         (not (equal? lokation "there"))))
> (set! lokation "on a bus")              
> (would-ya-could-ya?)                    
#t                                        
```

Parameters, however, offer several crucial advantages over `set!`:

* The `parameterize` form helps automatically reset the value of a
  parameter when control escapes due to an exception. Adding exception
  handlers and other forms to rewind a `set!` is relatively tedious.

* Parameters work nicely with tail calls \(see \[missing\]\). The last
  `body` in a `parameterize` form is in tail position with respect to
  the `parameterize` form.

* Parameters work properly with threads \(see \[missing\]\). The
  `parameterize` form adjusts the value of a parameter only for
  evaluation in the current thread, which avoids race conditions with
  other threads.
