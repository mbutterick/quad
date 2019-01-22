# Racket Essentials

This chapter provides a quick introduction to Racket as background for
the rest of the guide. Readers with some Racket experience can safely
skip to \[missing\].

    1 Simple Values                                      
                                                         
    2 Simple Definitions and Expressions                 
      2.1 Definitions                                    
      2.2 An Aside on Indenting Code                     
      2.3 Identifiers                                    
      2.4 Function Calls \(Procedure Applications\)      
      2.5 Conditionals with `if`, `and`, `or`, and `cond`
      2.6 Function Calls, Again                          
      2.7 Anonymous Functions with `lambda`              
      2.8 Local Binding with `define`, `let`, and `let*` 
                                                         
    3 Lists, Iteration, and Recursion                    
      3.1 Predefined List Loops                          
      3.2 List Iteration from Scratch                    
      3.3 Tail Recursion                                 
      3.4 Recursion versus Iteration                     
                                                         
    4 Pairs, Lists, and Racket Syntax                    
      4.1 Quoting Pairs and Symbols with `quote`         
      4.2 Abbreviating `quote` with `'`                  
      4.3 Lists and Racket Syntax                        

## 1. Simple Values

Racket values include numbers, booleans, strings, and byte strings. In
DrRacket and documentation examples \(when you read the documentation in
color\), value expressions are shown in green.

_Numbers_ are written in the usual way, including fractions and
imaginary numbers:

> +\[missing\] \(later in this guide\) explains more about numbers.

```racket
1       3.14                  
1/2     6.02e+23              
1+2i    9999999999999999999999
```

_Booleans_ are `#t` for true and `#f` for false. In conditionals,
however, all non-`#f` values are treated as true.

> +\[missing\] \(later in this guide\) explains more about booleans.

_Strings_ are written between doublequotes. Within a string, backslash
is an escaping character; for example, a backslash followed by a
doublequote includes a literal doublequote in the string. Except for an
unescaped doublequote or backslash, any Unicode character can appear in
a string constant.

> +\[missing\] \(later in this guide\) explains more about strings.

```racket
"Hello, world!"            
"Benjamin \"Bugsy\" Siegel"
"λx:(μα.α→α).xx"           
```

When a constant is evaluated in the REPL, it typically prints the same
as its input syntax. In some cases, the printed form is a normalized
version of the input syntax. In documentation and in DrRacket’s REPL,
results are printed in blue instead of green to highlight the difference
between an input expression and a printed result.

Examples:

```racket
> 1.0000                         
1.0                              
> "Bugs \u0022Figaro\u0022 Bunny"
"Bugs \"Figaro\" Bunny"          
```

## 2. Simple Definitions and Expressions

A program module is written as

`#lang` >_langname_< >_topform_<\*

where a >_topform_< is either a >_definition_< or an >_expr_<. The REPL
also evaluates >_topform_<s.

In syntax specifications, text with a gray background, such as `#lang`,
represents literal text. Whitespace must appear between such literals
and nonterminals like >_id_<, except that whitespace is not required
before or after `(`, `)`, `[`, or `]`.  A comment, which starts with `;`
and runs until the end of the line, is treated the same as whitespace.

> +\[missing\] in \[missing\] provides more on different forms of
> comments.

Following the usual conventions, \* in a grammar means zero or more
repetitions of the preceding element, + means one or more repetitions of
the preceding element, and {} groups a sequence as an element for
repetition.

### 2.1. Definitions

A definition of the form

> +\[missing\] \(later in this guide\) explains more about definitions.

`(` `define` >_id_< >_expr_< `)`

binds >_id_< to the result of >_expr_<, while

`(` `define` `(` >_id_< >_id_<\* `)` >_expr_<+ `)`

binds the first >_id_< to a function \(also called a _procedure_\) that
takes arguments as named by the remaining >_id_<s. In the function case,
the >_expr_<s are the body of the function. When the function is called,
it returns the result of the last >_expr_<.

Examples:

```racket
(define pie 3)             ; defines pie to be 3        
                                                        
(define (piece str)        ; defines piece as a function
  (substring str 0 pie))   ;  of one argument           
                                                        
> pie                                                   
3                                                       
> (piece "key lime")                                    
"key"                                                   
```

Under the hood, a function definition is really the same as a
non-function definition, and a function name does not have to be used in
a function call. A function is just another kind of value, though the
printed form is necessarily less complete than the printed form of a
number or string.

Examples:

```racket
> piece               
#<procedure:piece>    
> substring           
#<procedure:substring>
```

A function definition can include multiple expressions for the
function’s body. In that case, only the value of the last expression is
returned when the function is called. The other expressions are
evaluated only for some side-effect, such as printing.

Examples:

```racket
(define (bake flavor)            
  (printf "preheating oven...\n")
  (string-append flavor " pie")) 
                                 
> (bake "apple")                 
preheating oven...               
"apple pie"                      
```

Racket programmers prefer to avoid side-effects, so a definition usually
has just one expression in its body. It’s important, though, to
understand that multiple expressions are allowed in a definition body,
because it explains why the following `nobake` function fails to include
its argument in its result:

```racket                      
(define (nobake flavor)        
  string-append flavor "jello")
```                            
                               
```racket                      
> (nobake "green")             
"jello"                        
```                            

Within `nobake`, there are no parentheses around `string-append flavor
"jello"`, so they are three separate expressions instead of one
function-call expression. The expressions `string-append` and `flavor`
are evaluated, but the results are never used. Instead, the result of
the function is just the result of the final expression, `"jello"`.

### 2.2. An Aside on Indenting Code

Line breaks and indentation are not significant for parsing Racket
programs, but most Racket programmers use a standard set of conventions
to make code more readable. For example, the body of a definition is
typically indented under the first line of the definition. Identifiers
are written immediately after an open parenthesis with no extra space,
and closing parentheses never go on their own line.

DrRacket automatically indents according to the standard style when you
type Enter in a program or REPL expression. For example, if you hit
Enter after typing `(define (greet name)`, then DrRacket automatically
inserts two spaces for the next line.  If you change a region of code,
you can select it in DrRacket and hit Tab, and DrRacket will re-indent
the code \(without inserting any line breaks\). Editors like Emacs offer
a Racket or Scheme mode with similar indentation support.

Re-indenting not only makes the code easier to read, it gives you extra
feedback that your parentheses match in the way that you intended. For
example, if you leave out a closing parenthesis after the last argument
to a function, automatic indentation starts the next line under the
first argument, instead of under the `define` keyword:

```racket
(define (halfbake flavor                                  
                  (string-append flavor " creme brulee")))
```

In this case, indentation helps highlight the mistake. In other cases,
where the indentation may be normal while an open parenthesis has no
matching close parenthesis, both `racket` and DrRacket use the source’s
indentation to suggest where a parenthesis might be missing.

### 2.3. Identifiers

Racket’s syntax for identifiers is especially liberal. Excluding the
special characters

> +\[missing\] \(later in this guide\) explains more about identifiers.

   `(` `)` `[` `]` `{` `}` `"` `,` `'` ` `;` `#` `|` `\`

and except for the sequences of characters that make number constants,
almost any sequence of non-whitespace characters forms an >_id_<. For
example `substring` is an identifier. Also, `string-append` and `a+b`
are identifiers, as opposed to arithmetic expressions. Here are several
more examples:

```racket
+                              
Hfuhruhurr                     
integer?                       
pass/fail                      
john-jacob-jingleheimer-schmidt
a-b-c+1-2-3                    
```

### 2.4. Function Calls \(Procedure Applications\)

We have already seen many function calls, which are called _procedure
applications_ in more traditional terminology. The syntax of a function
call is

> +\[missing\] \(later in this guide\) explains more about function calls.

`(` >_id_< >_expr_<\* `)`

where the number of >_expr_<s determines the number of arguments
supplied to the function named by >_id_<.

The `racket` language pre-defines many function identifiers, such as
`substring` and `string-append`. More examples are below.

In example Racket code throughout the documentation, uses of pre-defined
names are hyperlinked to the reference manual. So, you can click on an
identifier to get full details about its use.

```racket
> (string-append "rope" "twine" "yarn")  ; append strings       
"ropetwineyarn"                                                 
> (substring "corduroys" 0 4)            ; extract a substring  
"cord"                                                          
> (string-length "shoelace")             ; get a string's length
8                                                               
> (string? "Ceci n'est pas une string.") ; recognize strings    
#t                                                              
> (string? 1)                                                   
#f                                                              
> (sqrt 16)                              ; find a square root   
4                                                               
> (sqrt -16)                                                    
0+4i                                                            
> (+ 1 2)                                ; add numbers          
3                                                               
> (- 2 1)                                ; subtract numbers     
1                                                               
> (< 2 1)                                ; compare numbers      
#f                                                              
> (>= 2 1)                                                      
#t                                                              
> (number? "c'est une number")           ; recognize numbers    
#f                                                              
> (number? 1)                                                   
#t                                                              
> (equal? 6 "half dozen")                ; compare anything     
#f                                                              
> (equal? 6 6)                                                  
#t                                                              
> (equal? "half dozen" "half dozen")                            
#t                                                              
```

### 2.5. Conditionals with `if`, `and`, `or`, and `cond`

The next simplest kind of expression is an `if` conditional:

`(` `if` >_expr_< >_expr_< >_expr_< `)`

> +\[missing\] \(later in this guide\) explains more about conditionals.

The first >_expr_< is always evaluated. If it produces a non-`#f` value,
then the second >_expr_< is evaluated for the result of the whole `if`
expression, otherwise the third >_expr_< is evaluated for the result.

Example:

```racket
> (if (> 2 3)   
      "bigger"  
      "smaller")
"smaller"       
```

```racket                               
(define (reply s)                       
  (if (equal? "hello" (substring s 0 5))
      "hi!"                             
      "huh?"))                          
```                                     
                                        
```racket                               
> (reply "hello racket")                
"hi!"                                   
> (reply "λx:(μα.α→α).xx")              
"huh?"                                  
```                                     

Complex conditionals can be formed by nesting `if` expressions. For
example, you could make the `reply` function work when given
non-strings:

```racket
(define (reply s)                           
  (if (string? s)                           
      (if (equal? "hello" (substring s 0 5))
          "hi!"                             
          "huh?")                           
      "huh?"))                              
```

Instead of duplicating the `"huh?"` case, this function is better
written as

```racket
(define (reply s)                           
  (if (if (string? s)                       
          (equal? "hello" (substring s 0 5))
          #f)                               
      "hi!"                                 
      "huh?"))                              
```

but these kinds of nested `if`s are difficult to read.  Racket provides
more readable shortcuts through the `and` and `or` forms, which work
with any number of expressions:

> +\[missing\] \(later in this guide\) explains more about `and` and `or`.

```racket
( and >_expr_<* )
( or >_expr_<* ) 
```

The `and` form short-circuits: it stops and returns `#f` when an
expression produces `#f`, otherwise it keeps going. The `or` form
similarly short-circuits when it encounters a true result.

Examples:

```racket
(define (reply s)                             
  (if (and (string? s)                        
           (>= (string-length s) 5)           
           (equal? "hello" (substring s 0 5)))
      "hi!"                                   
      "huh?"))                                
                                              
> (reply "hello racket")                      
"hi!"                                         
> (reply 17)                                  
"huh?"                                        
```

Another common pattern of nested `if`s involves a sequence of tests,
each with its own result:

```racket
(define (reply-more s)                                          
  (if (equal? "hello" (substring s 0 5))                        
      "hi!"                                                     
      (if (equal? "goodbye" (substring s 0 7))                  
          "bye!"                                                
          (if (equal? "?" (substring s (- (string-length s) 1)))
              "I don't know"                                    
              "huh?"))))                                        
```

The shorthand for a sequence of tests is the `cond` form:

> +\[missing\] \(later in this guide\) explains more about `cond`.

`(` `cond` {`[` >_expr_< >_expr_<\* `]`}\* `)`

A `cond` form contains a sequence of clauses between square brackets. In
each clause, the first >_expr_< is a test expression. If it produces
true, then the clause’s remaining >_expr_<s are evaluated, and the last
one in the clause provides the answer for the entire `cond` expression;
the rest of the clauses are ignored. If the test >_expr_< produces `#f`,
then the clause’s remaining >_expr_<s are ignored, and evaluation
continues with the next clause. The last clause can use `else` as a
synonym for a `#t` test expression.

Using `cond`, the `reply-more` function can be more clearly written as
follows:

```racket                                             
(define (reply-more s)                                
  (cond                                               
   [(equal? "hello" (substring s 0 5))                
    "hi!"]                                            
   [(equal? "goodbye" (substring s 0 7))              
    "bye!"]                                           
   [(equal? "?" (substring s (- (string-length s) 1)))
    "I don't know"]                                   
   [else "huh?"]))                                    
```                                                   
                                                      
```racket                                             
> (reply-more "hello racket")                         
"hi!"                                                 
> (reply-more "goodbye cruel world")                  
"bye!"                                                
> (reply-more "what is your favorite color?")         
"I don't know"                                        
> (reply-more "mine is lime green")                   
"huh?"                                                
```                                                   

The use of square brackets for `cond` clauses is a convention. In
Racket, parentheses and square brackets are actually interchangeable, as
long as `(` is matched with `)` and `[` is matched with `]`. Using
square brackets in a few key places makes Racket code even more
readable.

### 2.6. Function Calls, Again

In our earlier grammar of function calls, we oversimplified.  The actual
syntax of a function call allows an arbitrary expression for the
function, instead of just an >_id_<:

> +\[missing\] \(later in this guide\) explains more about function calls.

`(` >_expr_< >_expr_<\* `)`

The first >_expr_< is often an >_id_<, such as `string-append` or `+`,
but it can be anything that evaluates to a function. For example, it can
be a conditional expression:

```racket                                
(define (double v)                       
  ((if (string? v) string-append +) v v))
```                                      
                                         
```racket                                
> (double "mnah")                        
"mnahmnah"                               
> (double 5)                             
10                                       
```                                      

Syntactically, the first expression in a function call could even be a
number—but that leads to an error, since a number is not a function.

```racket
> (1 2 3 4)                                           
application: not a procedure;                         
 expected a procedure that can be applied to arguments
  given: 1                                            
  arguments...:                                       
   2                                                  
   3                                                  
   4                                                  
```

When you accidentally omit a function name or when you use extra
parentheses around an expression, you’ll most often get an “expected a
procedure” error like this one.

### 2.7. Anonymous Functions with `lambda`

Programming in Racket would be tedious if you had to name all of your
numbers. Instead of writing `(+ 1 2)`, you’d have to write

> +\[missing\] \(later in this guide\) explains more about `lambda`.

```racket
> (define a 1)
> (define b 2)
> (+ a b)     
3             
```

It turns out that having to name all your functions can be tedious, too.
For example, you might have a function `twice` that takes a function and
an argument. Using `twice` is convenient if you already have a name for
the function, such as `sqrt`:

```racket          
(define (twice f v)
  (f (f v)))       
```                
                   
```racket          
> (twice sqrt 16)  
2                  
```                

If you want to call a function that is not yet defined, you could define
it, and then pass it to `twice`:

```racket               
(define (louder s)      
  (string-append s "!"))
```                     
                        
```racket               
> (twice louder "hello")
"hello!!"               
```                     

But if the call to `twice` is the only place where `louder` is used,
it’s a shame to have to write a whole definition. In Racket, you can use
a `lambda` expression to produce a function directly. The `lambda` form
is followed by identifiers for the function’s arguments, and then the
function’s body expressions:

`(` `lambda` `(` >_id_<\* `)` >_expr_<+ `)`

Evaluating a `lambda` form by itself produces a function:

```racket
> (lambda (s) (string-append s "!"))
#<procedure>                        
```

Using `lambda`, the above call to `twice` can be re-written as

```racket
> (twice (lambda (s) (string-append s "!")) 
         "hello")                           
"hello!!"                                   
> (twice (lambda (s) (string-append s "?!"))
         "hello")                           
"hello?!?!"                                 
```

Another use of `lambda` is as a result for a function that generates
functions:

```racket                                
(define (make-add-suffix s2)             
  (lambda (s) (string-append s s2)))     
```                                      
                                         
```racket                                
> (twice (make-add-suffix "!") "hello")  
"hello!!"                                
> (twice (make-add-suffix "?!") "hello") 
"hello?!?!"                              
> (twice (make-add-suffix "...") "hello")
"hello......"                            
```                                      

Racket is a _lexically scoped_ language, which means that `s2` in the
function returned by `make-add-suffix` always refers to the argument for
the call that created the function. In other words, the
`lambda`-generated function “remembers” the right `s2`:

```racket
> (define louder (make-add-suffix "!"))   
> (define less-sure (make-add-suffix "?"))
> (twice less-sure "really")              
"really??"                                
> (twice louder "really")                 
"really!!"                                
```

We have so far referred to definitions of the form `(define `>_id_<`
`>_expr_<`)` as “non-function definitions.” This characterization is
misleading, because the >_expr_< could be a `lambda` form, in which case
the definition is equivalent to using the “function” definition form.
For example, the following two definitions of `louder` are equivalent:

```racket                  
(define (louder s)         
  (string-append s "!"))   
                           
(define louder             
  (lambda (s)              
    (string-append s "!")))
```                        
                           
```racket                  
> louder                   
#<procedure:louder>        
```                        

Note that the expression for `louder` in the second case is an
“anonymous” function written with `lambda`, but, if possible, the
compiler infers a name, anyway, to make printing and error reporting as
informative as possible.

### 2.8. Local Binding with
`define`, `let`, and `let*`

It’s time to retract another simplification in our grammar of Racket. In
the body of a function, definitions can appear before the body
expressions:

> +\[missing\] \(later in this guide\) explains more about local
> \(internal\) definitions.

```racket
( define ( >_id_< >_id_<* ) >_definition_<* >_expr_<+ )
( lambda ( >_id_<* ) >_definition_<* >_expr_<+ )       
```

Definitions at the start of a function body are local to the function
body.

Examples:

```racket
(define (converse s)                                    
  (define (starts? s2) ; local to converse              
    (define len2 (string-length s2))  ; local to starts?
    (and (>= (string-length s) len2)                    
         (equal? s2 (substring s 0 len2))))             
  (cond                                                 
   [(starts? "hello") "hi!"]                            
   [(starts? "goodbye") "bye!"]                         
   [else "huh?"]))                                      
                                                        
> (converse "hello!")                                   
"hi!"                                                   
> (converse "urp")                                      
"huh?"                                                  
> starts? ; outside of converse, so...                  
starts?: undefined;                                     
 cannot reference an identifier before its definition   
  in module: top-level                                  
```

Another way to create local bindings is the `let` form. An advantage of
`let` is that it can be used in any expression position. Also, `let`
binds many identifiers at once, instead of requiring a separate `define`
for each identifier.

> +\[missing\] \(later in this guide\) explains more about `let` and
> `let*`.

`(` `let` `(` {`[` >_id_< >_expr_< `]`}\* `)` >_expr_<+ `)`

Each binding clause is an >_id_< and an >_expr_< surrounded by square
brackets, and the expressions after the clauses are the body of the
`let`. In each clause, the >_id_< is bound to the result of the >_expr_<
for use in the body.

```racket
> (let ([x (random 4)]    
        [o (random 4)])   
    (cond                 
     [(> x o) "X wins"]   
     [(> o x) "O wins"]   
     [else "cat's game"]))
"O wins"                  
```

The bindings of a `let` form are available only in the body of the
`let`, so the binding clauses cannot refer to each other. The `let*`
form, in contrast, allows later clauses to use earlier bindings:

```racket
> (let* ([x (random 4)]                         
         [o (random 4)]                         
         [diff (number->string (abs (- x o)))]) 
    (cond                                       
     [(> x o) (string-append "X wins by " diff)]
     [(> o x) (string-append "O wins by " diff)]
     [else "cat's game"]))                      
"O wins by 2"                                   
```

## 3. Lists, Iteration, and Recursion

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

### 3.1. Predefined List Loops

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

### 3.2. List Iteration from Scratch

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

### 3.3. Tail Recursion

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

### 3.4. Recursion versus Iteration

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

## 4. Pairs, Lists, and Racket Syntax

The `cons` function actually accepts any two values, not just a list for
the second argument. When the second argument is not `empty` and not
itself produced by `cons`, the result prints in a special way. The two
values joined with `cons` are printed between parentheses, but with a
dot \(i.e., a period surrounded by whitespace\) in between:

```racket
> (cons 1 2)             
'(1 . 2)                 
> (cons "banana" "split")
'("banana" . "split")    
```

Thus, a value produced by `cons` is not always a list. In general, the
result of `cons` is a _pair_. The more traditional name for the `cons?`
function is `pair?`, and we’ll use the traditional name from now on.

The name `rest` also makes less sense for non-list pairs; the more
traditional names for `first` and `rest` are `car` and `cdr`,
respectively. \(Granted, the traditional names are also nonsense. Just
remember that “a” comes before “d,” and `cdr` is pronounced
“could-er.”\)

Examples:

```racket
> (car (cons 1 2))    
1                     
> (cdr (cons 1 2))    
2                     
> (pair? empty)       
#f                    
> (pair? (cons 1 2))  
#t                    
> (pair? (list 1 2 3))
#t                    
```

Racket’s pair datatype and its relation to lists is essentially a
historical curiosity, along with the dot notation for printing and the
funny names `car` and `cdr`. Pairs are deeply wired into to the culture,
specification, and implementation of Racket, however, so they survive in
the language.

You are perhaps most likely to encounter a non-list pair when making a
mistake, such as accidentally reversing the arguments to `cons`:

```racket
> (cons (list 2 3) 1)
'((2 3) . 1)         
> (cons 1 (list 2 3))
'(1 2 3)             
```

Non-list pairs are used intentionally, sometimes. For example, the
`make-hash` function takes a list of pairs, where the `car` of each pair
is a key and the `cdr` is an arbitrary value.

The only thing more confusing to new Racketeers than non-list pairs is
the printing convention for pairs where the second element _is_ a pair,
but _is not_ a list:

```racket
> (cons 0 (cons 1 2))
'(0 1 . 2)           
```

In general, the rule for printing a pair is as follows: use the dot
notation unless the dot is immediately followed by an open parenthesis.
In that case, remove the dot, the open parenthesis, and the matching
close parenthesis. Thus, `'(0 . (1 . 2))` becomes `'(0 1 . 2)`, and `'(1
. (2 . (3 . ())))` becomes `'(1 2 3)`.

### 4.1. Quoting Pairs and Symbols with `quote`

A list prints with a quote mark before it, but if an element of a list
is itself a list, then no quote mark is printed for the inner list:

```racket
> (list (list 1) (list 2 3) (list 4))
'((1) (2 3) (4))                     
```

For nested lists, especially, the `quote` form lets you write a list as
an expression in essentially the same way that the list prints:

```racket
> (quote ("red" "green" "blue"))
'("red" "green" "blue")         
> (quote ((1) (2 3) (4)))       
'((1) (2 3) (4))                
> (quote ())                    
'()                             
```

The `quote` form works with the dot notation, too, whether the quoted
form is normalized by the dot-parenthesis elimination rule or not:

```racket
> (quote (1 . 2))      
'(1 . 2)               
> (quote (0 . (1 . 2)))
'(0 1 . 2)             
```

Naturally, lists of any kind can be nested:

```racket
> (list (list 1 2 3) 5 (list "a" "b" "c"))
'((1 2 3) 5 ("a" "b" "c"))                
> (quote ((1 2 3) 5 ("a" "b" "c")))       
'((1 2 3) 5 ("a" "b" "c"))                
```

If you wrap an identifier with `quote`, then you get output that looks
like an identifier, but with a `'` prefix:

```racket
> (quote jane-doe)
'jane-doe         
```

A value that prints like a quoted identifier is a _symbol_. In the same
way that parenthesized output should not be confused with expressions, a
printed symbol should not be confused with an identifier. In particular,
the symbol `(quote map)` has nothing to do with the `map` identifier or
the predefined function that is bound to `map`, except that the symbol
and the identifier happen to be made up of the same letters.

Indeed, the intrinsic value of a symbol is nothing more than its
character content. In this sense, symbols and strings are almost the
same thing, and the main difference is how they print. The functions
`symbol->string` and `string->symbol` convert between them.

Examples:

```racket
> map                         
#<procedure:map>              
> (quote map)                 
'map                          
> (symbol? (quote map))       
#t                            
> (symbol? map)               
#f                            
> (procedure? map)            
#t                            
> (string->symbol "map")      
'map                          
> (symbol->string (quote map))
"map"                         
```

In the same way that `quote` for a list automatically applies itself to
nested lists, `quote` on a parenthesized sequence of identifiers
automatically applies itself to the identifiers to create a list of
symbols:

```racket
> (car (quote (road map)))          
'road                               
> (symbol? (car (quote (road map))))
#t                                  
```

When a symbol is inside a list that is printed with `'`, the `'` on the
symbol is omitted, since `'` is doing the job already:

```racket
> (quote (road map))
'(road map)         
```

The `quote` form has no effect on a literal expression such as a number
or string:

```racket
> (quote 42)             
42                       
> (quote "on the record")
"on the record"          
```

### 4.2. Abbreviating `quote` with `'`

As you may have guessed, you can abbreviate a use of `quote` by just
putting `'` in front of a form to quote:

```racket
> '(1 2 3)                     
'(1 2 3)                       
> 'road                        
'road                          
> '((1 2 3) road ("a" "b" "c"))
'((1 2 3) road ("a" "b" "c"))  
```

In the documentation, `'` within an expression is printed in green along
with the form after it, since the combination is an expression that is a
constant. In DrRacket, only the `'` is colored green. DrRacket is more
precisely correct, because the meaning of `quote` can vary depending on
the context of an expression. In the documentation, however, we
routinely assume that standard bindings are in scope, and so we paint
quoted forms in green for extra clarity.

A `'` expands to a `quote` form in quite a literal way. You can see this
if you put a `'` in front of a form that has a `'`:

```racket
> (car ''road)       
'quote               
> (car '(quote road))
'quote               
```

The `'` abbreviation works in output as well as input. The REPL’s
printer recognizes the symbol `'quote` as the first element of a
two-element list when printing output, in which case it uses `’` to
print the output:

```racket
> (quote (quote road))
”road                 
> '(quote road)       
”road                 
> ''road              
”road                 
```

### 4.3. Lists and Racket Syntax

Now that you know the truth about pairs and lists, and now that you’ve
seen `quote`, you’re ready to understand the main way in which we have
been simplifying Racket’s true syntax.

The syntax of Racket is not defined directly in terms of character
streams. Instead, the syntax is determined by two layers:

* a _reader_ layer, which turns a sequence of characters into lists,
  symbols, and other constants; and

* an _expander_ layer, which processes the lists, symbols, and other
  constants to parse them as an expression.

The rules for printing and reading go together. For example, a list is
printed with parentheses, and reading a pair of parentheses produces a
list. Similarly, a non-list pair is printed with the dot notation, and a
dot on input effectively runs the dot-notation rules in reverse to
obtain a pair.

One consequence of the read layer for expressions is that you can use
the dot notation in expressions that are not quoted forms:

```racket
> (+ 1 . (2))
3            
```

This works because `(+ 1 . (2))` is just another way of writing `(+ 1
2)`. It is practically never a good idea to write application
expressions using this dot notation; it’s just a consequence of the way
Racket’s syntax is defined.

Normally, `.` is allowed by the reader only with a parenthesized
sequence, and only before the last element of the sequence. However, a
pair of `.`s can also appear around a single element in a parenthesized
sequence, as long as the element is not first or last. Such a pair
triggers a reader conversion that moves the element between `.`s to the
front of the list. The conversion enables a kind of general infix
notation:

```racket
> (1 . < . 2) 
#t            
> '(1 . < . 2)
'(< 1 2)      
```

This two-dot convention is non-traditional, and it has essentially
nothing to do with the dot notation for non-list pairs. Racket
programmers use the infix convention sparingly—mostly for asymmetric
binary operators such as `<` and `is-a?`.
