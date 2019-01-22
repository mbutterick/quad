# Simple Definitions and Expressions

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

## 1. Definitions

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

## 2. An Aside on Indenting Code

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

## 3. Identifiers

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

## 4. Function Calls \(Procedure Applications\)

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

## 5. Conditionals with `if`, `and`, `or`, and `cond`

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

## 6. Function Calls, Again

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

## 7. Anonymous Functions with `lambda`

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

## 8. Local Binding with
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
"X wins"                  
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
"O wins by 1"                                   
```
