#lang qtest/markdown
# The Racket Guide

Matthew Flatt, Robert Bruce Findler, and PLT

This guide is intended for programmers who are new to Racket or new to
some part of Racket. It assumes programming experience, so if you are
new to programming, consider instead reading _[How to Design
Programs](http://www.htdp.org)_. If you want an especially quick
introduction to Racket, start with \[missing\].

Chapter 2 provides a brief introduction to Racket. From Chapter 3 on,
this guide dives into details—covering much of the Racket toolbox, but
leaving precise details to \[missing\] and other reference manuals.

> The source of this manual is available on
> [GitHub](https://github.com/racket/racket/tree/master/pkgs/racket-doc/scribblings/guide).


## 1. Welcome to Racket

Depending on how you look at it, **Racket** is

* a _programming language_—a dialect of Lisp and a descendant of Scheme;

  > See Dialects of Racket and Scheme for more information on other
  > dialects of Lisp and how they relate to Racket.

* a _family_ of programming languages—variants of Racket, and more; or

* a set of _tools_—for using a family of programming languages.

Where there is no room for confusion, we use simply _Racket_.

Racket’s main tools are

* **`racket`**, the core compiler, interpreter, and run-time system;

* **DrRacket**, the programming environment; and

* **`raco`**, a command-line tool for executing **Ra**cket **co**mmands
  that install packages, build libraries, and more.

Most likely, you’ll want to explore the Racket language using DrRacket,
especially at the beginning. If you prefer, you can also work with the
command-line `racket` interpreter and your favorite text editor; see
also Command-Line Tools and Your Editor of Choice. The rest of this
guide presents the language mostly independent of your choice of editor.

If you’re using DrRacket, you’ll need to choose the proper language,
because DrRacket accommodates many different variants of Racket, as well
as other languages. Assuming that you’ve never used DrRacket before,
start it up, type the line

`#lang` `racket`

in DrRacket’s top text area, and then click the Run button that’s above
the text area. DrRacket then understands that you mean to work in the
normal variant of Racket \(as opposed to the smaller `racket/base` or
many other possibilities\).

> More Rackets describes some of the other possibilities.

If you’ve used DrRacket before with something other than a program that
starts `#lang`, DrRacket will remember the last language that you used,
instead of inferring the language from the `#lang` line. In that case,
use the Language|Choose Language... menu item.  In the dialog that
appears, select the first item, which tells DrRacket to use the language
that is declared in a source program via `#lang`. Put the `#lang` line
above in the top text area, still.

### 1.1. Interacting with Racket

DrRacket’s bottom text area and the `racket` command-line program \(when
started with no options\) both act as a kind of calculator. You type a
Racket expression, hit the Return key, and the answer is printed. In the
terminology of Racket, this kind of calculator is called a
_read-eval-print loop_ or _REPL_.

A number by itself is an expression, and the answer is just the number:

```racket
> 5
5  
```

A string is also an expression that evaluates to itself. A string is
written with double quotes at the start and end of the string:

```racket
> "Hello, world!"
"Hello, world!"  
```

Racket uses parentheses to wrap larger expressions—almost any kind of
expression, other than simple constants. For example, a function call is
written: open parenthesis, function name, argument expression, and
closing parenthesis. The following expression calls the built-in
function `substring` with the arguments `"the boy out of the country"`,
`4`, and `7`:

```racket
> (substring "the boy out of the country" 4 7)
"boy"                                         
```

### 1.2. Definitions and Interactions

You can define your own functions that work like `substring` by using
the `define` form, like this:

```racket                               
(define (extract str)                   
  (substring str 4 7))                  
```                                     
                                        
```racket                               
> (extract "the boy out of the country")
"boy"                                   
> (extract "the country out of the boy")
"cou"                                   
```                                     

Although you can evaluate the `define` form in the REPL, definitions are
normally a part of a program that you want to keep and use later. So, in
DrRacket, you’d normally put the definition in the top text area—called
the _definitions area_—along with the `#lang` prefix:

```racket
#lang racket          
                      
(define (extract str) 
  (substring str 4 7))
```

If calling `(extract "the boy")` is part of the main action of your
program, that would go in the definitions area, too. But if it was just
an example expression that you were using to explore `extract`, then
you’d more likely leave the definitions area as above, click Run, and
then evaluate `(extract "the boy")` in the REPL.

When using command-line `racket` instead of DrRacket, you’d save the
above text in a file using your favorite editor. If you save it as
`"extract.rkt"`, then after starting `racket` in the same directory,
you’d evaluate the following sequence:

> If you use `xrepl`, you can use `,enter extract.rkt`.

```racket
> (enter! "extract.rkt")             
> (extract "the gal out of the city")
"gal"                                
```

The `enter!` form both loads the code and switches the evaluation
context to the inside of the module, just like DrRacket’s Run button.

### 1.3. Creating Executables

If your file \(or definitions area in DrRacket\) contains

```racket
#lang racket                      
                                  
(define (extract str)             
  (substring str 4 7))            
                                  
(extract "the cat out of the bag")
```

then it is a complete program that prints “cat” when run. You can run
the program within DrRacket or using `enter!` in `racket`, but if the
program is saved in >_src-filename_<, you can also run it from a command
line with

  `racket `>_src-filename_<

To package the program as an executable, you have a few options:

* In DrRacket, you can select the Racket|Create        Executable...
  menu item.

* From a command-line prompt, run `raco exe `>_src-filename_<, where
  >_src-filename_< contains the program. See \[missing\] for more
  information.

* With Unix or Mac OS, you can turn the program file into an executable
  script by inserting the line

  > See Scripts for more information on script files.

    `#! /usr/bin/env racket`

  at the very beginning of the file. Also, change the file permissions
  to executable using `chmod +x `>_filename_< on the command line.

  The script works as long as `racket` is in the user’s executable
  search path.  Alternately, use a full path to `racket` after `#!`
  \(with a space between `#!` and the path\), in which case the user’s
  executable search path does not matter.

### 1.4. A Note to Readers with Lisp/Scheme Experience

If you already know something about Scheme or Lisp, you might be tempted
to put just

```racket
(define (extract str) 
  (substring str 4 7))
```

into `"extract.rktl"` and run `racket` with

```racket
> (load "extract.rktl")  
> (extract "the dog out")
"dog"                    
```

That will work, because `racket` is willing to imitate a traditional
Lisp environment, but we strongly recommend against using `load` or
writing programs outside of a module.

Writing definitions outside of a module leads to bad error messages, bad
performance, and awkward scripting to combine and run programs. The
problems are not specific to `racket`; they’re fundamental limitations
of the traditional top-level environment, which Scheme and Lisp
implementations have historically fought with ad hoc command-line flags,
compiler directives, and build tools. The module system is designed to
avoid these problems, so start with `#lang`, and you’ll be happier with
Racket in the long run.

## 2. Racket Essentials

This chapter provides a quick introduction to Racket as background for
the rest of the guide. Readers with some Racket experience can safely
skip to Built-In Datatypes.

    2.1 Simple Values                                      
    2.2 Simple Definitions and Expressions                 
      2.2.1 Definitions                                    
      2.2.2 An Aside on Indenting Code                     
      2.2.3 Identifiers                                    
      2.2.4 Function Calls \(Procedure Applications\)      
      2.2.5 Conditionals with `if`, `and`, `or`, and `cond`
      2.2.6 Function Calls, Again                          
      2.2.7 Anonymous Functions with `lambda`              
      2.2.8 Local Binding with `define`, `let`, and `let*` 
    2.3 Lists, Iteration, and Recursion                    
      2.3.1 Predefined List Loops                          
      2.3.2 List Iteration from Scratch                    
      2.3.3 Tail Recursion                                 
      2.3.4 Recursion versus Iteration                     
    2.4 Pairs, Lists, and Racket Syntax                    
      2.4.1 Quoting Pairs and Symbols with `quote`         
      2.4.2 Abbreviating `quote` with `'`                  
      2.4.3 Lists and Racket Syntax     



### 2.1. Simple Values

Racket values include numbers, booleans, strings, and byte strings. In
DrRacket and documentation examples \(when you read the documentation in
color\), value expressions are shown in green.

_Numbers_ are written in the usual way, including fractions and
imaginary numbers:

> +Numbers \(later in this guide\) explains more about numbers.

```racket
1       3.14                  
1/2     6.02e+23              
1+2i    9999999999999999999999
```

_Booleans_ are `#t` for true and `#f` for false. In conditionals,
however, all non-`#f` values are treated as true.

> +Booleans \(later in this guide\) explains more about booleans.

_Strings_ are written between doublequotes. Within a string, backslash
is an escaping character; for example, a backslash followed by a
doublequote includes a literal doublequote in the string. Except for an
unescaped doublequote or backslash, any Unicode character can appear in
a string constant.

> +Strings \(Unicode\) \(later in this guide\) explains more about
> strings.

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

### 2.2. Simple Definitions and Expressions

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

#### 2.2.1. Definitions

A definition of the form

> +Definitions: `define` \(later in this guide\) explains more about
> definitions.

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

#### 2.2.2. An Aside on Indenting Code

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

#### 2.2.3. Identifiers

Racket’s syntax for identifiers is especially liberal. Excluding the
special characters

> +Identifiers and Binding \(later in this guide\) explains more about
> identifiers.

   `(` `)` `[` `]` `{` `}` `"` `,` `'``;` `#` `|` `\`

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


#### 2.2.4. Function Calls \(Procedure Applications\)

We have already seen many function calls, which are called _procedure
applications_ in more traditional terminology. The syntax of a function
call is

> +Function Calls \(later in this guide\) explains more about function
> calls.

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

#### 2.2.5. Conditionals with `if`, `and`, `or`, and `cond`

The next simplest kind of expression is an `if` conditional:

`(` `if` >_expr_< >_expr_< >_expr_< `)`

> +Conditionals \(later in this guide\) explains more about conditionals.

The first >_expr_< is always evaluated. If it produces a non-`#f` value,
then the second >_expr_< is evaluated for the result of the whole `if`
expression, otherwise the third >_expr_< is evaluated for the result.
                            
                                

