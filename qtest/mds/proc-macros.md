# General Macro Transformers

The `define-syntax` form creates a _transformer binding_ for an
identifier, which is a binding that can be used at compile time while
expanding expressions to be evaluated at run time. The compile-time
value associated with a transformer binding can be anything; if it is a
procedure of one argument, then the binding is used as a macro, and the
procedure is the _macro transformer_.

    1 Syntax Objects                                
                                                    
    2 Macro Transformer Procedures                  
                                                    
    3 Mixing Patterns and Expressions: `syntax-case`
                                                    
    4 `with-syntax` and `generate-temporaries`      
                                                    
    5 Compile and Run-Time Phases                   
                                                    
    6 General Phase Levels                          
      6.1 Phases and Bindings                       
      6.2 Phases and Modules                        
                                                    
    7 Syntax Taints                                 

## 1. Syntax Objects

The input and output of a macro transformer \(i.e., source and
replacement forms\) are represented as _syntax objects_. A syntax object
contains symbols, lists, and constant values \(such as numbers\) that
essentially correspond to the `quote`d form of the expression. For
example, a representation of the expression `(+ 1 2)` contains the
symbol `'+` and the numbers `1` and `2`, all in a list. In addition to
this quoted content, a syntax object associates source-location and
lexical-binding information with each part of the form. The
source-location information is used when reporting syntax errors \(for
example\), and the lexical-binding information allows the macro system
to maintain lexical scope. To accommodate this extra information, the
represention of the expression `(+ 1 2)` is not merely `'(+ 1 2)`, but a
packaging of `'(+ 1 2)` into a syntax object.

To create a literal syntax object, use the `syntax` form:

```racket
> (syntax (+ 1 2))        
#<syntax:eval:1:0 (+ 1 2)>
```

In the same way that `'` abbreviates `quote`, `#'` abbreviates `syntax`:

```racket
> #'(+ 1 2)               
#<syntax:eval:1:0 (+ 1 2)>
```

A syntax object that contains just a symbol is an _identifier syntax
object_. Racket provides some additional operations specific to
identifier syntax objects, including the `identifier?` operation to
detect identifiers. Most notably, `free-identifier=?`  determines
whether two identifiers refer to the same binding:

```racket
> (identifier? #'car)                           
#t                                              
> (identifier? #'(+ 1 2))                       
#f                                              
> (free-identifier=? #'car #'cdr)               
#f                                              
> (free-identifier=? #'car #'car)               
#t                                              
> (require (only-in racket/base [car also-car]))
> (free-identifier=? #'car #'also-car)          
#t                                              
```

To see the lists, symbols, numbers, etc. within a syntax object, use
`syntax->datum`:

```racket
> (syntax->datum #'(+ 1 2))
'(+ 1 2)                   
```

The `syntax-e` function is similar to `syntax->datum`, but it unwraps a
single layer of source-location and lexical-context information, leaving
sub-forms that have their own information wrapped as syntax objects:

```racket
> (syntax-e #'(+ 1 2))                                           
'(#<syntax:eval:1:0 +> #<syntax:eval:1:0 1> #<syntax:eval:1:0 2>)
```

The `syntax-e` function always leaves syntax-object wrappers around
sub-forms that are represented via symbols, numbers, and other literal
values. The only time it unwraps extra sub-forms is when unwrapping a
pair, in which case the `cdr` of the pair may be recursively unwrapped,
depending on how the syntax object was constructed.

The opposite of `syntax->datum` is, of course, `datum->syntax`.  In
addition to a datum like `'(+ 1 2)`, `datum->syntax` needs an existing
syntax object to donate its lexical context, and optionally another
syntax object to donate its source location:

```racket
> (datum->syntax #'lex    
                 '(+ 1 2) 
                 #'srcloc)
#<syntax:eval:1:0 (+ 1 2)>
```

In the above example, the lexical context of `#'lex` is used for the new
syntax object, while the source location of `#'srcloc` is used.

When the second \(i.e., the “datum”\) argument to `datum->syntax`
includes syntax objects, those syntax objects are preserved intact in
the result. That is, deconstructing the result with `syntax-e`
eventually produces the syntax objects that were given to
`datum->syntax`.

## 2. Macro Transformer Procedures

Any procedure of one argument can be a macro transformer.  As it turns
out, the `syntax-rules` form is a macro that expands to a procedure
form. For example, if you evaluate a `syntax-rules` form directly
\(instead of placing on the right-hand of a `define-syntax` form\), the
result is a procedure:

```racket
> (syntax-rules () [(nothing) something])
#<procedure>                             
```

Instead of using `syntax-rules`, you can write your own macro
transformer procedure directly using `lambda`. The argument to the
procedure is a syntax object that represents the source form, and the
result of the procedure must be a syntax object that represents the
replacement form:

```racket
> (define-syntax self-as-string                          
    (lambda (stx)                                        
      (datum->syntax stx                                 
                     (format "~s" (syntax->datum stx)))))
> (self-as-string (+ 1 2))                               
"(self-as-string (+ 1 2))"                               
```

The source form passed to a macro transformer represents an expression
in which its identifier is used in an application position \(i.e., after
a parenthesis that starts an expression\), or it represents the
identifier by itself if it is used as an expression position and not in
an application position.The procedure produced by `syntax-rules` raises
a syntax error if its argument corresponds to a use of the identifier by
itself, which is why `syntax-rules` does not implement an identifier
macro.

```racket
> (self-as-string (+ 1 2))
"(self-as-string (+ 1 2))"
> self-as-string          
"self-as-string"          
```

The `define-syntax` form supports the same shortcut syntax for functions
as `define`, so that the following `self-as-string` definition is
equivalent to the one that uses `lambda` explicitly:

```racket
> (define-syntax (self-as-string stx)                 
    (datum->syntax stx                                
                   (format "~s" (syntax->datum stx))))
> (self-as-string (+ 1 2))                            
"(self-as-string (+ 1 2))"                            
```

## 3. Mixing Patterns and Expressions: `syntax-case`

The procedure generated by `syntax-rules` internally uses `syntax-e` to
deconstruct the given syntax object, and it uses `datum->syntax` to
construct the result. The `syntax-rules` form doesn’t provide a way to
escape from pattern-matching and template-construction mode into an
arbitrary Racket expression.

The `syntax-case` form lets you mix pattern matching, template
construction, and arbitrary expressions:

```racket
(syntax-case stx-expr (literal-id ...)
  [pattern expr]                      
  ...)                                
```

Unlike `syntax-rules`, the `syntax-case` form does not produce a
procedure. Instead, it starts with a `stx-expr` expression that
determines the syntax object to match against the `pattern`s. Also, each
`syntax-case` clause has a `pattern` and `expr`, instead of a `pattern`
and `template`. Within an `expr`, the `syntax` form—usually abbreviated
with `#'`—shifts into template-construction mode; if the `expr` of a
clause starts with `#'`, then we have something like a `syntax-rules`
form:

```racket
> (syntax->datum              
   (syntax-case #'(+ 1 2) ()  
    [(op n1 n2) #'(- n1 n2)]))
'(- 1 2)                      
```

We could write the `swap` macro using `syntax-case` instead of
`define-syntax-rule` or `syntax-rules`:

```racket
(define-syntax (swap stx)           
  (syntax-case stx ()               
    [(swap x y) #'(let ([tmp x])    
                    (set! x y)      
                    (set! y tmp))]))
```

One advantage of using `syntax-case` is that we can provide better error
reporting for `swap`. For example, with the `define-syntax-rule`
definition of `swap`, then `(swap x 2)` produces a syntax error in terms
of `set!`, because `2` is not an identifier. We can refine our
`syntax-case` implementation of `swap` to explicitly check the
sub-forms:

```racket
(define-syntax (swap stx)                         
  (syntax-case stx ()                             
    [(swap x y)                                   
     (if (and (identifier? #'x)                   
              (identifier? #'y))                  
         #'(let ([tmp x])                         
             (set! x y)                           
             (set! y tmp))                        
         (raise-syntax-error #f                   
                             "not an identifier"  
                             stx                  
                             (if (identifier? #'x)
                                 #'y              
                                 #'x)))]))        
```

With this definition, `(swap x 2)` provides a syntax error originating
from `swap` instead of `set!`.

In the above definition of `swap`, `#'x` and `#'y` are templates, even
though they are not used as the result of the macro transformer. This
example illustrates how templates can be used to access pieces of the
input syntax, in this case for checking the form of the pieces. Also,
the match for `#'x` or `#'y` is used in the call to
`raise-syntax-error`, so that the syntax-error message can point
directly to the source location of the non-identifier.

## 4. `with-syntax` and `generate-temporaries`

Since `syntax-case` lets us compute with arbitrary Racket expressions,
we can more simply solve a problem that we had in writing
`define-for-cbr` \(see \[missing\]\), where we needed to generate a set
of names based on a sequence `id ...`:

```racket
(define-syntax (define-for-cbr stx)          
  (syntax-case stx ()                        
    [(_ do-f (id ...) body)                  
     ....                                    
       #'(define (do-f get ... put ...)      
           (define-get/put-id id get put) ...
           body) ....]))                     
```

In place of the `....`s above, we need to bind `get ...` and `put ...`
to lists of generated identifiers. We cannot use `let` to bind `get` and
`put`, because we need bindings that count as pattern variables, instead
of normal local variables. The `with-syntax` form lets us bind pattern
variables:

```racket
(define-syntax (define-for-cbr stx)          
  (syntax-case stx ()                        
    [(_ do-f (id ...) body)                  
     (with-syntax ([(get ...) ....]          
                   [(put ...) ....])         
       #'(define (do-f get ... put ...)      
           (define-get/put-id id get put) ...
           body))]))                         
```

Now we need an expression in place of `....` that generates as many
identifiers as there are `id` matches in the original pattern. Since
this is a common task, Racket provides a helper function,
`generate-temporaries`, that takes a sequence of identifiers and returns
a sequence of generated identifiers:

```racket
(define-syntax (define-for-cbr stx)                              
  (syntax-case stx ()                                            
    [(_ do-f (id ...) body)                                      
     (with-syntax ([(get ...) (generate-temporaries #'(id ...))] 
                   [(put ...) (generate-temporaries #'(id ...))])
       #'(define (do-f get ... put ...)                          
           (define-get/put-id id get put) ...                    
           body))]))                                             
```

This way of generating identifiers is normally easier to think about
than tricking the macro expander into generating names with purely
pattern-based macros.

In general, the left-hand side of a `with-syntax` binding is a pattern,
just like in `syntax-case`. In fact, a `with-syntax` form is just a
`syntax-case` form turned partially inside-out.

## 5. Compile and Run-Time Phases

As sets of macros get more complicated, you might want to write your own
helper functions, like `generate-temporaries`. For example, to provide
good syntax error messsages, `swap`, `rotate`, and `define-cbr` all
should check that certain sub-forms in the source form are identifiers.
We could use a `check-ids` function to perform this checking everywhere:

```racket
(define-syntax (swap stx)                  
  (syntax-case stx ()                      
    [(swap x y) (begin                     
                  (check-ids stx #'(x y))  
                  #'(let ([tmp x])         
                      (set! x y)           
                      (set! y tmp)))]))    
                                           
(define-syntax (rotate stx)                
  (syntax-case stx ()                      
    [(rotate a c ...)                      
     (begin                                
       (check-ids stx #'(a c ...))         
       #'(shift-to (c ... a) (a c ...)))]))
```

The `check-ids` function can use the `syntax->list` function to convert
a syntax-object wrapping a list into a list of syntax objects:

```racket
(define (check-ids stx forms)                 
  (for-each                                   
   (lambda (form)                             
     (unless (identifier? form)               
       (raise-syntax-error #f                 
                           "not an identifier"
                           stx                
                           form)))            
   (syntax->list forms)))                     
```

If you define `swap` and `check-ids` in this way, however, it doesn’t
work:

```racket
> (let ([a 1] [b 2]) (swap a b))                     
check-ids: undefined;                                
 cannot reference an identifier before its definition
  in module: top-level                               
```

The problem is that `check-ids` is defined as a run-time expression, but
`swap` is trying to use it at compile time. In interactive mode, compile
time and run time are interleaved, but they are not interleaved within
the body of a module, and they are not interleaved across modules that
are compiled ahead-of-time. To help make all of these modes treat code
consistently, Racket separates the binding spaces for different phases.

To define a `check-ids` function that can be referenced at compile time,
use `begin-for-syntax`:

```racket
(begin-for-syntax                               
  (define (check-ids stx forms)                 
    (for-each                                   
     (lambda (form)                             
       (unless (identifier? form)               
         (raise-syntax-error #f                 
                             "not an identifier"
                             stx                
                             form)))            
     (syntax->list forms))))                    
```

With this for-syntax definition, then `swap` works:

```racket
> (let ([a 1] [b 2]) (swap a b) (list a b))
'(2 1)                                     
> (swap a 1)                               
eval:13:0: swap: not an identifier         
  at: 1                                    
  in: (swap a 1)                           
```

When organizing a program into modules, you may want to put helper
functions in one module to be used by macros that reside on other
modules. In that case, you can write the helper function using `define`:

`"utils.rkt"`
```racket
#lang racket                                  
                                              
(provide check-ids)                           
                                              
(define (check-ids stx forms)                 
  (for-each                                   
   (lambda (form)                             
     (unless (identifier? form)               
       (raise-syntax-error #f                 
                           "not an identifier"
                           stx                
                           form)))            
   (syntax->list forms)))                     
```

Then, in the module that implements macros, import the helper function
using `(require (for-syntax "utils.rkt"))` instead of `(require
"utils.rkt")`:

```racket
#lang racket                             
                                         
(require (for-syntax "utils.rkt"))       
                                         
(define-syntax (swap stx)                
  (syntax-case stx ()                    
    [(swap x y) (begin                   
                  (check-ids stx #'(x y))
                  #'(let ([tmp x])       
                      (set! x y)         
                      (set! y tmp)))]))  
```

Since modules are separately compiled and cannot have circular
dependencies, the `"utils.rkt"` module’s run-time body can be compiled
before the compiling the module that implements `swap`.  Thus, the
run-time definitions in `"utils.rkt"` can be used to implement `swap`,
as long as they are explicitly shifted into compile time by `(require
(for-syntax ....))`.

The `racket` module provides `syntax-case`, `generate-temporaries`,
`lambda`, `if`, and more for use in both the run-time and compile-time
phases. That is why we can use `syntax-case` in the `racket` REPL both
directly and in the right-hand side of a `define-syntax` form.

The `racket/base` module, in contrast, exports those bindings only in
the run-time phase. If you change the module above that defines `swap`
so that it uses the `racket/base` language instead of `racket`, then it
no longer works. Adding `(require (for-syntax racket/base))` imports
`syntax-case` and more into the compile-time phase, so that the module
works again.

Suppose that `define-syntax` is used to define a local macro in the
right-hand side of a `define-syntax` form. In that case, the right-hand
side of the inner `define-syntax` is in the _meta-compile phase level_,
also known as _phase level 2_. To import `syntax-case` into that phase
level, you would have to use `(require (for-syntax (for-syntax
racket/base)))` or, equivalently, `(require (for-meta 2 racket/base))`.
For example,

```racket
#lang racket/base                                         
(require  ;; This provides the bindings for the definition
          ;; of shell-game.                               
          (for-syntax racket/base)                        
                                                          
          ;; And this for the definition of               
          ;; swap.                                        
          (for-syntax (for-syntax racket/base)))          
                                                          
(define-syntax (shell-game stx)                           
                                                          
  (define-syntax (swap stx)                               
    (syntax-case stx ()                                   
      [(_ a b)                                            
       #'(let ([tmp a])                                   
           (set! a b)                                     
           (set! b tmp))]))                               
                                                          
  (syntax-case stx ()                                     
    [(_ a b c)                                            
     (let ([a #'a] [b #'b] [c #'c])                       
       (when (= 0 (random 2)) (swap a b))                 
       (when (= 0 (random 2)) (swap b c))                 
       (when (= 0 (random 2)) (swap a c))                 
       #`(list #,a #,b #,c))]))                           
                                                          
(shell-game 3 4 5)                                        
(shell-game 3 4 5)                                        
(shell-game 3 4 5)                                        
```

Negative phase levels also exist. If a macro uses a helper function that
is imported `for-syntax`, and if the helper function returns
syntax-object constants generated by `syntax`, then identifiers in the
syntax will need bindings at _phase level -1_, also known as the
_template phase level_, to have any binding at the run-time phase level
relative to the module that defines the macro.

For instance, the `swap-stx` helper function in the example below is not
a syntax transformer—it’s just an ordinary function—but it produces
syntax objects that get spliced into the result of `shell-game`.
Therefore, its containing `helper` submodule needs to be imported at
`shell-game`’s phase 1 with `(require (for-syntax 'helper))`.

But from the perspective of `swap-stx`, its results will ultimately be
evaluated at phase level -1, when the syntax returned by `shell-game` is
evaluated. In other words, a negative phase level is a positive phase
level from the opposite direction: `shell-game`’s phase 1 is
`swap-stx`’s phase 0, so `shell-game`’s phase 0 is `swap-stx`’s phase
-1. And that’s why this example won’t work—the `'helper` submodule has
no bindings at phase -1.

```racket
#lang racket/base                 
(require (for-syntax racket/base))
                                  
(module helper racket/base        
  (provide swap-stx)              
  (define (swap-stx a-stx b-stx)  
    #`(let ([tmp #,a-stx])        
          (set! #,a-stx #,b-stx)  
          (set! #,b-stx tmp))))   
                                  
(require (for-syntax 'helper))    
                                  
(define-syntax (shell-game stx)   
  (syntax-case stx ()             
    [(_ a b c)                    
     #`(begin                     
         #,(swap-stx #'a #'b)     
         #,(swap-stx #'b #'c)     
         #,(swap-stx #'a #'c)     
         (list a b c))]))         
                                  
(define x 3)                      
(define y 4)                      
(define z 5)                      
(shell-game x y z)                
```

To repair this example, we add `(require (for-template racket/base))` to
the `'helper` submodule.

```racket
#lang racket/base                                                          
(require (for-syntax racket/base))                                         
                                                                           
(module helper racket/base                                                 
  (require (for-template racket/base)) ; binds `let` and `set!` at phase -1
  (provide swap-stx)                                                       
  (define (swap-stx a-stx b-stx)                                           
    #`(let ([tmp #,a-stx])                                                 
          (set! #,a-stx #,b-stx)                                           
          (set! #,b-stx tmp))))                                            
                                                                           
(require (for-syntax 'helper))                                             
                                                                           
(define-syntax (shell-game stx)                                            
  (syntax-case stx ()                                                      
    [(_ a b c)                                                             
     #`(begin                                                              
         #,(swap-stx #'a #'b)                                              
         #,(swap-stx #'b #'c)                                              
         #,(swap-stx #'a #'c)                                              
         (list a b c))]))                                                  
                                                                           
(define x 3)                                                               
(define y 4)                                                               
(define z 5)                                                               
(shell-game x y z)                                                         
(shell-game x y z)                                                         
(shell-game x y z)                                                         
```

## 6. General Phase Levels

A _phase_ can be thought of as a way to separate computations in a
pipeline of processes where one produces code that is used by the next.
\(E.g., a pipeline that consists of a preprocessor process, a compiler,
and an assembler.\)

Imagine starting two Racket processes for this purpose.  If you ignore
inter-process communication channels like sockets and files, the
processes will have no way to share anything other than the text that is
piped from the standard output of one process into the standard input of
the other.  Similarly, Racket effectively allows multiple invocations of
a module to exist in the same process but separated by phase.  Racket
enforces _separation_ of such phases, where different phases cannot
communicate in any way other than via the protocol of macro expansion,
where the output of one phases is the code used in the next.

### 6.1. Phases and Bindings

Every binding of an identifier exists in a particular phase.  The link
between a binding and its phase is represented by an integer _phase
level_.  Phase level 0 is the phase used for “plain” \(or “runtime”\)
definitions, so

`(define` `age` `5)`

adds a binding for `age` into phase level 0.  The identifier `age` can
be defined at a higher phase level using `begin-for-syntax`:

```racket
(begin-for-syntax
  (define age 5))
```

With a single `begin-for-syntax` wrapper, `age` is defined at phase
level 1.  We can easily mix these two definitions in the same module or
in a top-level namespace, and there is no clash between the two `age`s
that are defined at different phase levels:

```racket
> (define age 3)   
> (begin-for-syntax
    (define age 9))
```

The `age` binding at phase level 0 has a value of 3, and the `age`
binding at phase level 1 has a value of 9.

Syntax objects capture binding information as a first-class value. Thus,

`#'age`

is a syntax object that represents the `age` binding—but since there are
two `age`s \(one at phase level 0 and one at phase level 1\), which one
does it capture?  In fact, Racket imbues `#'age` with lexical
information for all phase levels, so the answer is that `#'age` captures
both.

The relevant binding of `age` captured by `#'age` is determined when
`#'age` is eventually used.  As an example, we bind `#'age` to a pattern
variable so we can use it in a template, and then we `eval`uate the
template: We use `eval` here to demonstrate phases, but see \[missing\]
for caveats about `eval`.

```racket
> (eval (with-syntax ([age #'age])
          #'(displayln age)))     
3                                 
```

The result is `3` because `age` is used at phase 0 level. We can try
again with the use of `age` inside `begin-for-syntax`:

```racket
> (eval (with-syntax ([age #'age])
          #'(begin-for-syntax     
              (displayln age))))  
9                                 
```

In this case, the answer is `9`, because we are using `age` at phase
level 1 instead of 0 \(i.e., `begin-for-syntax` evaluates its
expressions at phase level 1\). So, you can see that we started with the
same syntax object, `#'age`, and we were able to use it in two different
ways: at phase level 0 and at phase level 1.

A syntax object has a lexical context from the moment it first exists. A
syntax object that is provided from a module retains its lexical
context, and so it references bindings in the context of its source
module, not the context of its use.  The following example defines
`button` at phase level 0 and binds it to `0`, while `see-button` binds
the syntax object for `button` in module `a`:

```racket
> (module a racket                                           
    (define button 0)                                        
    (provide (for-syntax see-button))                        
    ; Why not (define see-button #'button)? We explain later.
    (define-for-syntax see-button #'button))                 
> (module b racket                                           
    (require 'a)                                             
    (define button 8)                                        
    (define-syntax (m stx)                                   
      see-button)                                            
    (m))                                                     
> (require 'b)                                               
0                                                            
```

The result of the `m` macro is the value of `see-button`, which is
`#'button` with the lexical context of the `a` module.  Even though
there is another `button` in `b`, the second `button` will not confuse
Racket, because the lexical context of `#'button` \(the value bound to
`see-button`\) is `a`.

Note that `see-button` is bound at phase level 1 by virtue of defining
it with `define-for-syntax`.  Phase level 1 is needed because `m` is a
macro, so its body executes at one phase higher than the context of its
definition.  Since `m` is defined at phase level 0, its body is at phase
level 1, so any bindings referenced by the body must be at phase level
1.

### 6.2. Phases and Modules

A phase level is a module-relative concept.  When importing from another
module via `require`, Racket lets us shift imported bindings to a phase
level that is different from the original one:

```racket
(require "a.rkt")                ; import with no phase shift
(require (for-syntax "a.rkt"))   ; shift phase by +1         
(require (for-template "a.rkt")) ; shift phase by -1         
(require (for-meta 5 "a.rkt"))   ; shift phase by +5         
```

That is, using `for-syntax` in `require` means that all of the bindings
from that module will have their phase levels increased by one.  A
binding that is `define`d at phase level 0 and imported with
`for-syntax` becomes a phase-level 1 binding:

```racket
> (module c racket                          
    (define x 0) ; defined at phase level 0 
    (provide x))                            
> (module d racket                          
    (require (for-syntax 'c))               
    ; has a binding at phase level 1, not 0:
    #'x)                                    
```

Let’s see what happens if we try to create a binding for the `#'button`
syntax object at phase level 0:

```racket
> (define button 0)           
> (define see-button #'button)
```

Now both `button` and `see-button` are defined at phase 0.  The lexical
context of `#'button` will know that there is a binding for `button` at
phase 0.  In fact, it seems like things are working just fine if we try
to `eval` `see-button`:

```racket
> (eval see-button)
0                  
```

Now, let’s use `see-button` in a macro:

```racket
> (define-syntax (m stx)                             
    see-button)                                      
> (m)                                                
see-button: undefined;                               
 cannot reference an identifier before its definition
  in module: top-level                               
```

Clearly, `see-button` is not defined at phase level 1, so we cannot
refer to it inside the macro body.  Let’s try to use `see-button` in
another module by putting the button definitions in a module and
importing it at phase level 1.  Then, we will get `see-button` at phase
level 1:

```racket
> (module a racket                                              
    (define button 0)                                           
    (define see-button #'button)                                
    (provide see-button))                                       
> (module b racket                                              
    (require (for-syntax 'a)) ; gets see-button at phase level 1
    (define-syntax (m stx)                                      
      see-button)                                               
    (m))                                                        
eval:1:0: button: unbound identifier;                           
 also, no #%top syntax transformer is bound                     
  in: button                                                    
```

Racket says that `button` is unbound now!  When `a` is imported at phase
level 1, we have the following bindings:

```racket
button     at phase level 1
see-button at phase level 1
```

So the macro `m` can see a binding for `see-button` at phase level 1 and
will return the `#'button` syntax object, which refers to `button`
binding at phase level 1.  But the use of `m` is at phase level 0, and
there is no `button` at phase level 0 in `b`.  That is why `see-button`
needs to be bound at phase level 1, as in the original `a`.  In the
original `b`, then, we have the following bindings:

```racket
button     at phase level 0
see-button at phase level 1
```

In this scenario, we can use `see-button` in the macro, since
`see-button` is bound at phase level 1.  When the macro expands, it will
refer to a `button` binding at phase level 0.

Defining `see-button` with `(define see-button #'button)` isn’t
inherently wrong; it depends on how we intend to use `see-button`.  For
example, we can arrange for `m` to sensibly use `see-button` because it
puts it in a phase level 1 context using `begin-for-syntax`:

```racket
> (module a racket                 
    (define button 0)              
    (define see-button #'button)   
    (provide see-button))          
> (module b racket                 
    (require (for-syntax 'a))      
    (define-syntax (m stx)         
      (with-syntax ([x see-button])
        #'(begin-for-syntax        
            (displayln x))))       
    (m))                           
0                                  
```

In this case, module `b` has both `button` and `see-button` bound at
phase level 1.  The expansion of the macro is

```racket
(begin-for-syntax    
  (displayln button))
```

which works, because `button` is bound at phase level 1.

Now, you might try to cheat the phase system by importing `a` at both
phase level 0 and phase level 1.  Then you would have the following
bindings

```racket
button     at phase level 0
see-button at phase level 0
button     at phase level 1
see-button at phase level 1
```

You might expect now that `see-button` in a macro would work, but it
doesn’t:

```racket
> (module a racket                         
    (define button 0)                      
    (define see-button #'button)           
    (provide see-button))                  
> (module b racket                         
    (require 'a                            
             (for-syntax 'a))              
    (define-syntax (m stx)                 
      see-button)                          
    (m))                                   
eval:1:0: button: unbound identifier;      
 also, no #%top syntax transformer is bound
  in: button                               
```

The `see-button` inside macro `m` comes from the `(for-syntax 'a)`
import.  For macro `m` to work, it needs to have `button` bound at phase
0. That binding exists—it’s implied by `(require 'a)`.  However,
`(require 'a)` and `(require (for-syntax 'a))` are _different
instantiations_ of the same module.  The `see-button` at phase 1 only
refers to the `button` at phase 1, not the `button` bound at phase 0
from a different instantiation—even from the same source module.

This kind of phase-level mismatch between instantiations can be repaired
with `syntax-shift-phase-level`. Recall that a syntax object like
`#'button` captures lexical information at _all_ phase levels. The
problem here is that `see-button` is invoked at phase 1, but needs to
return a syntax object that can be evaluated at phase 0. By default,
`see-button` is bound to `#'button` at the same phase level. But with
`syntax-shift-phase-level`, we can make `see-button` refer to `#'button`
at a different relative phase level. In this case, we use a phase shift
of `-1` to make `see-button` at phase 1 refer to `#'button` at phase 0.
\(Because the phase shift happens at every level, it will also make
`see-button` at phase 0 refer to `#'button` at phase -1.\)

Note that `syntax-shift-phase-level` merely creates a reference across
phases. To make that reference work, we still need to instantiate our
module at both phases so the reference and its target have their
bindings available. Thus, in module `'b`, we still import module `'a` at
both phase 0 and phase 1—using `(require 'a (for-syntax 'a))`—so we have
a phase-1 binding for `see-button` and a phase-0 binding for `button`.
Now macro `m` will work.

```racket
> (module a racket                                            
    (define button 0)                                         
    (define see-button (syntax-shift-phase-level #'button -1))
    (provide see-button))                                     
> (module b racket                                            
    (require 'a (for-syntax 'a))                              
    (define-syntax (m stx)                                    
      see-button)                                             
    (m))                                                      
> (require 'b)                                                
0                                                             
```

By the way, what happens to the `see-button` that’s bound at phase 0?
Its `#'button` binding has likewise been shifted, but to phase -1. Since
`button` itself isn’t bound at phase -1, if we try to evaluate
`see-button` at phase 0, we get an error. In other words, we haven’t
permanently cured our mismatch problem—we’ve just shifted it to a less
bothersome location.

```racket
> (module a racket                                            
    (define button 0)                                         
    (define see-button (syntax-shift-phase-level #'button -1))
    (provide see-button))                                     
> (module b racket                                            
    (require 'a (for-syntax 'a))                              
    (define-syntax (m stx)                                    
      see-button)                                             
    (m))                                                      
> (module b2 racket                                           
    (require 'a)                                              
    (eval see-button))                                        
> (require 'b2)                                               
button: undefined;                                            
 cannot reference an identifier before its definition         
  in module: top-level                                        
```

Mismatches like the one above can also arise when a macro tries to match
literal bindings—using `syntax-case` or `syntax-parse`.

```racket
> (module x racket                                 
    (require (for-syntax syntax/parse)             
             (for-template racket/base))           
                                                   
    (provide (all-defined-out))                    
                                                   
    (define button 0)                              
    (define (make) #'button)                       
    (define-syntax (process stx)                   
      (define-literal-set locals (button))         
      (syntax-parse stx                            
        [(_ (n (~literal button))) #'#''ok])))     
> (module y racket                                 
    (require (for-meta 1 'x)                       
             (for-meta 2 'x racket/base))          
                                                   
    (begin-for-syntax                              
      (define-syntax (m stx)                       
        (with-syntax ([out (make)])                
          #'(process (0 out)))))                   
                                                   
    (define-syntax (p stx)                         
      (m))                                         
                                                   
    (p))                                           
eval:2.0: process: expected the identifier `button'
  at: button                                       
  in: (process (0 button))                         
```

In this example, `make` is being used in `y` at phase level 2, and it
returns the `#'button` syntax object—which refers to `button` bound at
phase level 0 inside `x` and at phase level 2 in `y` from `(for-meta 2
'x)`.  The `process` macro is imported at phase level 1 from `(for-meta
1 'x)`, and it knows that `button` should be bound at phase level 1.
When the `syntax-parse` is executed inside `process`, it is looking for
`button` bound at phase level 1 but it sees only a phase level 2 binding
and doesn’t match.

To fix the example, we can provide `make` at phase level 1 relative to
`x`, and then we import it at phase level 1 in `y`:

```racket
> (module x racket                            
    (require (for-syntax syntax/parse)        
             (for-template racket/base))      
                                              
    (provide (all-defined-out))               
                                              
    (define button 0)                         
                                              
    (provide (for-syntax make))               
    (define-for-syntax (make) #'button)       
    (define-syntax (process stx)              
      (define-literal-set locals (button))    
      (syntax-parse stx                       
        [(_ (n (~literal button))) #'#''ok])))
> (module y racket                            
    (require (for-meta 1 'x)                  
             (for-meta 2 racket/base))        
                                              
    (begin-for-syntax                         
      (define-syntax (m stx)                  
        (with-syntax ([out (make)])           
          #'(process (0 out)))))              
                                              
    (define-syntax (p stx)                    
      (m))                                    
                                              
    (p))                                      
> (require 'y)                                
'ok                                           
```

## 7. Syntax Taints

A use of a macro can expand into a use of an identifier that is not
exported from the module that binds the macro. In general, such an
identifier must not be extracted from the expanded expression and used
in a different context, because using the identifier in a different
context may break invariants of the macro’s module.

For example, the following module exports a macro `go` that expands to a
use of `unchecked-go`:

`"m.rkt"`
```racket
#lang racket                             
(provide go)                             
                                         
(define (unchecked-go n x)               
  ; to avoid disaster, n must be a number
  (+ n 17))                              
                                         
(define-syntax (go stx)                  
  (syntax-case stx ()                    
   [(_ x)                                
    #'(unchecked-go 8 x)]))              
```

If the reference to `unchecked-go` is extracted from the expansion of
`(go 'a)`, then it might be inserted into a new expression,
`(unchecked-go #f 'a)`, leading to disaster. The `datum->syntax`
procedure can be used similarly to construct references to an unexported
identifier, even when no macro expansion includes a reference to the
identifier.

To prevent such abuses of unexported identifiers, the `go` macro must
explicitly protect its expansion by using `syntax-protect`:

```racket
(define-syntax (go stx)                     
  (syntax-case stx ()                       
   [(_ x)                                   
    (syntax-protect #'(unchecked-go 8 x))]))
```

The `syntax-protect` function causes any syntax object that is extracted
from the result of `go` to be _tainted_.  The macro expander rejects
tainted identifiers, so attempting to extract `unchecked-go` from the
expansion of `(go 'a)` produces an identifier that cannot be used to
construct a new expression \(or, at least, not one that the macro
expander will accept\). The `syntax-rules`, `syntax-id-rule`, and
`define-syntax-rule` forms automatically protect their expansion
results.

More precisely, `syntax-protect` _arms_ a syntax object with a _dye
pack_. When a syntax object is armed, then `syntax-e` taints any syntax
object in its result. Similarly, `datum->syntax` taints its result when
its first argument is armed. Finally, if any part of a quoted syntax
object is armed, then the corresponding part is tainted in the resulting
syntax constant.

Of course, the macro expander itself must be able to _disarm_ a taint on
a syntax object, so that it can further expand an expression or its
sub-expressions. When a syntax object is armed with a dye pack, the dye
pack has an associated inspector that can be used to disarm the dye
pack. A `(syntax-protect stx)` function call is actually a shorthand for
`(syntax-arm stx #f #t)`, which arms `stx` using a suitable inspector.
The expander uses `syntax-disarm` and with its inspector on every
expression before trying to expand or compile it.

In much the same way that the macro expander copies properties from a
syntax transformer’s input to its output \(see \[missing\]\), the
expander copies dye packs from a transformer’s input to its output.
Building on the previous example,

`"n.rkt"`
```racket
#lang racket                
(require "m.rkt")           
                            
(provide go-more)           
                            
(define y 'hello)           
                            
(define-syntax (go-more stx)
  (syntax-protect #'(go y)))
```

the expansion of `(go-more)` introduces a reference to the unexported
`y` in `(go y)`, and the expansion result is armed so that `y` cannot be
extracted from the expansion.  Even if `go` did not use `syntax-protect`
for its result \(perhaps because it does not need to protect
`unchecked-go` after all\), the dye pack on `(go y)` is propagated to
the final expansion `(unchecked-go 8 y)`. The macro expander uses
`syntax-rearm` to propagate dye packs from a transformer’s input to its
output.

### 7.1. Tainting Modes

In some cases, a macro implementor intends to allow limited
destructuring of a macro result without tainting the result. For
example, given the following `define-like-y` macro,

`"q.rkt"`
```racket
#lang racket                                            
                                                        
(provide define-like-y)                                 
                                                        
(define y 'hello)                                       
                                                        
(define-syntax (define-like-y stx)                      
  (syntax-case stx ()                                   
    [(_ id) (syntax-protect #'(define-values (id) y))]))
```

someone may use the macro in an internal definition:

```racket
(let ()            
  (define-like-y x)
  x)               
```

The implementor of the `"q.rkt"` module most likely intended to allow
such uses of `define-like-y`. To convert an internal definition into a
`letrec` binding, however, the `define` form produced by `define-like-y`
must be deconstructed, which would normally taint both the binding `x`
and the reference to `y`.

Instead, the internal use of `define-like-y` is allowed, because
`syntax-protect` treats specially a syntax list that begins with
`define-values`. In that case, instead of arming the overall expression,
each individual element of the syntax list is armed, pushing dye packs
further into the second element of the list so that they are attached to
the defined identifiers. Thus, `define-values`, `x`, and `y` in the
expansion result `(define-values (x) y)` are individually armed, and the
definition can be deconstructed for conversion to `letrec`.

Just like `syntax-protect`, the expander rearms a transformer result
that starts with `define-values`, by pushing dye packs into the list
elements. As a result, `define-like-y` could have been implemented to
produce `(define id y)`, which uses `define` instead of `define-values`.
In that case, the entire `define` form is at first armed with a dye
pack, but as the `define` form is expanded to `define-values`, the dye
pack is moved to the parts.

The macro expander treats syntax-list results starting with
`define-syntaxes` in the same way that it treats results starting with
`define-values`. Syntax-list results starting with `begin` are treated
similarly, except that the second element of the syntax list is treated
like all the other elements \(i.e., the immediate element is armed,
instead of its content\). Furthermore, the macro expander applies this
special handling recursively, in case a macro produces a `begin` form
that contains nested `define-values` forms.

The default application of dye packs can be overridden by attaching a
`'taint-mode` property \(see \[missing\]\) to the resulting syntax
object of a macro transformer. If the property value is `'opaque`, then
the syntax object is armed and not its parts. If the property value is
`'transparent`, then the syntax object’s parts are armed. If the
property value is `'transparent-binding`, then the syntax object’s parts
and the sub-parts of the second part \(as for `define-values` and
`define-syntaxes`\) are armed. The `'transparent` and
`'transparent-binding` modes trigger recursive property checking at the
parts, so that armings can be pushed arbitrarily deeply into a
transformer’s result.

### 7.2. Taints and Code Inspectors

Tools that are intended to be privileged \(such as a debugging
transformer\) must disarm dye packs in expanded programs.  Privilege is
granted through _code inspectors_. Each dye pack records an inspector,
and a syntax object can be disarmed using a sufficiently powerful
inspector.

When a module is declared, the declaration captures the current value of
the `current-code-inspector` parameter.  The captured inspector is used
when `syntax-protect` is applied by a macro transformer that is defined
within the module. A tool can disarm the resulting syntax object by
supplying `syntax-disarm` with an inspector that is the same or a
super-inspector of the module’s inspector. Untrusted code is ultimately
run after setting `current-code-inspector` to a less powerful inspector
\(after trusted code, such as debugging tools, have been loaded\).

With this arrangement, macro-generating macros require some care, since
the generating macro may embed syntax objects in the generated macro
that need to have the generating module’s protection level, rather than
the protection level of the module that contains the generated macro. To
avoid this problem, use the module’s declaration-time inspector, which
is accessible as `(variable-reference->module-declaration-inspector
(#%variable-reference))`, and use it to define a variant of
`syntax-protect`.

For example, suppose that the `go` macro is implemented through a macro:

```racket
#lang racket                                             
(provide def-go)                                         
                                                         
(define (unchecked-go n x)                               
  (+ n 17))                                              
                                                         
(define-syntax (def-go stx)                              
  (syntax-case stx ()                                    
    [(_ go)                                              
     (protect-syntax                                     
      #'(define-syntax (go stx)                          
          (syntax-case stx ()                            
            [(_ x)                                       
             (protect-syntax #'(unchecked-go 8 x))])))]))
```

When `def-go` is used inside another module to define `go`, and when the
`go`-defining module is at a different protection level than the
`def-go`-defining module, the generated macro’s use of `protect-syntax`
is not right.  The use of `unchecked-go` should be protected at the
level of the `def-go`-defining module, not the `go`-defining module.

The solution is to define and use `go-syntax-protect`, instead:

```racket
#lang racket                                                   
(provide def-go)                                               
                                                               
(define (unchecked-go n x)                                     
  (+ n 17))                                                    
                                                               
(define-for-syntax go-syntax-protect                           
  (let ([insp (variable-reference->module-declaration-inspector
               (#%variable-reference))])                       
    (lambda (stx) (syntax-arm stx insp))))                     
                                                               
(define-syntax (def-go stx)                                    
  (syntax-case stx ()                                          
    [(_ go)                                                    
     (protect-syntax                                           
      #'(define-syntax (go stx)                                
          (syntax-case stx ()                                  
           [(_ x)                                              
            (go-syntax-protect #'(unchecked-go 8 x))])))]))    
```

### 7.3. Protected Exports

Sometimes, a module needs to export bindings to some modules—other
modules that are at the same trust level as the exporting module—but
prevent access from untrusted modules. Such exports should use the
`protect-out` form in `provide`. For example, `ffi/unsafe` exports all
of its unsafe bindings as _protected_ in this sense.

Code inspectors, again, provide the mechanism for determining which
modules are trusted and which are untrusted. When a module is declared,
the value of `current-code-inspector` is associated to the module
declaration. When a module is instantiated \(i.e., when the body of the
declaration is actually executed\), a sub-inspector is created to guard
the module’s exports. Access to the module’s protected exports requires
a code inspector higher in the inspector hierarchy than the module’s
instantiation inspector; note that a module’s declaration inspector is
always higher than its instantiation inspector, so modules are declared
with the same code inspector can access each other’s exports.

Syntax-object constants within a module, such as literal identifiers in
a template, retain the inspector of their source module. In this way, a
macro from a trusted module can be used within an untrusted module, and
protected identifiers in the macro expansion still work, even through
they ultimately appear in an untrusted module. Naturally, such
identifiers should be armed, so that they cannot be extracted from the
macro expansion and abused by untrusted code.

Compiled code from a `".zo"` file is inherently untrustworthy,
unfortunately, since it can be synthesized by means other than
`compile`. When compiled code is written to a `".zo"` file,
syntax-object constants within the compiled code lose their inspectors.
All syntax-object constants within compiled code acquire the enclosing
module’s declaration-time inspector when the code is loaded.
