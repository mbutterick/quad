# Module Languages

When using the longhand `module` form for writing modules, the module
path that is specified after the new module’s name provides the initial
imports for the module. Since the initial-import module determines even
the most basic bindings that are available in a module’s body, such as
`require`, the initial import can be called a _module language_.

The most common module languages are `racket` or `racket/base`, but you
can define your own module language by defining a suitable module. For
example, using `provide` subforms like `all-from-out`, `except-out`, and
`rename-out`, you can add, remove, or rename bindings from `racket` to
produce a module language that is a variant of `racket`:

> +\[missing\] introduces the longhand `module` form.

```racket
> (module raquet racket                                    
    (provide (except-out (all-from-out racket) lambda)     
             (rename-out [lambda function])))              
> (module score 'raquet                                    
    (map (function (points) (case points                   
                             [(0) "love"] [(1) "fifteen"]  
                             [(2) "thirty"] [(3) "forty"]))
         (list 0 2)))                                      
> (require 'score)                                         
'("love" "thirty")                                         
```

## 1. Implicit Form Bindings

If you try to remove too much from `racket` in defining your own module
language, then the resulting module will no longer work right as a
module language:

```racket
> (module just-lambda racket                               
    (provide lambda))                                      
> (module identity 'just-lambda                            
    (lambda (x) x))                                        
eval:2:0: module: no #%module-begin binding in the module's
language                                                   
  in: (module identity (quote just-lambda) (lambda (x) x)) 
```

The `#%module-begin` form is an implicit form that wraps the body of a
module. It must be provided by a module that is to be used as module
language:

```racket
> (module just-lambda racket        
    (provide lambda #%module-begin))
> (module identity 'just-lambda     
    (lambda (x) x))                 
> (require 'identity)               
#<procedure>                        
```

The other implicit forms provided by `racket/base` are `#%app` for
function calls, `#%datum` for literals, and `#%top` for identifiers that
have no binding:

```racket
> (module just-lambda racket        
    (provide lambda #%module-begin  
             ; ten needs these, too:
             #%app #%datum))        
> (module ten 'just-lambda          
    ((lambda (x) x) 10))            
> (require 'ten)                    
10                                  
```

Implicit forms such as `#%app` can be used explicitly in a module, but
they exist mainly to allow a module language to restrict or change the
meaning of implicit uses. For example, a `lambda-calculus` module
language might restrict functions to a single argument, restrict
function calls to supply a single argument, restrict the module body to
a single expression, disallow literals, and treat unbound identifiers as
uninterpreted symbols:

```racket
> (module lambda-calculus racket                             
    (provide (rename-out [1-arg-lambda lambda]               
                         [1-arg-app #%app]                   
                         [1-form-module-begin #%module-begin]
                         [no-literals #%datum]               
                         [unbound-as-quoted #%top]))         
    (define-syntax-rule (1-arg-lambda (x) expr)              
      (lambda (x) expr))                                     
    (define-syntax-rule (1-arg-app e1 e2)                    
      (#%app e1 e2))                                         
    (define-syntax-rule (1-form-module-begin e)              
      (#%module-begin e))                                    
    (define-syntax (no-literals stx)                         
      (raise-syntax-error #f "no" stx))                      
    (define-syntax-rule (unbound-as-quoted . id)             
      'id))                                                  
> (module ok 'lambda-calculus                                
    ((lambda (x) (x z))                                      
     (lambda (y) y)))                                        
> (require 'ok)                                              
'z                                                           
> (module not-ok 'lambda-calculus                            
    (lambda (x y) x))                                        
eval:4:0: lambda: use does not match pattern: (lambda (x)    
expr)                                                        
  in: (lambda (x y) x)                                       
> (module not-ok 'lambda-calculus                            
    (lambda (x) x)                                           
    (lambda (y) (y y)))                                      
eval:5:0: #%module-begin: use does not match pattern:        
(#%module-begin e)                                           
  in: (#%module-begin (lambda (x) x) (lambda (y) (y y)))     
> (module not-ok 'lambda-calculus                            
    (lambda (x) (x x x)))                                    
eval:6:0: #%app: use does not match pattern: (#%app e1 e2)   
  in: (#%app x x x)                                          
> (module not-ok 'lambda-calculus                            
    10)                                                      
eval:7:0: #%datum: no                                        
  in: (#%datum . 10)                                         
```

Module languages rarely redefine `#%app`, `#%datum`, and `#%top`, but
redefining `#%module-begin` is more frequently useful. For example, when
using modules to construct descriptions of HTML pages where a
description is exported from the module as `page`, an alternate
`#%module-begin` can help eliminate `provide` and quasiquoting
boilerplate, as in `"html.rkt"`:

`"html.rkt"`
```racket
#lang racket                                          
(require racket/date)                                 
                                                      
(provide (except-out (all-from-out racket)            
                     #%module-begin)                  
         (rename-out [module-begin #%module-begin])   
         now)                                         
                                                      
(define-syntax-rule (module-begin expr ...)           
  (#%module-begin                                     
   (define page `(html expr ...))                     
   (provide page)))                                   
                                                      
(define (now)                                         
  (parameterize ([date-display-format 'iso-8601])     
    (date->string (seconds->date (current-seconds)))))
```

Using the `"html.rkt"` module language, a simple web page can be
described without having to explicitly define or export `page` and
starting in `quasiquote`d mode instead of expression mode:

```racket
> (module lady-with-the-spinning-head "html.rkt"                
    (title "Queen of Diamonds")                                 
    (p "Updated: " ,(now)))                                     
> (require 'lady-with-the-spinning-head)                        
> page                                                          
'(html (title "Queen of Diamonds") (p "Updated: " "2019-01-21"))
```

## 2. Using `#lang`` ``s-exp`

Implementing a language at the level of `#lang` is more complex than
declaring a single module, because `#lang` lets programmers control
several different facets of a language. The `s-exp` language, however,
acts as a kind of meta-language for using a module language with the
`#lang` shorthand:

```racket
#lang s-exp module-name
form ...               
```

is the same as

```racket
(module name module-name
  form ...)             
```

where `name` is derived from the source file containing the `#lang`
program. The name `s-exp` is short for “S-expression,” which is a
traditional name for Racket’s reader-level lexical conventions:
parentheses, identifiers, numbers, double-quoted strings with certain
backslash escapes, and so on.

Using `#lang s-exp`, the `lady-with-the-spinning-head` example from
before can be written more compactly as:

```racket
#lang s-exp "html.rkt"     
                           
(title "Queen of Diamonds")
(p "Updated: " ,(now))     
```

Later in this guide, \[missing\] explains how to define your own `#lang`
language, but first we explain how you can write reader-level extensions
to Racket.
