# Exceptions and Control

Racket provides an especially rich set of control operations—not only
operations for raising and catching exceptions, but also operations for
grabbing and restoring portions of a computation.

    1 Exceptions        
                        
    2 Prompts and Aborts
                        
    3 Continuations     

## 1. Exceptions

Whenever a run-time error occurs, an _exception_ is raised. Unless the
exception is caught, then it is handled by printing a message associated
with the exception, and then escaping from the computation.

```racket
> (/ 1 0)              
/: division by zero    
> (car 17)             
car: contract violation
  expected: pair?      
  given: 17            
```

To catch an exception, use the `with-handlers` form:

```racket
(with-handlers ([predicate-expr handler-expr] ...)
  body ...+)                                      
```

Each `predicate-expr` in a handler determines a kind of exception that
is caught by the `with-handlers` form, and the value representing the
exception is passed to the handler procedure produced by `handler-expr`.
The result of the `handler-expr` is the result of the `with-handlers`
expression.

For example, a divide-by-zero error raises an instance of the
`exn:fail:contract:divide-by-zero` structure type:

```racket
> (with-handlers ([exn:fail:contract:divide-by-zero?
                   (lambda (exn) +inf.0)])          
    (/ 1 0))                                        
+inf.0                                              
> (with-handlers ([exn:fail:contract:divide-by-zero?
                   (lambda (exn) +inf.0)])          
    (car 17))                                       
car: contract violation                             
  expected: pair?                                   
  given: 17                                         
```

The `error` function is one way to raise your own exception. It packages
an error message and other information into an `exn:fail` structure:

```racket
> (error "crash!")                                    
crash!                                                
> (with-handlers ([exn:fail? (lambda (exn) 'air-bag)])
    (error "crash!"))                                 
'air-bag                                              
```

The `exn:fail:contract:divide-by-zero` and `exn:fail` structure types
are sub-types of the `exn` structure type. Exceptions raised by core
forms and functions always raise an instance of `exn` or one of its
sub-types, but an exception does not have to be represented by a
structure. The `raise` function lets you raise any value as an
exception:

```racket
> (raise 2)                                                     
uncaught exception: 2                                           
> (with-handlers ([(lambda (v) (equal? v 2)) (lambda (v) 'two)])
    (raise 2))                                                  
'two                                                            
> (with-handlers ([(lambda (v) (equal? v 2)) (lambda (v) 'two)])
    (/ 1 0))                                                    
/: division by zero                                             
```

Multiple `predicate-expr`s in a `with-handlers` form let you handle
different kinds of exceptions in different ways. The predicates are
tried in order, and if none of them match, then the exception is
propagated to enclosing contexts.

```racket
> (define (always-fail n)                              
    (with-handlers ([even? (lambda (v) 'even)]         
                    [positive? (lambda (v) 'positive)])
      (raise n)))                                      
> (always-fail 2)                                      
'even                                                  
> (always-fail 3)                                      
'positive                                              
> (always-fail -3)                                     
uncaught exception: -3                                 
> (with-handlers ([negative? (lambda (v) 'negative)])  
   (always-fail -3))                                   
'negative                                              
```

Using `(lambda (v) #t)` as a predicate captures all exceptions, of
course:

```racket
> (with-handlers ([(lambda (v) #t) (lambda (v) 'oops)])
    (car 17))                                          
'oops                                                  
```

Capturing all exceptions is usually a bad idea, however. If the user
types Ctl-C in a terminal window or clicks the Stop button in DrRacket
to interrupt a computation, then normally the `exn:break` exception
should not be caught. To catch only exceptions that represent errors,
use `exn:fail?` as the predicate:

```racket
> (with-handlers ([exn:fail? (lambda (v) 'oops)])   
    (car 17))                                       
'oops                                               
> (with-handlers ([exn:fail? (lambda (v) 'oops)])   
    (break-thread (current-thread)) ; simulate Ctl-C
    (car 17))                                       
user break                                          
```

## 2. Prompts and Aborts

When an exception is raised, control escapes out of an arbitrary deep
evaluation context to the point where the exception is caught—or all the
way out if the exception is never caught:

```racket
> (+ 1 (+ 1 (+ 1 (+ 1 (+ 1 (+ 1 (/ 1 0)))))))
/: division by zero                          
```

But if control escapes “all the way out,” why does the REPL keep going
after an error is printed? You might think that it’s because the REPL
wraps every interaction in a `with-handlers` form that catches all
exceptions, but that’s not quite the reason.

The actual reason is that the REPL wraps the interaction with a
_prompt_, which effectively marks the evaluation context with an escape
point. If an exception is not caught, then information about the
exception is printed, and then evaluation _aborts_ to the nearest
enclosing prompt. More precisely, each prompt has a _prompt tag_, and
there is a designated _default prompt tag_ that the uncaught-exception
handler uses to abort.

The `call-with-continuation-prompt` function installs a prompt with a
given prompt tag, and then it evaluates a given thunk under the prompt.
The `default-continuation-prompt-tag` function returns the default
prompt tag. The `abort-current-continuation` function escapes to the
nearest enclosing prompt that has a given prompt tag.

```racket
> (define (escape v)                                   
    (abort-current-continuation                        
     (default-continuation-prompt-tag)                 
     (lambda () v)))                                   
> (+ 1 (+ 1 (+ 1 (+ 1 (+ 1 (+ 1 (escape 0)))))))       
0                                                      
> (+ 1                                                 
     (call-with-continuation-prompt                    
      (lambda ()                                       
        (+ 1 (+ 1 (+ 1 (+ 1 (+ 1 (+ 1 (escape 0))))))))
      (default-continuation-prompt-tag)))              
1                                                      
```

In `escape` above, the value `v` is wrapped in a procedure that is
called after escaping to the enclosing prompt.

Prompts and aborts look very much like exception handling and raising.
Indeed, prompts and aborts are essentially a more primitive form of
exceptions, and `with-handlers` and `raise` are implemented in terms of
prompts and aborts. The power of the more primitive forms is related to
the word “continuation” in the operator names, as we discuss in the next
section.

## 3. Continuations

A _continuation_ is a value that encapsulates a piece of an expression’s
evaluation context. The `call-with-composable-continuation` function
captures the _current continuation_ starting outside the current
function call and running up to the nearest enclosing prompt. \(Keep in
mind that each REPL interaction is implicitly wrapped in a prompt.\)

For example, in

`(+` `1` `(+` `1` `(+` `1` `0)))`

at the point where `0` is evaluated, the expression context includes
three nested addition expressions. We can grab that context by changing
`0` to grab the continuation before returning 0:

```racket
> (define saved-k #f)                            
> (define (save-it!)                             
    (call-with-composable-continuation           
     (lambda (k) ; k is the captured continuation
       (set! saved-k k)                          
       0)))                                      
> (+ 1 (+ 1 (+ 1 (save-it!))))                   
3                                                
```

The continuation saved in `save-k` encapsulates the program context `(+
1 (+ 1 (+ 1 ?)))`, where `?` represents a place to plug in a result
value—because that was the expression context when `save-it!` was
called. The continuation is encapsulated so that it behaves like the
function `(lambda (v) (+ 1 (+ 1 (+ 1 v))))`:

```racket
> (saved-k 0)          
3                      
> (saved-k 10)         
13                     
> (saved-k (saved-k 0))
6                      
```

The continuation captured by `call-with-composable-continuation` is
determined dynamically, not syntactically. For example, with

```racket
> (define (sum n)             
    (if (zero? n)             
        (save-it!)            
        (+ n (sum (sub1 n)))))
> (sum 5)                     
15                            
```

the continuation in `saved-k` becomes `(lambda (x) (+ 5 (+ 4 (+ 3 (+ 2
(+ 1 x))))))`:

```racket
> (saved-k 0) 
15            
> (saved-k 10)
25            
```

A more traditional continuation operator in Racket \(or Scheme\) is
`call-with-current-continuation`, which is usually abbreviated
`call/cc`. It is like `call-with-composable-continuation`, but applying
the captured continuation first aborts \(to the current prompt\) before
restoring the saved continuation. In addition, Scheme systems
traditionally support a single prompt at the program start, instead of
allowing new prompts via `call-with-continuation-prompt`. Continuations
as in Racket are sometimes called _delimited continuations_, since a
program can introduce new delimiting prompts, and continuations as
captured by `call-with-composable-continuation` are sometimes called
_composable continuations_, because they do not have a built-in abort.

For an example of how continuations are useful, see \[missing\]. For
specific control operators that have more convenient names than the
primitives described here, see `racket/control`.
