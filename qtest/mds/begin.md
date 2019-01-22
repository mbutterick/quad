# Sequencing

Racket programmers prefer to write programs with as few side-effects as
possible, since purely functional code is more easily tested and
composed into larger programs. Interaction with the external
environment, however, requires sequencing, such as when writing to a
display, opening a graphical window, or manipulating a file on disk.

## 1. Effects Before: `begin`

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

## 2. Effects After: `begin0`

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
Start: 1548117334950.433                                   
End..: 1548117335053.375                                   
0                                                          
> (log-times (lambda () (values 1 2)))                     
Start: 1548117335053.996                                   
End..: 1548117335054.022                                   
1                                                          
2                                                          
```

## 3. Effects If...: `when` and `unless`

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
