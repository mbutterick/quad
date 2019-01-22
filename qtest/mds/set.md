# Assignment: `set!`

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

## 1. Guidelines for Using Assignment

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

## 2. Multiple Values: `set!-values`

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
