# Local Binding

Although internal `define`s can be used for local binding, Racket
provides three forms that give the programmer more control over
bindings: `let`, `let*`, and `letrec`.

## 1. Parallel Binding: `let`

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

## 2. Sequential Binding: `let*`

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

## 3. Recursive Binding: `letrec`

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

## 4. Named `let`

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

## 5. Multiple Values: `let-values`, `let*-values`, `letrec-values`

> +\[missing\] in \[missing\] also documents multiple-value binding forms.

In the same way that `define-values` binds multiple results in a
definition \(see \[missing\]\), `let-values`, `let*-values`, and
`letrec-values` bind multiple results locally.

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
