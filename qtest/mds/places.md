# Parallelism with Places

The `racket/place` library provides support for performance improvement
through parallelism with the `place` form. The `place` form creates a
_place_, which is effectively a new Racket instance that can run in
parallel to other places, including the initial place.  The full power
of the Racket language is available at each place, but places can
communicate only through message passing—using the `place-channel-put`
and `place-channel-get` functions on a limited set of values—which helps
ensure the safety and independence of parallel computations.

As a starting example, the racket program below uses a place to
determine whether any number in the list has a double that is also in
the list:

```racket
#lang racket                            
                                        
(provide main)                          
                                        
(define (any-double? l)                 
  (for/or ([i (in-list l)])             
    (for/or ([i2 (in-list l)])          
      (= i2 (* 2 i)))))                 
                                        
(define (main)                          
  (define p                             
    (place ch                           
      (define l (place-channel-get ch)) 
      (define l-double? (any-double? l))
      (place-channel-put ch l-double?)))
                                        
  (place-channel-put p (list 1 2 4 8))  
                                        
  (place-channel-get p))                
```

The identifier `ch` after `place` is bound to a _place channel_. The
remaining body expressions within the `place` form are evaluated in a
new place, and the body expressions use `ch` to communicate with the
place that spawned the new place.

In the body of the `place` form above, the new place receives a list of
numbers over `ch` and binds the list to `l`.  It then calls
`any-double?` on the list and binds the result to `l-double?`. The final
body expression sends the `l-double?` result back to the original place
over `ch`.

In DrRacket, after saving and running the above program, evaluate
`(main)` in the interactions window to create the new place. When using
places inside DrRacket, the module containg place code must be saved to
a file before it will execute.  Alternatively, save the program as
`"double.rkt"` and run from a command line with

  `racket -tm double.rkt`

where the `-t` flag tells `racket` to load the `double.rkt` module, the
`-m` flag calls the exported `main` function, and `-tm` combines the two
flags.

The `place` form has two subtle features. First, it lifts the `place`
body to an anonymous, module-level function.  This lifting means that
any binding referenced by the `place` body must be available in the
module’s top level. Second, the `place` form `dynamic-require`s the
enclosing module in a newly created place. As part of the
`dynamic-require`, the current module body is evaluated in the new
place.  The consequence of this second feature is that `place` should
not appear immediately in a module or in a function that is called in a
module’s top level; otherwise, invoking the module will invoke the same
module in a new place, and so on, triggering a cascade of place
creations that will soon exhaust memory.

```racket
#lang racket                                    
                                                
(provide main)                                  
                                                
; Don't do this!                                
(define p (place ch (place-channel-get ch)))    
                                                
(define (indirect-place-invocation)             
  (define p2 (place ch (place-channel-get ch))))
                                                
; Don't do this, either!                        
(indirect-place-invocation)                     
```
