# Units \(Components\)

_Units_ organize a program into separately compilable and reusable
_components_. A unit resembles a procedure in that both are first-class
values that are used for abstraction. While procedures abstract over
values in expressions, units abstract over names in collections of
definitions. Just as a procedure is called to evaluate its expressions
given actual arguments for its formal parameters, a unit is _invoked_ to
evaluate its definitions given actual references for its imported
variables. Unlike a procedure, however, a unit’s imported variables can
be partially linked with the exported variables of another unit _prior
to invocation_. Linking merges multiple units together into a single
compound unit. The compound unit itself imports variables that will be
propagated to unresolved imported variables in the linked units, and
re-exports some variables from the linked units for further linking.

    1 Signatures and Units               
                                         
    2 Invoking Units                     
                                         
    3 Linking Units                      
                                         
    4 First-Class Units                  
                                         
    5 Whole-`module` Signatures and Units
                                         
    6 Contracts for Units                
      6.1 Adding Contracts to Signatures 
      6.2 Adding Contracts to Units      
                                         
    7 `unit` versus `module`             

## 1. Signatures and Units

The interface of a unit is described in terms of _signatures_. Each
signature is defined \(normally within a `module`\) using
`define-signature`.  For example, the following signature, placed in a
`"toy-factory-sig.rkt"` file, describes the exports of a component that
implements a toy factory:

> By convention, signature names end with `^`.

`"toy-factory-sig.rkt"`
```racket
#lang racket                                
                                            
(define-signature toy-factory^              
  (build-toys  ; (integer? -> (listof toy?))
   repaint     ; (toy? symbol? -> toy?)     
   toy?        ; (any/c -> boolean?)        
   toy-color)) ; (toy? -> symbol?)          
                                            
(provide toy-factory^)                      
```

An implementation of the `toy-factory^` signature is written using
`define-unit` with an `export` clause that names `toy-factory^`:

> By convention, unit names end with `@`.

`"simple-factory-unit.rkt"`
```racket
#lang racket                               
                                           
(require "toy-factory-sig.rkt")            
                                           
(define-unit simple-factory@               
  (import)                                 
  (export toy-factory^)                    
                                           
  (printf "Factory started.\n")            
                                           
  (define-struct toy (color) #:transparent)
                                           
  (define (build-toys n)                   
    (for/list ([i (in-range n)])           
      (make-toy 'blue)))                   
                                           
  (define (repaint t col)                  
    (make-toy col)))                       
                                           
(provide simple-factory@)                  
```

The `toy-factory^` signature also could be referenced by a unit that
needs a toy factory to implement something else. In that case,
`toy-factory^` would be named in an `import` clause. For example, a toy
store would get toys from a toy factory. \(Suppose, for the sake of an
example with interesting features, that the store is willing to sell
only toys in a particular color.\)

`"toy-store-sig.rkt"`
```racket
#lang racket                            
                                        
(define-signature toy-store^            
  (store-color     ; (-> symbol?)       
   stock!          ; (integer? -> void?)
   get-inventory)) ; (-> (listof toy?)) 
                                        
(provide toy-store^)                    
```

`"toy-store-unit.rkt"`
```racket
#lang racket                             
                                         
(require "toy-store-sig.rkt"             
         "toy-factory-sig.rkt")          
                                         
(define-unit toy-store@                  
  (import toy-factory^)                  
  (export toy-store^)                    
                                         
  (define inventory null)                
                                         
  (define (store-color) 'green)          
                                         
  (define (maybe-repaint t)              
    (if (eq? (toy-color t) (store-color))
        t                                
        (repaint t (store-color))))      
                                         
  (define (stock! n)                     
    (set! inventory                      
          (append inventory              
                  (map maybe-repaint     
                       (build-toys n)))))
                                         
  (define (get-inventory) inventory))    
                                         
(provide toy-store@)                     
```

Note that `"toy-store-unit.rkt"` imports `"toy-factory-sig.rkt"`, but
not `"simple-factory-unit.rkt"`.  Consequently, the `toy-store@` unit
relies only on the specification of a toy factory, not on a specific
implementation.

## 2. Invoking Units

The `simple-factory@` unit has no imports, so it can be invoked directly
using `invoke-unit`:

```racket
> (require "simple-factory-unit.rkt")
> (invoke-unit simple-factory@)      
Factory started.                     
```

The `invoke-unit` form does not make the body definitions available,
however, so we cannot build any toys with this factory. The
`define-values/invoke-unit` form binds the identifiers of a signature to
the values supplied by a unit \(to be invoked\) that implements the
signature:

```racket
> (define-values/invoke-unit/infer simple-factory@)
Factory started.                                   
> (build-toys 3)                                   
(list (toy 'blue) (toy 'blue) (toy 'blue))         
```

Since `simple-factory@` exports the `toy-factory^` signature, each
identifier in `toy-factory^` is defined by the
`define-values/invoke-unit/infer` form. The `/infer` part of the form
name indicates that the identifiers bound by the declaration are
inferred from `simple-factory@`.

Now that the identifiers in `toy-factory^` are defined, we can also
invoke `toy-store@`, which imports `toy-factory^` to produce
`toy-store^`:

```racket
> (require "toy-store-unit.rkt")              
> (define-values/invoke-unit/infer toy-store@)
> (get-inventory)                             
'()                                           
> (stock! 2)                                  
> (get-inventory)                             
(list (toy 'green) (toy 'green))              
```

Again, the `/infer` part `define-values/invoke-unit/infer` determines
that `toy-store@` imports `toy-factory^`, and so it supplies the
top-level bindings that match the names in `toy-factory^` as imports to
`toy-store@`.

## 3. Linking Units

We can make our toy economy more efficient by having toy factories that
cooperate with stores, creating toys that do not have to be repainted.
Instead, the toys are always created using the store’s color, which the
factory gets by importing `toy-store^`:

`"store-specific-factory-unit.rkt"`
```racket
#lang racket                          
                                      
(require "toy-store-sig.rkt"          
         "toy-factory-sig.rkt")       
                                      
(define-unit store-specific-factory@  
  (import toy-store^)                 
  (export toy-factory^)               
                                      
  (define-struct toy () #:transparent)
                                      
  (define (toy-color t) (store-color))
                                      
  (define (build-toys n)              
    (for/list ([i (in-range n)])      
      (make-toy)))                    
                                      
  (define (repaint t col)             
    (error "cannot repaint")))        
                                      
(provide store-specific-factory@)     
```

To invoke `store-specific-factory@`, we need `toy-store^` bindings to
supply to the unit. But to get `toy-store^` bindings by invoking
`toy-store@`, we will need a toy factory! The unit implementations are
mutually dependent, and we cannot invoke either before the other.

The solution is to _link_ the units together, and then we can invoke the
combined units. The `define-compound-unit/infer` form links any number
of units to form a combined unit. It can propagate imports and exports
from the linked units, and it can satisfy each unit’s imports using the
exports of other linked units.

```racket
> (require "toy-factory-sig.rkt")               
> (require "toy-store-sig.rkt")                 
> (require "store-specific-factory-unit.rkt")   
> (define-compound-unit/infer toy-store+factory@
    (import)                                    
    (export toy-factory^ toy-store^)            
    (link store-specific-factory@               
          toy-store@))                          
```

The overall result above is a unit `toy-store+factory@` that exports
both `toy-factory^` and `toy-store^`. The connection between
`store-specific-factory@` and `toy-store@` is inferred from the
signatures that each imports and exports.

This unit has no imports, so we can always invoke it:

```racket
> (define-values/invoke-unit/infer toy-store+factory@)
> (stock! 2)                                          
> (get-inventory)                                     
(list (toy) (toy))                                    
> (map toy-color (get-inventory))                     
'(green green)                                        
```

## 4. First-Class Units

The `define-unit` form combines `define` with a `unit` form, similar to
the way that `(define (f x) ....)`  combines `define` followed by an
identifier with an implicit `lambda`.

Expanding the shorthand, the definition of `toy-store@` could almost be
written as

```racket
(define toy-store@              
  (unit                         
   (import toy-factory^)        
   (export toy-store^)          
                                
   (define inventory null)      
                                
   (define (store-color) 'green)
   ....))                       
```

A difference between this expansion and `define-unit` is that the
imports and exports of `toy-store@` cannot be inferred. That is, besides
combining `define` and `unit`, `define-unit` attaches static information
to the defined identifier so that its signature information is available
statically to `define-values/invoke-unit/infer` and other forms.

Despite the drawback of losing static signature information, `unit` can
be useful in combination with other forms that work with first-class
values. For example, we could wrap a `unit` that creates a toy store in
a `lambda` to supply the store’s color:

`"toy-store-maker.rkt"`
```racket
#lang racket                                
                                            
(require "toy-store-sig.rkt"                
         "toy-factory-sig.rkt")             
                                            
(define toy-store@-maker                    
  (lambda (the-color)                       
    (unit                                   
     (import toy-factory^)                  
     (export toy-store^)                    
                                            
     (define inventory null)                
                                            
     (define (store-color) the-color)       
                                            
     ; the rest is the same as before       
                                            
     (define (maybe-repaint t)              
       (if (eq? (toy-color t) (store-color))
           t                                
           (repaint t (store-color))))      
                                            
     (define (stock! n)                     
       (set! inventory                      
             (append inventory              
                     (map maybe-repaint     
                          (build-toys n)))))
                                            
     (define (get-inventory) inventory))))  
                                            
(provide toy-store@-maker)                  
```

To invoke a unit created by `toy-store@-maker`, we must use
`define-values/invoke-unit`, instead of the `/infer` variant:

```racket
> (require "simple-factory-unit.rkt")                  
> (define-values/invoke-unit/infer simple-factory@)    
Factory started.                                       
> (require "toy-store-maker.rkt")                      
> (define-values/invoke-unit (toy-store@-maker 'purple)
    (import toy-factory^)                              
    (export toy-store^))                               
> (stock! 2)                                           
> (get-inventory)                                      
(list (toy 'purple) (toy 'purple))                     
```

In the `define-values/invoke-unit` form, the `(import toy-factory^)`
line takes bindings from the current context that match the names in
`toy-factory^` \(the ones that we created by invoking
`simple-factory@`\), and it supplies them as imports to `toy-store@`.
The `(export toy-store^)` clause indicates that the unit produced by
`toy-store@-maker` will export `toy-store^`, and the names from that
signature are defined after invoking the unit.

To link a unit from `toy-store@-maker`, we can use the `compound-unit`
form:

```racket
> (require "store-specific-factory-unit.rkt")                
> (define toy-store+factory@                                 
    (compound-unit                                           
     (import)                                                
     (export TF TS)                                          
     (link [((TF : toy-factory^)) store-specific-factory@ TS]
           [((TS : toy-store^)) toy-store@ TF])))            
```

This `compound-unit` form packs a lot of information into one place. The
left-hand-side `TF` and `TS` in the `link` clause are binding
identifiers. The identifier `TF` is essentially bound to the elements of
`toy-factory^` as implemented by `store-specific-factory@`.  The
identifier `TS` is similarly bound to the elements of `toy-store^` as
implemented by `toy-store@`. Meanwhile, the elements bound to `TS` are
supplied as imports for `store-specific-factory@`, since `TS` follows
`store-specific-factory@`. The elements bound to `TF` are similarly
supplied to `toy-store@`. Finally, `(export TF TS)` indicates that the
elements bound to `TF` and `TS` are exported from the compound unit.

The above `compound-unit` form uses `store-specific-factory@` as a
first-class unit, even though its information could be inferred. Every
unit can be used as a first-class unit, in addition to its use in
inference contexts. Also, various forms let a programmer bridge the gap
between inferred and first-class worlds. For example,
`define-unit-binding` binds a new identifier to the unit produced by an
arbitrary expression; it statically associates signature information to
the identifier, and it dynamically checks the signatures against the
first-class unit produced by the expression.

## 5. Whole-`module` Signatures and Units

In programs that use units, modules like `"toy-factory-sig.rkt"` and
`"simple-factory-unit.rkt"` are common. The `racket/signature` and
`racket/unit` module names can be used as languages to avoid much of the
boilerplate module, signature, and unit declaration text.

For example, `"toy-factory-sig.rkt"` can be written as

```racket
#lang racket/signature                   
                                         
build-toys  ; (integer? -> (listof toy?))
repaint     ; (toy? symbol? -> toy?)     
toy?        ; (any/c -> boolean?)        
toy-color   ; (toy? -> symbol?)          
```

The signature `toy-factory^` is automatically provided from the module,
inferred from the filename `"toy-factory-sig.rkt"` by replacing the
`"-sig.rkt"` suffix with `^`.

Similarly, `"simple-factory-unit.rkt"` module can be written

```racket
#lang racket/unit                        
                                         
(require "toy-factory-sig.rkt")          
                                         
(import)                                 
(export toy-factory^)                    
                                         
(printf "Factory started.\n")            
                                         
(define-struct toy (color) #:transparent)
                                         
(define (build-toys n)                   
  (for/list ([i (in-range n)])           
    (make-toy 'blue)))                   
                                         
(define (repaint t col)                  
  (make-toy col))                        
```

The unit `simple-factory@` is automatically provided from the module,
inferred from the filename `"simple-factory-unit.rkt"` by replacing the
`"-unit.rkt"` suffix with `@`.

## 6. Contracts for Units

There are a couple of ways of protecting units with contracts.  One way
is useful when writing new signatures, and the other handles the case
when a unit must conform to an already existing signature.

### 6.1. Adding Contracts to Signatures

When contracts are added to a signature, then all units which implement
that signature are protected by those contracts.  The following version
of the `toy-factory^` signature adds the contracts previously written in
comments:

`"contracted-toy-factory-sig.rkt"`
```racket
#lang racket                                
                                            
(define-signature contracted-toy-factory^   
  ((contracted                              
    [build-toys (-> integer? (listof toy?))]
    [repaint    (-> toy? symbol? toy?)]     
    [toy?       (-> any/c boolean?)]        
    [toy-color  (-> toy? symbol?)])))       
                                            
(provide contracted-toy-factory^)           
```

Now we take the previous implementation of `simple-factory@` and
implement this version of `toy-factory^` instead:

`"contracted-simple-factory-unit.rkt"`
```racket
#lang racket                               
                                           
(require "contracted-toy-factory-sig.rkt") 
                                           
(define-unit contracted-simple-factory@    
  (import)                                 
  (export contracted-toy-factory^)         
                                           
  (printf "Factory started.\n")            
                                           
  (define-struct toy (color) #:transparent)
                                           
  (define (build-toys n)                   
    (for/list ([i (in-range n)])           
      (make-toy 'blue)))                   
                                           
  (define (repaint t col)                  
    (make-toy col)))                       
                                           
(provide contracted-simple-factory@)       
```

As before, we can invoke our new unit and bind the exports so that we
can use them.  This time, however, misusing the exports causes the
appropriate contract errors.

```racket
> (require "contracted-simple-factory-unit.rkt")              
> (define-values/invoke-unit/infer contracted-simple-factory@)
Factory started.                                              
> (build-toys 3)                                              
(list (toy 'blue) (toy 'blue) (toy 'blue))                    
> (build-toys #f)                                             
build-toys: contract violation                                
  expected: integer?                                          
  given: #f                                                   
  in: the 1st argument of                                     
      (-> integer? (listof toy?))                             
  contract from:                                              
      (unit contracted-simple-factory@)                       
  blaming: top-level                                          
   (assuming the contract is correct)                         
  at: eval:34.0                                               
> (repaint 3 'blue)                                           
repaint: contract violation                                   
  expected: toy?                                              
  given: 3                                                    
  in: the 1st argument of                                     
      (-> toy? symbol? toy?)                                  
  contract from:                                              
      (unit contracted-simple-factory@)                       
  blaming: top-level                                          
   (assuming the contract is correct)                         
  at: eval:34.0                                               
```

### 6.2. Adding Contracts to Units

However, sometimes we may have a unit that must conform to an already
existing signature that is not contracted.  In this case, we can create
a unit contract with `unit/c` or use the `define-unit/contract` form,
which defines a unit which has been wrapped with a unit contract.

For example, here’s a version of `toy-factory@` which still implements
the regular `toy-factory^`, but whose exports have been protected with
an appropriate unit contract.

`"wrapped-simple-factory-unit.rkt"`
```racket
#lang racket                                       
                                                   
(require "toy-factory-sig.rkt")                    
                                                   
(define-unit/contract wrapped-simple-factory@      
  (import)                                         
  (export (toy-factory^                            
           [build-toys (-> integer? (listof toy?))]
           [repaint    (-> toy? symbol? toy?)]     
           [toy?       (-> any/c boolean?)]        
           [toy-color  (-> toy? symbol?)]))        
                                                   
  (printf "Factory started.\n")                    
                                                   
  (define-struct toy (color) #:transparent)        
                                                   
  (define (build-toys n)                           
    (for/list ([i (in-range n)])                   
      (make-toy 'blue)))                           
                                                   
  (define (repaint t col)                          
    (make-toy col)))                               
                                                   
(provide wrapped-simple-factory@)                  
```

```racket
> (require "wrapped-simple-factory-unit.rkt")              
> (define-values/invoke-unit/infer wrapped-simple-factory@)
Factory started.                                           
> (build-toys 3)                                           
(list (toy 'blue) (toy 'blue) (toy 'blue))                 
> (build-toys #f)                                          
wrapped-simple-factory@: contract violation                
  expected: integer?                                       
  given: #f                                                
  in: the 1st argument of                                  
      (unit/c                                              
       (import)                                            
       (export (toy-factory^                               
                (build-toys                                
                 (-> integer? (listof toy?)))              
                (repaint (-> toy? symbol? toy?))           
                (toy? (-> any/c boolean?))                 
                (toy-color (-> toy? symbol?))))            
       (init-depend))                                      
  contract from:                                           
      (unit wrapped-simple-factory@)                       
  blaming: top-level                                       
   (assuming the contract is correct)                      
  at: <collects>/racket/unit.rkt                           
> (repaint 3 'blue)                                        
wrapped-simple-factory@: contract violation                
  expected: toy?                                           
  given: 3                                                 
  in: the 1st argument of                                  
      (unit/c                                              
       (import)                                            
       (export (toy-factory^                               
                (build-toys                                
                 (-> integer? (listof toy?)))              
                (repaint (-> toy? symbol? toy?))           
                (toy? (-> any/c boolean?))                 
                (toy-color (-> toy? symbol?))))            
       (init-depend))                                      
  contract from:                                           
      (unit wrapped-simple-factory@)                       
  blaming: top-level                                       
   (assuming the contract is correct)                      
  at: <collects>/racket/unit.rkt                           
```

## 7. `unit` versus `module`

As a form for modularity, `unit` complements `module`:

* The `module` form is primarily for managing a universal namespace. For
  example, it allows a code fragment to refer specifically to the `car`
  operation from `racket/base`—the one that extracts the first element
  of an instance of the built-in pair datatype—as opposed to any number
  of other functions with the name `car`. In other words, the `module`
  construct lets you refer to _the_ binding that you want.

* The `unit` form is for parameterizing a code fragment with respect to
  most any kind of run-time value. For example, it allows a code
  fragment to work with a `car` function that accepts a single argument,
  where the specific function is determined later by linking the
  fragment to another. In other words, the `unit` construct lets you
  refer to _a_ binding that meets some specification.

The `lambda` and `class` forms, among others, also allow
parameterization of code with respect to values that are chosen later.
In principle, any of those could be implemented in terms of any of the
others. In practice, each form offers certain conveniences—such as
allowing overriding of methods or especially simple application to
values—that make them suitable for different purposes.

The `module` form is more fundamental than the others, in a sense. After
all, a program fragment cannot reliably refer to a `lambda`, `class`, or
`unit` form without the namespace management provided by `module`. At
the same time, because namespace management is closely related to
separate expansion and compilation, `module` boundaries end up as
separate-compilation boundaries in a way that prohibits mutual
dependencies among fragments. For similar reasons, `module` does not
separate interface from implementation.

Use `unit` when `module` by itself almost works, but when separately
compiled pieces must refer to each other, or when you want a stronger
separation between _interface_ \(i.e., the parts that need to be known
at expansion and compilation time\) and _implementation_ \(i.e., the
run-time parts\). More generally, use `unit` when you need to
parameterize code over functions, datatypes, and classes, and when the
parameterized code itself provides definitions to be linked with other
parameterized code.
