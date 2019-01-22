# Programmer-Defined Datatypes

> +\[missing\] in \[missing\] also documents structure types.

New datatypes are normally created with the `struct` form, which is the
topic of this chapter. The class-based object system, which we defer to
\[missing\], offers an alternate mechanism for creating new datatypes,
but even classes and objects are implemented in terms of structure
types.

## 1. Simple Structure Types: `struct`

> +\[missing\] in \[missing\] also documents `struct`.

To a first approximation, the syntax of `struct` is

```racket
(struct struct-id (field-id ...))
```

Examples:

`(struct` `posn` `(x` `y))`

The `struct` form binds `struct-id` and a number of identifiers that are
built from `struct-id` and the `field-id`s:

* `struct-id` : a _constructor_ function that takes as many arguments as
  the number of `field-id`s, and returns an instance of the structure
  type.

  Example:

  ```racket
> (posn 1 2)
  #<posn>     
```

* `struct-id?` : a _predicate_ function that takes a single argument and
  returns `#t` if it is an instance of the structure type, `#f`
  otherwise.

  Examples:

  ```racket
> (posn? 3)         
  #f                  
  > (posn? (posn 1 2))
  #t                  
```

* `struct-id-field-id` : for each `field-id`, an _accessor_ that
  extracts the value of the corresponding field from an instance of the
  structure type.

  Examples:

  ```racket
> (posn-x (posn 1 2))
  1                    
  > (posn-y (posn 1 2))
  2                    
```

* `struct:struct-id` : a _structure type descriptor_, which is a value
  that represents the structure type as a first-class value \(with
  `#:super`, as discussed later in More Structure Type Options\).

A `struct` form places no constraints on the kinds of values that can
appear for fields in an instance of the structure type. For example,
`(posn "apple" #f)` produces an instance of `posn`, even though
`"apple"` and `#f` are not valid coordinates for the obvious uses of
`posn` instances. Enforcing constraints on field values, such as
requiring them to be numbers, is normally the job of a contract, as
discussed later in \[missing\].

## 2. Copying and Update

The `struct-copy` form clones a structure and optionally updates
specified fields in the clone. This process is sometimes called a
_functional update_, because the result is a structure with updated
field values. but the original structure is not modified.

```racket
(struct-copy struct-id struct-expr [field-id expr] ...)
```

The `struct-id` that appears after `struct-copy` must be a structure
type name bound by `struct` \(i.e., the name that cannot be used
directly as an expression\). The `struct-expr` must produce an instance
of the structure type. The result is a new instance of the structure
type that is like the old one, except that the field indicated by each
`field-id` gets the value of the corresponding `expr`.

Examples:

```racket
> (define p1 (posn 1 2))                 
> (define p2 (struct-copy posn p1 [x 3]))
> (list (posn-x p2) (posn-y p2))         
'(3 2)                                   
> (list (posn-x p1) (posn-x p2))         
'(1 3)                                   
```

## 3. Structure Subtypes

An extended form of `struct` can be used to define a _structure
subtype_, which is a structure type that extends an existing structure
type:

```racket
(struct struct-id super-id (field-id ...))
```

The `super-id` must be a structure type name bound by `struct` \(i.e.,
the name that cannot be used directly as an expression\).

Examples:

```racket
(struct posn (x y))      
(struct 3d-posn posn (z))
```

A structure subtype inherits the fields of its supertype, and the
subtype constructor accepts the values for the subtype fields after
values for the supertype fields. An instance of a structure subtype can
be used with the predicate and accessors of the supertype.

Examples:

```racket
> (define p (3d-posn 1 2 3))                                   
> p                                                            
#<3d-posn>                                                     
> (posn? p)                                                    
#t                                                             
> (3d-posn-z p)                                                
3                                                              
; a 3d-posn has an x field, but there is no 3d-posn-x selector:
> (3d-posn-x p)                                                
3d-posn-x: undefined;                                          
 cannot reference an identifier before its definition          
  in module: top-level                                         
; use the supertype's posn-x selector to access the x field:   
> (posn-x p)                                                   
1                                                              
```

## 4. Opaque versus Transparent Structure Types

With a structure type definition like

`(struct` `posn` `(x` `y))`

an instance of the structure type prints in a way that does not show any
information about the fields’ values. That is, structure types by
default are _opaque_. If the accessors and mutators of a structure type
are kept private to a module, then no other module can rely on the
representation of the type’s instances.

To make a structure type _transparent_, use the `#:transparent` keyword
after the field-name sequence:

```racket             
(struct posn (x y)    
        #:transparent)
```                   
                      
```racket             
> (posn 1 2)          
(posn 1 2)            
```                   

An instance of a transparent structure type prints like a call to the
constructor, so that it shows the structures field values. A transparent
structure type also allows reflective operations, such as `struct?` and
`struct-info`, to be used on its instances \(see \[missing\]\).

Structure types are opaque by default, because opaque structure
instances provide more encapsulation guarantees. That is, a library can
use an opaque structure to encapsulate data, and clients of the library
cannot manipulate the data in the structure except as allowed by the
library.

## 5. Structure Comparisons

A generic `equal?` comparison automatically recurs on the fields of a
transparent structure type, but `equal?` defaults to mere instance
identity for opaque structure types:

`(struct` `glass` `(width` `height)` `#:transparent)`
                                                     
```racket                                            
> (equal? (glass 1 2) (glass 1 2))                   
#t                                                   
```                                                  

`(struct` `lead` `(width` `height))`
                                    
```racket                           
> (define slab (lead 1 2))          
> (equal? slab slab)                
#t                                  
> (equal? slab (lead 1 2))          
#f                                  
```                                 

To support instances comparisons via `equal?` without making the
structure type transparent, you can use the `#:methods` keyword,
`gen:equal+hash`, and implement three methods:

```racket                                                 
(struct lead (width height)                               
  #:methods                                               
  gen:equal+hash                                          
  [(define (equal-proc a b equal?-recur)                  
     ; compare a and b                                    
     (and (equal?-recur (lead-width a) (lead-width b))    
          (equal?-recur (lead-height a) (lead-height b))))
   (define (hash-proc a hash-recur)                       
     ; compute primary hash code of a                     
     (+ (hash-recur (lead-width a))                       
        (* 3 (hash-recur (lead-height a)))))              
   (define (hash2-proc a hash2-recur)                     
     ; compute secondary hash code of a                   
     (+ (hash2-recur (lead-width a))                      
             (hash2-recur (lead-height a))))])            
```                                                       
                                                          
```racket                                                 
> (equal? (lead 1 2) (lead 1 2))                          
#t                                                        
```                                                       

The first function in the list implements the `equal?` test on two
`lead`s; the third argument to the function is used instead of `equal?`
for recursive equality testing, so that data cycles can be handled
correctly. The other two functions compute primary and secondary hash
codes for use with hash tables:

```racket
> (define h (make-hash))        
> (hash-set! h (lead 1 2) 3)    
> (hash-ref h (lead 1 2))       
3                               
> (hash-ref h (lead 2 1))       
hash-ref: no value found for key
  key: #<lead>                  
```

The first function provided with `gen:equal+hash` is not required to
recursively compare the fields of the structure. For example, a
structure type representing a set might implement equality by checking
that the members of the set are the same, independent of the order of
elements in the internal representation. Just take care that the hash
functions produce the same value for any two structure types that are
supposed to be equivalent.

## 6. Structure Type Generativity

Each time that a `struct` form is evaluated, it generates a structure
type that is distinct from all existing structure types, even if some
other structure type has the same name and fields.

This generativity is useful for enforcing abstractions and implementing
programs such as interpreters, but beware of placing a `struct` form in
positions that are evaluated multiple times.

Examples:

```racket
(define (add-bigger-fish lst)                            
  (struct fish (size) #:transparent) ; new every time    
  (cond                                                  
   [(null? lst) (list (fish 1))]                         
   [else (cons (fish (* 2 (fish-size (car lst))))        
               lst)]))                                   
                                                         
> (add-bigger-fish null)                                 
(list (fish 1))                                          
> (add-bigger-fish (add-bigger-fish null))               
fish-size: contract violation;                           
 given value instantiates a different structure type with
the same name                                            
  expected: fish?                                        
  given: (fish 1)                                        
```

```racket                                        
(struct fish (size) #:transparent)               
(define (add-bigger-fish lst)                    
  (cond                                          
   [(null? lst) (list (fish 1))]                 
   [else (cons (fish (* 2 (fish-size (car lst))))
               lst)]))                           
```                                              
                                                 
```racket                                        
> (add-bigger-fish (add-bigger-fish null))       
(list (fish 2) (fish 1))                         
```                                              

## 7. Prefab Structure Types

Although a transparent structure type prints in a way that shows its
content, the printed form of the structure cannot be used in an
expression to get the structure back, unlike the printed form of a
number, string, symbol, or list.

A _prefab_ \(“previously fabricated”\) structure type is a built-in type
that is known to the Racket printer and expression reader. Infinitely
many such types exist, and they are indexed by name, field count,
supertype, and other such details. The printed form of a prefab
structure is similar to a vector, but it starts `#s` instead of just
`#`, and the first element in the printed form is the prefab structure
type’s name.

The following examples show instances of the `sprout` prefab structure
type that has one field. The first instance has a field value `'bean`,
and the second has field value `'alfalfa`:

```racket
> '#s(sprout bean)   
'#s(sprout bean)     
> '#s(sprout alfalfa)
'#s(sprout alfalfa)  
```

Like numbers and strings, prefab structures are “self-quoting,” so the
quotes above are optional:

```racket
> #s(sprout bean)
'#s(sprout bean) 
```

When you use the `#:prefab` keyword with `struct`, instead of generating
a new structure type, you obtain bindings that work with the existing
prefab structure type:

```racket
> (define lunch '#s(sprout bean))
> (struct sprout (kind) #:prefab)
> (sprout? lunch)                
#t                               
> (sprout-kind lunch)            
'bean                            
> (sprout 'garlic)               
'#s(sprout garlic)               
```

The field name `kind` above does not matter for finding the prefab
structure type; only the name `sprout` and the number of fields matters.
At the same time, the prefab structure type `sprout` with three fields
is a different structure type than the one with a single field:

```racket
> (sprout? #s(sprout bean #f 17))                        
#f                                                       
> (struct sprout (kind yummy? count) #:prefab) ; redefine
> (sprout? #s(sprout bean #f 17))                        
#t                                                       
> (sprout? lunch)                                        
#f                                                       
```

A prefab structure type can have another prefab structure type as its
supertype, it can have mutable fields, and it can have auto fields.
Variations in any of these dimensions correspond to different prefab
structure types, and the printed form of the structure type’s name
encodes all of the relevant details.

```racket
> (struct building (rooms [location #:mutable]) #:prefab)
> (struct house building ([occupied #:auto]) #:prefab    
    #:auto-value 'no)                                    
> (house 5 'factory)                                     
'#s((house (1 no) building 2 #(1)) 5 factory no)         
```

Every prefab structure type is transparent—but even less abstract than a
transparent type, because instances can be created without any access to
a particular structure-type declaration or existing examples. Overall,
the different options for structure types offer a spectrum of
possibilities from more abstract to more convenient:

* Opaque \(the default\) : Instances cannot be inspected or forged
  without access to the structure-type declaration. As discussed in the
  next section, constructor guards and properties can be attached to the
  structure type to further protect or to specialize the behavior of its
  instances.

* Transparent : Anyone can inspect or create an instance without access
  to the structure-type declaration, which means that the value printer
  can show the content of an instance. All instance creation passes
  through a constructor guard, however, so that the content of an
  instance can be controlled, and the behavior of instances can be
  specialized through properties. Since the structure type is generated
  by its definition, instances cannot be manufactured simply through the
  name of the structure type, and therefore cannot be generated
  automatically by the expression reader.

* Prefab : Anyone can inspect or create an instance at any time, without
  prior access to a structure-type declaration or an example instance.
  Consequently, the expression reader can manufacture instances
  directly. The instance cannot have a constructor guard or properties.

Since the expression reader can generate prefab instances, they are
useful when convenient serialization is more important than abstraction.
Opaque and transparent structures also can be serialized, however, if
they are defined with `serializable-struct` as described in \[missing\].

## 8. More Structure Type Options

The full syntax of `struct` supports many options, both at the
structure-type level and at the level of individual fields:

```racket
(struct struct-id maybe-super (field ...)
        struct-option ...)               
                                         
maybe-super =                            
            | super-id                   
                                         
field       = field-id                   
            | [field-id field-option ...]
```

A `struct-option` always starts with a keyword:

```racket
#:mutable
```
Causes all fields of the structure to be mutable, and introduces  for
each `field-id` a _mutator_   `set-struct-id-field-id!`  that sets the
value of the corresponding field in an instance of  the structure type.
Examples:

```racket
> (struct dot (x y) #:mutable)
(define d (dot 1 2))          
                              
> (dot-x d)                   
1                             
> (set-dot-x! d 10)           
> (dot-x d)                   
10                            
```
The `#:mutable` option can also be used as a `field-option`, in which
case it makes an individual field mutable.
Examples:

```racket
> (struct person (name [age #:mutable]))             
(define friend (person "Barney" 5))                  
                                                     
> (set-person-age! friend 6)                         
> (set-person-name! friend "Mary")                   
set-person-name!: undefined;                         
 cannot reference an identifier before its definition
  in module: top-level                               
```

```racket
#:transparent
```
Controls reflective access to structure instances, as discussed in a
previous section, Opaque versus Transparent Structure Types.

```racket
#:inspector inspector-expr
```
Generalizes `#:transparent` to support more controlled access to
reflective operations.

```racket
#:prefab
```
Accesses a built-in structure type, as discussed in a previous section,
Prefab Structure Types.

```racket
#:auto-value auto-expr
```
Specifies a value to be used for all automatic fields in the structure
type, where an automatic field is indicated by the `#:auto` field
option. The constructor procedure does not accept arguments for
automatic fields. Automatic fields are implicitly mutable \(via
reflective operations\), but mutator functions are bound only if
`#:mutable` is also specified.
Examples:

```racket
> (struct posn (x y [z #:auto])
               #:transparent   
               #:auto-value 0) 
> (posn 1 2)                   
(posn 1 2 0)                   
```

```racket
#:guard guard-expr
```
Specifies a  _constructor guard_ procedure to be called whenever an
instance of the structure type is created. The guard takes as many
arguments as non-automatic fields in the structure type, plus one  more
for the name of the instantiated type \(in case a sub-type is
instantiated, in which case it’s best to report an error using the
sub-type’s name\). The guard should return the same number of values  as
given, minus the name argument. The guard can raise an exception  if one
of the given arguments is unacceptable, or it can convert an  argument.
Examples:

```racket
> (struct thing (name)                                      
          #:transparent                                     
          #:guard (lambda (name type-name)                  
                    (cond                                   
                      [(string? name) name]                 
                      [(symbol? name) (symbol->string name)]
                      [else (error type-name                
                                   "bad name: ~e"           
                                   name)])))                
> (thing "apple")                                           
(thing "apple")                                             
> (thing 'apple)                                            
(thing "apple")                                             
> (thing 1/2)                                               
thing: bad name: 1/2                                        
```
The guard is called even when subtype instances are created. In that
case, only the fields accepted by the constructor are provided to  the
guard \(but the subtype’s guard gets both the original fields and
fields added by the subtype\).
Examples:

```racket
> (struct person thing (age)                               
          #:transparent                                    
          #:guard (lambda (name age type-name)             
                    (if (negative? age)                    
                        (error type-name "bad age: ~e" age)
                        (values name age))))               
> (person "John" 10)                                       
(person "John" 10)                                         
> (person "Mary" -1)                                       
person: bad age: -1                                        
> (person 10 10)                                           
person: bad name: 10                                       
```

```racket
#:methods interface-expr [body ...]
```
Associates method definitions for the structure type that correspond to
a _generic interface_.  For example, implementing the methods for
`gen:dict` allows instances of a structure type to be used as
dictionaries. Implementing the methods for `gen:custom-write` allows the
customization of how an instance of a structure type is `display`ed.
Examples:

```racket
> (struct cake (candles)                             
          #:methods gen:custom-write                 
          [(define (write-proc cake port mode)       
             (define n (cake-candles cake))          
             (show "   ~a   ~n" n #\. port)          
             (show " .-~a-. ~n" n #\| port)          
             (show " | ~a | ~n" n #\space port)      
             (show "---~a---~n" n #\- port))         
           (define (show fmt n ch port)              
             (fprintf port fmt (make-string n ch)))])
> (display (cake 5))                                 
   .....                                             
 .-|||||-.                                           
 |       |                                           
-----------                                          
```

```racket
#:property prop-expr val-expr
```
Associates a _property_ and value with the structure type.   For
example, the `prop:procedure` property allows a   structure instance to
be used as a function; the property value   determines how a call is
implemented when using the structure as a   function.
Examples:

```racket
> (struct greeter (name)                               
          #:property prop:procedure                    
                     (lambda (self other)              
                       (string-append                  
                        "Hi " other                    
                        ", I'm " (greeter-name self))))
(define joe-greet (greeter "Joe"))                     
                                                       
> (greeter-name joe-greet)                             
"Joe"                                                  
> (joe-greet "Mary")                                   
"Hi Mary, I'm Joe"                                     
> (joe-greet "John")                                   
"Hi John, I'm Joe"                                     
```

```racket
#:super super-expr
```
An alternative to supplying a `super-id` next to `struct-id`. Instead of
the name of a structure type \(which is not an expression\),
`super-expr` should produce a structure type descriptor value. An
advantage of `#:super` is that structure type descriptors are values, so
they can be passed to procedures.
Examples:

```racket
(define (raven-constructor super-type)                 
  (struct raven ()                                     
          #:super super-type                           
          #:transparent                                
          #:property prop:procedure (lambda (self)     
                                      'nevermore))     
  raven)                                               
                                                       
> (let ([r ((raven-constructor struct:posn) 1 2)])     
    (list r (r)))                                      
(list (raven 1 2) 'nevermore)                          
> (let ([r ((raven-constructor struct:thing) "apple")])
    (list r (r)))                                      
(list (raven "apple") 'nevermore)                      
```

> +\[missing\] in \[missing\] provides more on structure types.
