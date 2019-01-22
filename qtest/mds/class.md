# Classes and Objects

> This chapter is based on a paper \[Flatt06\].

A `class` expression denotes a first-class value, just like a `lambda`
expression:

```racket
(class superclass-expr decl-or-expr ...)
```

The `superclass-expr` determines the superclass for the new class. Each
`decl-or-expr` is either a declaration related to methods, fields, and
initialization arguments, or it is an expression that is evaluated each
time that the class is instantiated. In other words, instead of a
method-like constructor, a class has initialization expressions
interleaved with field and method declarations.

By convention, class names end with `%`. The built-in root class is
`object%`. The following expression creates a class with public methods
`get-size`, `grow`, and `eat`:

```racket
(class object%                                          
  (init size)                ; initialization argument  
                                                        
  (define current-size size) ; field                    
                                                        
  (super-new)                ; superclass initialization
                                                        
  (define/public (get-size)                             
    current-size)                                       
                                                        
  (define/public (grow amt)                             
    (set! current-size (+ amt current-size)))           
                                                        
  (define/public (eat other-fish)                       
    (grow (send other-fish get-size))))                 
```

The `size` initialization argument must be supplied via a named
argument when instantiating the class through the `new` form:

`(new` `(class` `object%` `(init` `size)` `....)` `[size` `10])`

Of course, we can also name the class and its instance:

```racket
(define fish% (class object% (init size) ....))
(define charlie (new fish% [size 10]))         
```

In the definition of `fish%`, `current-size` is a private field that
starts out with the value of the `size` initialization argument.
Initialization arguments like `size` are available only during class
instantiation, so they cannot be referenced directly from a method. The
`current-size` field, in contrast, is available to methods.

The `(super-new)` expression in `fish%` invokes the initialization of
the superclass. In this case, the superclass is `object%`, which takes
no initialization arguments and performs no work; `super-new` must be
used, anyway, because a class must always invoke its superclass’s
initialization.

Initialization arguments, field declarations, and expressions such as
`(super-new)` can appear in any order within a `class`, and they can be
interleaved with method declarations. The relative order of expressions
in the class determines the order of evaluation during instantiation.
For example, if a field’s initial value requires calling a method that
works only after superclass initialization, then the field declaration
must be placed after the `super-new` call. Ordering field and
initialization declarations in this way helps avoid imperative
assignment. The relative order of method declarations makes no
difference for evaluation, because methods are fully defined before a
class is instantiated.

## 1. Methods

Each of the three `define/public` declarations in `fish%` introduces a
new method. The declaration uses the same syntax as a Racket function,
but a method is not accessible as an independent function.  A call to
the `grow` method of a `fish%` object requires the `send` form:

```racket
> (send charlie grow 6)  
> (send charlie get-size)
16                       
```

Within `fish%`, self methods can be called like functions, because the
method names are in scope.  For example, the `eat` method within `fish%`
directly invokes the `grow` method.  Within a class, attempting to use a
method name in any way other than a method call results in a syntax
error.

In some cases, a class must call methods that are supplied by the
superclass but not overridden. In that case, the class can use `send`
with `this` to access the method:

```racket                                                   
(define hungry-fish% (class fish% (super-new)               
                       (define/public (eat-more fish1 fish2)
                         (send this eat fish1)              
                         (send this eat fish2))))           
```                                                         
                                                            
                                                            

Alternately, the class can declare the existence of a method using
`inherit`, which brings the method name into scope for a direct call:

```racket                                                   
(define hungry-fish% (class fish% (super-new)               
                       (inherit eat)                        
                       (define/public (eat-more fish1 fish2)
                         (eat fish1) (eat fish2))))         
```                                                         
                                                            
                                                            

With the `inherit` declaration, if `fish%` had not provided an `eat`
method, an error would be signaled in the evaluation of the `class` form
for `hungry-fish%`. In contrast, with `(send this ....)`, an error would
not be signaled until the `eat-more` method is called and the `send`
form is evaluated. For this reason, `inherit` is preferred.

Another drawback of `send` is that it is less efficient than `inherit`.
Invocation of a method via `send` involves finding a method in the
target object’s class at run time, making `send` comparable to an
interface-based method call in Java. In contrast, `inherit`-based method
invocations use an offset within the class’s method table that is
computed when the class is created.

To achieve performance similar to `inherit`-based method calls when
invoking a method from outside the method’s class, the programmer must
use the `generic` form, which produces a class- and method-specific
_generic method_ to be invoked with `send-generic`:

`(define` `get-fish-size` `(generic` `fish%` `get-size))`   
                                                            
```racket                                                   
> (send-generic charlie get-fish-size)                      
16                                                          
> (send-generic (new hungry-fish% [size 32]) get-fish-size) 
32                                                          
> (send-generic (new object%) get-fish-size)                
generic:get-size: target is not an instance of the generic's
class                                                       
  target: (object)                                          
  class name: fish%                                         
```                                                         

Roughly speaking, the form translates the class and the external method
name to a location in the class’s method table. As illustrated by the
last example, sending through a generic method checks that its argument
is an instance of the generic’s class.

Whether a method is called directly within a `class`, through a generic
method, or through `send`, method overriding works in the usual way:

```racket                                          
(define picky-fish% (class fish% (super-new)       
                      (define/override (grow amt)  
                                                   
                        (super grow (* 3/4 amt)))))
(define daisy (new picky-fish% [size 20]))         
```                                                
                                                   
```racket                                          
> (send daisy eat charlie)                         
> (send daisy get-size)                            
32                                                 
```                                                

The `grow` method in `picky-fish%` is declared with `define/override`
instead of `define/public`, because `grow` is meant as an overriding
declaration. If `grow` had been declared with `define/public`, an error
would have been signaled when evaluating the `class` expression, because
`fish%` already supplies `grow`.

Using `define/override` also allows the invocation of the overridden
method via a `super` call. For example, the `grow` implementation in
`picky-fish%` uses `super` to delegate to the superclass implementation.

## 2. Initialization Arguments

Since `picky-fish%` declares no initialization arguments, any
initialization values supplied in `(new picky-fish% ....)`  are
propagated to the superclass initialization, i.e., to `fish%`. A
subclass can supply additional initialization arguments for its
superclass in a `super-new` call, and such initialization arguments take
precedence over arguments supplied to `new`. For example, the following
`size-10-fish%` class always generates fish of size 10:

`(define` `size-10-fish%` `(class` `fish%` `(super-new` `[size` `10])))`
                                                                        
```racket                                                               
> (send (new size-10-fish%) get-size)                                   
10                                                                      
```                                                                     

In the case of `size-10-fish%`, supplying a `size` initialization
argument with `new` would result in an initialization error; because the
`size` in `super-new` takes precedence, a `size` supplied to `new` would
have no target declaration.

An initialization argument is optional if the `class` form declares a
default value. For example, the following `default-10-fish%` class
accepts a `size` initialization argument, but its value defaults to 10
if no value is supplied on instantiation:

```racket                                           
(define default-10-fish% (class fish%               
                           (init [size 10])         
                           (super-new [size size])))
```                                                 
                                                    
```racket                                           
> (new default-10-fish%)                            
(object:default-10-fish% ...)                       
> (new default-10-fish% [size 20])                  
(object:default-10-fish% ...)                       
```                                                 

In this example, the `super-new` call propagates its own `size` value as
the `size` initialization argument to the superclass.

## 3. Internal and External Names

The two uses of `size` in `default-10-fish%` expose the double life of
class-member identifiers. When `size` is the first identifier of a
bracketed pair in `new` or `super-new`, `size` is an _external name_
that is symbolically matched to an initialization argument in a class.
When `size` appears as an expression within `default-10-fish%`, `size`
is an _internal name_ that is lexically scoped. Similarly, a call to an
inherited `eat` method uses `eat` as an internal name, whereas a `send`
of `eat` uses `eat` as an external name.

The full syntax of the `class` form allows a programmer to specify
distinct internal and external names for a class member. Since internal
names are local, they can be renamed to avoid shadowing or conflicts.
Such renaming is not frequently necessary, but workarounds in the
absence of renaming can be especially cumbersome.

## 4. Interfaces

Interfaces are useful for checking that an object or a class implements
a set of methods with a particular \(implied\) behavior. This use of
interfaces is helpful even without a static type system \(which is the
main reason that Java has interfaces\).

An interface in Racket is created using the `interface` form, which
merely declares the method names required to implement the interface. An
interface can extend other interfaces, which means that implementations
of the interface automatically implement the extended interfaces.

```racket
(interface (superinterface-expr ...) id ...)
```

To declare that a class implements an interface, the `class*` form must
be used instead of `class`:

```racket
(class* superclass-expr (interface-expr ...) decl-or-expr ...)
```

For example, instead of forcing all fish classes to be derived from
`fish%`, we can define `fish-interface` and change the `fish%` class to
declare that it implements `fish-interface`:

```racket
(define fish-interface (interface () get-size grow eat))
(define fish% (class* object% (fish-interface) ....))   
```

If the definition of `fish%` does not include `get-size`, `grow`, and
`eat` methods, then an error is signaled in the evaluation of the
`class*` form, because implementing the `fish-interface` interface
requires those methods.

The `is-a?` predicate accepts an object as its first argument and either
a class or interface as its second argument. When given a class, `is-a?`
checks whether the object is an instance of that class or a derived
class.  When given an interface, `is-a?` checks whether the object’s
class implements the interface. In addition, the `implementation?`
predicate checks whether a given class implements a given interface.

## 5. Final, Augment, and Inner

As in Java, a method in a `class` form can be specified as _final_,
which means that a subclass cannot override the method.  A final method
is declared using `public-final` or `override-final`, depending on
whether the declaration is for a new method or an overriding
implementation.

Between the extremes of allowing arbitrary overriding and disallowing
overriding entirely, the class system also supports Beta-style
_augmentable_ methods \[Goldberg04\]. A method declared with `pubment`
is like `public`, but the method cannot be overridden in subclasses; it
can be augmented only. A `pubment` method must explicitly invoke an
augmentation \(if any\) using `inner`; a subclass augments the method
using `augment`, instead of `override`.

In general, a method can switch between augment and override modes in a
class derivation. The `augride` method specification indicates an
augmentation to a method where the augmentation is itself overrideable
in subclasses \(though the superclass’s implementation cannot be
overridden\). Similarly, `overment` overrides a method and makes the
overriding implementation augmentable.

## 6. Controlling the Scope of External Names

> Java’s access modifiers \(like `protected`\) play a role similar to
> `define-member-name`, but unlike in Java, Racket’s mechanism for
> controlling access is based on lexical scope, not the inheritance
> hierarchy.

As noted in Internal and External Names, class members have both
internal and external names. A member definition binds an internal name
locally, and this binding can be locally renamed.  External names, in
contrast, have global scope by default, and a member definition does not
bind an external name. Instead, a member definition refers to an
existing binding for an external name, where the member name is bound to
a _member key_; a class ultimately maps member keys to methods, fields,
and initialization arguments.

Recall the `hungry-fish%` `class` expression:

```racket
(define hungry-fish% (class fish% ....                      
                       (inherit eat)                        
                       (define/public (eat-more fish1 fish2)
                         (eat fish1) (eat fish2))))         
```

During its evaluation, the `hungry-fish%` and `fish%` classes refer to
the same global binding of `eat`.  At run time, calls to `eat` in
`hungry-fish%` are matched with the `eat` method in `fish%` through the
shared method key that is bound to `eat`.

The default binding for an external name is global, but a programmer can
introduce an external-name binding with the `define-member-name` form.

```racket
(define-member-name id member-key-expr)
```

In particular, by using `(generate-member-key)` as the
`member-key-expr`, an external name can be localized for a particular
scope, because the generated member key is inaccessible outside the
scope. In other words, `define-member-name` gives an external name a
kind of package-private scope, but generalized from packages to
arbitrary binding scopes in Racket.

For example, the following `fish%` and `pond%` classes cooperate via a
`get-depth` method that is only accessible to the cooperating classes:

```racket
(define-values (fish% pond%) ; two mutually recursive classes
  (let ()                                                    
    (define-member-name get-depth (generate-member-key))     
    (define fish%                                            
      (class ....                                            
        (define my-depth ....)                               
        (define my-pond ....)                                
        (define/public (dive amt)                            
        (set! my-depth                                       
              (min (+ my-depth amt)                          
                   (send my-pond get-depth))))))             
    (define pond%                                            
      (class ....                                            
        (define current-depth ....)                          
        (define/public (get-depth) current-depth)))          
    (values fish% pond%)))                                   
```

External names are in a namespace that separates them from other Racket
names. This separate namespace is implicitly used for the method name in
`send`, for initialization-argument names in `new`, or for the external
name in a member definition.  The special form `member-name-key`
provides access to the binding of an external name in an arbitrary
expression position: `(member-name-key id)` produces the member-key
binding of `id` in the current scope.

A member-key value is primarily used with a `define-member-name` form.
Normally, then, `(member-name-key id)` captures the method key of `id`
so that it can be communicated to a use of `define-member-name` in a
different scope. This capability turns out to be useful for generalizing
mixins, as discussed next.

## 7. Mixins

Since `class` is an expression form instead of a top-level declaration
as in Smalltalk and Java, a `class` form can be nested inside any
lexical scope, including `lambda`. The result is a _mixin_, i.e., a
class extension that is parameterized with respect to its superclass.

For example, we can parameterize the `picky-fish%` class over its
superclass to define `picky-mixin`:

```racket
(define (picky-mixin %)                                    
  (class % (super-new)                                     
    (define/override (grow amt) (super grow (* 3/4 amt)))))
(define picky-fish% (picky-mixin fish%))                   
```

Many small differences between Smalltalk-style classes and Racket
classes contribute to the effective use of mixins. In particular, the
use of `define/override` makes explicit that `picky-mixin` expects a
class with a `grow` method. If `picky-mixin` is applied to a class
without a `grow` method, an error is signaled as soon as `picky-mixin`
is applied.

Similarly, a use of `inherit` enforces a “method existence” requirement
when the mixin is applied:

```racket
(define (hungry-mixin %)                 
  (class % (super-new)                   
    (inherit eat)                        
    (define/public (eat-more fish1 fish2)
      (eat fish1)                        
      (eat fish2))))                     
```

The advantage of mixins is that we can easily combine them to create new
classes whose implementation sharing does not fit into a
single-inheritance hierarchy—without the ambiguities associated with
multiple inheritance. Equipped with `picky-mixin` and `hungry-mixin`,
creating a class for a hungry, yet picky fish is straightforward:

```racket
(define picky-hungry-fish%           
  (hungry-mixin (picky-mixin fish%)))
```

The use of keyword initialization arguments is critical for the easy use
of mixins. For example, `picky-mixin` and `hungry-mixin` can augment any
class with suitable `eat` and `grow` methods, because they do not
specify initialization arguments and add none in their `super-new`
expressions:

```racket
(define person%                                     
  (class object%                                    
    (init name age)                                 
    ....                                            
    (define/public (eat food) ....)                 
    (define/public (grow amt) ....)))               
(define child% (hungry-mixin (picky-mixin person%)))
(define oliver (new child% [name "Oliver"] [age 6]))
```

Finally, the use of external names for class members \(instead of
lexically scoped identifiers\) makes mixin use convenient. Applying
`picky-mixin` to `person%` works because the names `eat` and `grow`
match, without any a priori declaration that `eat` and `grow` should be
the same method in `fish%` and `person%`. This feature is a potential
drawback when member names collide accidentally; some accidental
collisions can be corrected by limiting the scope external names, as
discussed in Controlling the Scope of External Names.

### 7.1. Mixins and Interfaces

Using `implementation?`, `picky-mixin` could require that its base class
implements `grower-interface`, which could be implemented by both
`fish%` and `person%`:

```racket
(define grower-interface (interface () grow))           
(define (picky-mixin %)                                 
  (unless (implementation? % grower-interface)          
    (error "picky-mixin: not a grower-interface class"))
  (class % ....))                                       
```

Another use of interfaces with a mixin is to tag classes generated by
the mixin, so that instances of the mixin can be recognized. In other
words, `is-a?` cannot work on a mixin represented as a function, but it
can recognize an interface \(somewhat like a _specialization
interface_\) that is consistently implemented by the mixin.  For
example, classes generated by `picky-mixin` could be tagged with
`picky-interface`, enabling the `is-picky?` predicate:

```racket
(define picky-interface (interface ()))                 
(define (picky-mixin %)                                 
  (unless (implementation? % grower-interface)          
    (error "picky-mixin: not a grower-interface class"))
  (class* % (picky-interface) ....))                    
(define (is-picky? o)                                   
  (is-a? o picky-interface))                            
```

### 7.2. The `mixin` Form

To codify the `lambda`-plus-`class` pattern for implementing mixins,
including the use of interfaces for the domain and range of the mixin,
the class system provides a `mixin` macro:

```racket
(mixin (interface-expr ...) (interface-expr ...)
  decl-or-expr ...)                             
```

The first set of `interface-expr`s determines the domain of the mixin,
and the second set determines the range. That is, the expansion is a
function that tests whether a given base class implements the first
sequence of `interface-expr`s and produces a class that implements the
second sequence of `interface-expr`s. Other requirements, such as the
presence of `inherit`ed methods in the superclass, are then checked for
the `class` expansion of the `mixin` form.  For example:

```racket
> (define choosy-interface (interface () choose?))           
> (define hungry-interface (interface () eat))               
> (define choosy-eater-mixin                                 
    (mixin (choosy-interface) (hungry-interface)             
      (inherit choose?)                                      
      (super-new)                                            
      (define/public (eat x)                                 
        (cond                                                
          [(choose? x)                                       
           (printf "chomp chomp chomp on ~a.\n" x)]          
          [else                                              
           (printf "I'm not crazy about ~a.\n" x)]))))       
> (define herring-lover%                                     
    (class* object% (choosy-interface)                       
      (super-new)                                            
      (define/public (choose? x)                             
        (regexp-match #px"^herring" x))))                    
> (define herring-eater% (choosy-eater-mixin herring-lover%))
> (define eater (new herring-eater%))                        
> (send eater eat "elderberry")                              
I'm not crazy about elderberry.                              
> (send eater eat "herring")                                 
chomp chomp chomp on herring.                                
> (send eater eat "herring ice cream")                       
chomp chomp chomp on herring ice cream.                      
```

Mixins not only override methods and introduce public methods, they can
also augment methods, introduce augment-only methods, add an
overrideable augmentation, and add an augmentable override — all of the
things that a class can do \(see Final, Augment, and Inner\).

### 7.3. Parameterized Mixins

As noted in Controlling the Scope of External Names, external names can
be bound with `define-member-name`. This facility allows a mixin to be
generalized with respect to the methods that it defines and uses.  For
example, we can parameterize `hungry-mixin` with respect to the external
member key for `eat`:

```racket
(define (make-hungry-mixin eat-method-key)          
  (define-member-name eat eat-method-key)           
  (mixin () () (super-new)                          
    (inherit eat)                                   
    (define/public (eat-more x y) (eat x) (eat y))))
```

To obtain a particular hungry-mixin, we must apply this function to a
member key that refers to a suitable `eat` method, which we can obtain
using `member-name-key`:

```racket
((make-hungry-mixin (member-name-key eat))         
 (class object% .... (define/public (eat x) 'yum)))
```

Above, we apply `hungry-mixin` to an anonymous class that provides
`eat`, but we can also combine it with a class that provides `chomp`,
instead:

```racket
((make-hungry-mixin (member-name-key chomp))         
 (class object% .... (define/public (chomp x) 'yum)))
```

## 8. Traits

A _trait_ is similar to a mixin, in that it encapsulates a set of
methods to be added to a class. A trait is different from a mixin in
that its individual methods can be manipulated with trait operators such
as `trait-sum` \(merge the methods of two traits\), `trait-exclude`
\(remove a method from a trait\), and `trait-alias` \(add a copy of a
method with a new name; do not redirect any calls to the old name\).

The practical difference between mixins and traits is that two traits
can be combined, even if they include a common method and even if
neither method can sensibly override the other. In that case, the
programmer must explicitly resolve the collision, usually by aliasing
methods, excluding methods, and merging a new trait that uses the
aliases.

Suppose our `fish%` programmer wants to define two class extensions,
`spots` and `stripes`, each of which includes a `get-color` method. The
fish’s spot color should not override the stripe color nor vice versa;
instead, a `spots+stripes-fish%` should combine the two colors, which is
not possible if `spots` and `stripes` are implemented as plain mixins.
If, however, `spots` and `stripes` are implemented as traits, they can
be combined. First, we alias `get-color` in each trait to a
non-conflicting name. Second, the `get-color` methods are removed from
both and the traits with only aliases are merged. Finally, the new trait
is used to create a class that introduces its own `get-color` method
based on the two aliases, producing the desired `spots+stripes`
extension.

### 8.1. Traits as Sets of Mixins

One natural approach to implementing traits in Racket is as a set of
mixins, with one mixin per trait method.  For example, we might attempt
to define the spots and stripes traits as follows, using association
lists to represent sets:

```racket
(define spots-trait                                    
  (list (cons 'get-color                               
               (lambda (%) (class % (super-new)        
                             (define/public (get-color)
                               'black))))))            
(define stripes-trait                                  
  (list (cons 'get-color                               
              (lambda (%) (class % (super-new)         
                            (define/public (get-color) 
                              'red))))))               
```

A set representation, such as the above, allows `trait-sum` and
`trait-exclude` as simple manipulations; unfortunately, it does not
support the `trait-alias` operator. Although a mixin can be duplicated
in the association list, the mixin has a fixed method name, e.g.,
`get-color`, and mixins do not support a method-rename operation. To
support `trait-alias`, we must parameterize the mixins over the external
method name in the same way that `eat` was parameterized in
Parameterized Mixins.

To support the `trait-alias` operation, `spots-trait` should be
represented as:

```racket
(define spots-trait                                         
  (list (cons (member-name-key get-color)                   
              (lambda (get-color-key %)                     
                (define-member-name get-color get-color-key)
                (class % (super-new)                        
                  (define/public (get-color) 'black))))))   
```

When the `get-color` method in `spots-trait` is aliased to
`get-trait-color` and the `get-color` method is removed, the resulting
trait is the same as

```racket
(list (cons (member-name-key get-trait-color)             
            (lambda (get-color-key %)                     
              (define-member-name get-color get-color-key)
              (class % (super-new)                        
                (define/public (get-color) 'black)))))    
```

To apply a trait `T` to a class `C` and obtain a derived class, we use
`((trait->mixin T) C)`. The `trait->mixin` function supplies each mixin
of `T` with the key for the mixin’s method and a partial extension of
`C`:

```racket
(define ((trait->mixin T) C)                     
  (foldr (lambda (m %) ((cdr m) (car m) %)) C T))
```

Thus, when the trait above is combined with other traits and then
applied to a class, the use of `get-color` becomes a reference to the
external name `get-trait-color`.

### 8.2. Inherit and Super in Traits

This first implementation of traits supports `trait-alias`, and it
supports a trait method that calls itself, but it does not support
trait methods that call each other. In particular, suppose that a
spot-fish’s  market value depends on the color of its spots:

```racket
(define spots-trait                            
  (list (cons (member-name-key get-color) ....)
        (cons (member-name-key get-price)      
              (lambda (get-price %) ....       
                (class % ....                  
                  (define/public (get-price)   
                    .... (get-color) ....))))))
```

In this case, the definition of `spots-trait` fails, because `get-color`
is not in scope for the `get-price` mixin. Indeed, depending on the
order of mixin application when the trait is applied to a class, the
`get-color` method may not be available when `get-price` mixin is
applied to the class. Therefore adding an `(inherit get-color)`
declaration to the `get-price` mixin does not solve the problem.

One solution is to require the use of `(send this get-color)` in methods
such as `get-price`. This change works because `send` always delays the
method lookup until the method call is evaluated. The delayed lookup is
more expensive than a direct call, however. Worse, it also delays
checking whether a `get-color` method even exists.

A second, effective, and efficient solution is to change the encoding of
traits. Specifically, we represent each method as a pair of mixins: one
that introduces the method and one that implements it. When a trait is
applied to a class, all of the method-introducing mixins are applied
first. Then the method-implementing mixins can use `inherit` to directly
access any introduced method.

```racket
(define spots-trait                                      
  (list (list (local-member-name-key get-color)          
              (lambda (get-color get-price %) ....       
                (class % ....                            
                  (define/public (get-color) (void))))   
              (lambda (get-color get-price %) ....       
                (class % ....                            
                  (define/override (get-color) 'black))))
        (list (local-member-name-key get-price)          
              (lambda (get-price get-color %) ....       
                (class % ....                            
                  (define/public (get-price) (void))))   
              (lambda (get-color get-price %) ....       
                (class % ....                            
                  (inherit get-color)                    
                  (define/override (get-price)           
                    .... (get-color) ....))))))          
```

With this trait encoding, `trait-alias` adds a new method with a new
name, but it does not change any references to the old method.

### 8.3. The `trait` Form

The general-purpose trait pattern is clearly too complex for a
programmer to use directly, but it is easily codified in a `trait`
macro:

```racket
(trait trait-clause ...)
```

The `id`s in the optional `inherit` clause are available for direct
reference in the method `expr`s, and they must be supplied either by
other traits or the base class to which the trait is ultimately applied.

Using this form in conjunction with trait operators such as `trait-sum`,
`trait-exclude`, `trait-alias`, and `trait->mixin`, we can implement
`spots-trait` and `stripes-trait` as desired.

```racket
(define spots-trait                                            
  (trait                                                       
    (define/public (get-color) 'black)                         
    (define/public (get-price) ... (get-color) ...)))          
                                                               
(define stripes-trait                                          
  (trait                                                       
    (define/public (get-color) 'red)))                         
                                                               
(define spots+stripes-trait                                    
  (trait-sum                                                   
   (trait-exclude (trait-alias spots-trait                     
                               get-color get-spots-color)      
                  get-color)                                   
   (trait-exclude (trait-alias stripes-trait                   
                               get-color get-stripes-color)    
                  get-color)                                   
   (trait                                                      
     (inherit get-spots-color get-stripes-color)               
     (define/public (get-color)                                
       .... (get-spots-color) .... (get-stripes-color) ....))))
```

## 9. Class Contracts

As classes are values, they can flow across contract boundaries, and we
may wish to protect parts of a given class with contracts.  For this,
the `class/c` form is used.  The `class/c` form has many subforms, which
describe two types of contracts on fields and methods: those that affect
uses via instantiated objects and those that affect subclasses.

### 9.1. External Class Contracts

In its simplest form, `class/c` protects the public fields and methods
of objects instantiated from the contracted class.  There is also an
`object/c` form that can be used to similarly protect the public fields
and methods of a particular object. Take the following definition of
`animal%`, which uses a public field for its `size` attribute:

```racket
(define animal%                                    
  (class object%                                   
    (super-new)                                    
    (field [size 10])                              
    (define/public (eat food)                      
      (set! size (+ size (get-field size food))))))
```

For any instantiated `animal%`, accessing the `size` field should return
a positive number.  Also, if the `size` field is set, it should be
assigned a positive number.  Finally, the `eat` method should receive an
argument which is an object with a `size` field that contains a positive
number. To ensure these conditions, we will define the `animal%` class
with an appropriate contract:

```racket
(define positive/c (and/c number? positive?))         
(define edible/c (object/c (field [size positive/c])))
(define/contract animal%                              
  (class/c (field [size positive/c])                  
           [eat (->m edible/c void?)])                
  (class object%                                      
    (super-new)                                       
    (field [size 10])                                 
    (define/public (eat food)                         
      (set! size (+ size (get-field size food))))))   
```

Here we use `->m` to describe the behavior of `eat` since we do not need
to describe any requirements for the `this` parameter. Now that we have
our contracted class, we can see that the contracts on both `size` and
`eat` are enforced:

```racket
> (define bob (new animal%))                                            
> (set-field! size bob 3)                                               
> (get-field size bob)                                                  
3                                                                       
> (set-field! size bob 'large)                                          
animal%: contract violation                                             
  expected: positive/c                                                  
  given: 'large                                                         
  in: the size field in                                                 
      (class/c                                                          
       (eat                                                             
        (->m                                                            
         (object/c (field (size positive/c)))                           
         void?))                                                        
       (field (size positive/c)))                                       
  contract from: (definition animal%)                                   
  blaming: top-level                                                    
   (assuming the contract is correct)                                   
  at: eval:31.0                                                         
> (define richie (new animal%))                                         
> (send bob eat richie)                                                 
> (get-field size bob)                                                  
13                                                                      
> (define rock (new object%))                                           
> (send bob eat rock)                                                   
eat: contract violation;                                                
 no public field size                                                   
  in: the 1st argument of                                               
      the eat method in                                                 
      (class/c                                                          
       (eat                                                             
        (->m                                                            
         (object/c (field (size positive/c)))                           
         void?))                                                        
       (field (size positive/c)))                                       
  contract from: (definition animal%)                                   
  contract on: animal%                                                  
  blaming: top-level                                                    
   (assuming the contract is correct)                                   
  at: eval:31.0                                                         
> (define giant (new (class object% (super-new) (field [size 'large]))))
> (send bob eat giant)                                                  
eat: contract violation                                                 
  expected: positive/c                                                  
  given: 'large                                                         
  in: the size field in                                                 
      the 1st argument of                                               
      the eat method in                                                 
      (class/c                                                          
       (eat                                                             
        (->m                                                            
         (object/c (field (size positive/c)))                           
         void?))                                                        
       (field (size positive/c)))                                       
  contract from: (definition animal%)                                   
  contract on: animal%                                                  
  blaming: top-level                                                    
   (assuming the contract is correct)                                   
  at: eval:31.0                                                         
```

There are two important caveats for external class contracts. First,
external method contracts are only enforced when the target of dynamic
dispatch is the method implementation of the contracted class, which
lies within the contract boundary.  Overriding that implementation, and
thus changing the target of dynamic dispatch, will mean that the
contract is no longer enforced for clients, since accessing the method
no longer crosses the contract boundary.  Unlike external method
contracts, external field contracts are always enforced for clients of
subclasses, since fields cannot be overridden or shadowed.

Second, these contracts do not restrict subclasses of `animal%` in any
way.  Fields and methods that are inherited and used by subclasses are
not checked by these contracts, and uses of the superclass’s methods via
`super` are also unchecked.  The following example illustrates both
caveats:

```racket                                    
(define large-animal%                        
  (class animal%                             
    (super-new)                              
    (inherit-field size)                     
    (set! size 'large)                       
    (define/override (eat food)              
      (display "Nom nom nom") (newline))))   
```                                          
                                             
```racket                                    
> (define elephant (new large-animal%))      
> (send elephant eat (new object%))          
Nom nom nom                                  
> (get-field size elephant)                  
animal%: broke its own contract              
  promised: positive/c                       
  produced: 'large                           
  in: the size field in                      
      (class/c                               
       (eat                                  
        (->m                                 
         (object/c (field (size positive/c)))
         void?))                             
       (field (size positive/c)))            
  contract from: (definition animal%)        
  blaming: (definition animal%)              
   (assuming the contract is correct)        
  at: eval:31.0                              
```                                          

### 9.2. Internal Class Contracts

Notice that retrieving the `size` field from the object `elephant`
blames `animal%` for the contract violation. This blame is correct, but
unfair to the `animal%` class, as we have not yet provided it with a
method for protecting itself from subclasses.  To this end we add
internal class contracts, which provide directives to subclasses for how
they may access and override features of the superclass.  This
distinction between external and internal class contracts allows for
weaker contracts within the class hierarchy, where invariants may be
broken internally by subclasses but should be enforced for external uses
via instantiated objects.

As a simple example of what kinds of protection are available, we
provide an example aimed at the `animal%` class that uses all the
applicable forms:

```racket
(class/c (field [size positive/c])             
         (inherit-field [size positive/c])     
         [eat (->m edible/c void?)]            
         (inherit [eat (->m edible/c void?)])  
         (super [eat (->m edible/c void?)])    
         (override [eat (->m edible/c void?)]))
```

This class contract not only ensures that objects of class `animal%` are
protected as before, but also ensure that subclasses of `animal%` only
store appropriate values within the `size` field and use the
implementation of `size` from `animal%` appropriately. These contract
forms only affect uses within the class hierarchy, and only for method
calls that cross the contract boundary.

That means that `inherit` will only affect subclass uses of a method
until a subclass overrides that method, and that `override` only affects
calls from the superclass into a subclass’s overriding implementation of
that method.  Since these only affect internal uses, the `override` form
does not automatically enter subclasses into obligations when objects of
those classes are used.  Also, use of `override` only makes sense, and
thus can only be used, for methods where no Beta-style augmentation has
taken place. The following example shows this difference:

```racket
(define/contract sloppy-eater%                       
  (class/c [eat (->m edible/c edible/c)])            
  (begin                                             
    (define/contract glutton%                        
      (class/c (override [eat (->m edible/c void?)]))
      (class animal%                                 
        (super-new)                                  
        (inherit eat)                                
        (define/public (gulp food-list)              
          (for ([f food-list])                       
            (eat f)))))                              
    (class glutton%                                  
      (super-new)                                    
      (inherit-field size)                           
      (define/override (eat f)                       
        (let ([food-size (get-field size f)])        
          (set! size (/ food-size 2))                
          (set-field! size f (/ food-size 2))        
          f)))))                                     
```

```racket
> (define pig (new sloppy-eater%))            
> (define slop1 (new animal%))                
> (define slop2 (new animal%))                
> (define slop3 (new animal%))                
> (send pig eat slop1)                        
(object:animal% ...)                          
> (get-field size slop1)                      
5                                             
> (send pig gulp (list slop1 slop2 slop3))    
eat: broke its own contract                   
  promised: void?                             
  produced: (object:animal% ...)              
  in: the range of                            
      the eat method in                       
      (class/c                                
       (override (eat                         
                  (->m                        
                   (object/c                  
                    (field (size positive/c)))
                   void?))))                  
  contract from: (definition glutton%)        
  contract on: glutton%                       
  blaming: (definition sloppy-eater%)         
   (assuming the contract is correct)         
  at: eval:47.0                               
```

In addition to the internal class contract forms shown here, there are
similar forms for Beta-style augmentable methods.  The `inner` form
describes to the subclass what is expected from augmentations of a given
method.  Both `augment` and `augride` tell the subclass that the given
method is a method which has been augmented and that any calls to the
method in the subclass will dynamically dispatch to the appropriate
implementation in the superclass.  Such calls will be checked according
to the given contract.  The two forms differ in that  use of `augment`
signifies that subclasses can augment the given method, whereas use of
`augride` signifies that subclasses must override the current
augmentation instead.

This means that not all forms can be used at the same time.  Only one of
the `override`, `augment`, and `augride` forms can be used for a given
method, and none of these forms can be used if the given method has been
finalized.  In addition, `super` can be specified for a given method
only if `augride` or `override` can be specified. Similarly, `inner` can
be specified only if `augment` or `augride` can be specified.
