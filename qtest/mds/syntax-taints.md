# Syntax Taints

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

## 1. Tainting Modes

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

## 2. Taints and Code Inspectors

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

## 3. Protected Exports

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
