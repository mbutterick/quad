# Module Paths

A _module path_ is a reference to a module, as used with `require` or as
the `initial-module-path` in a `module` form. It can be any of several
forms:

```racket
(quote id)
```
A module path that is a quoted identifier refers to a non-file `module`
declaration using the identifier. This form of module reference makes
the most sense in a REPL.
Examples:

```racket
> (module m racket                             
    (provide color)                            
    (define color "blue"))                     
> (module n racket                             
    (require 'm)                               
    (printf "my favorite color is ~a\n" color))
> (require 'n)                                 
my favorite color is blue                      
```

```racket
rel-string
```
A string module path is a relative path using Unix-style conventions:
`/` is the path separator, `..` refers to the parent directory, and `.`
refers to the same directory. The `rel-string` must not start or end
with a path separator. If the path has no suffix, `".rkt"` is added
automatically.
The path is relative to the enclosing file, if any, or it is relative to
the current directory. \(More precisely, the path is relative to the
value of `(current-load-relative-directory)`, which is set while loading
a file.\)
\[missing\] shows examples using relative paths.
If a relative path ends with a `".ss"` suffix, it is converted to
`".rkt"`. If the file that implements the referenced module actually
ends in `".ss"`, the suffix will be changed back when attempting to load
the file \(but a `".rkt"` suffix takes precedence\). This two-way
conversion provides compatibility with older versions of Racket.

```racket
id
```
A module path that is an unquoted identifier refers to an installed
library. The `id` is constrained to contain only ASCII letters, ASCII
numbers, `+`, `-`, `_`, and `/`, where `/` separates path elements
within the identifier. The elements refer to collections and
sub-collections, instead of directories and sub-directories.
An example of this form is `racket/date`. It refers to the module whose
source is the `"date.rkt"` file in the `"racket"` collection, which is
installed as part of Racket. The `".rkt"` suffix is added automatically.
Another example of this form is `racket`, which is commonly used at the
initial import. The path `racket` is shorthand for `racket/main`; when
an `id` has no `/`, then `/main` is automatically added to the end.
Thus, `racket` or `racket/main` refers to the module whose source is the
`"main.rkt"` file in the `"racket"` collection.
Examples:

```racket
> (module m racket                                            
    (require racket/date)                                     
                                                              
    (printf "Today is ~s\n"                                   
            (date->string (seconds->date (current-seconds)))))
> (require 'm)                                                
Today is "Monday, January 21st, 2019"                         
```
When the full path of a module ends with `".rkt"`, if no such file
exists but one does exist with the `".ss"` suffix, then the `".ss"`
suffix is substituted automatically. This transformation provides
compatibility with older versions of Racket.

```racket
(lib rel-string)
```
Like an unquoted-identifier path, but expressed as a string instead of
an identifier. Also, the `rel-string` can end with a file suffix, in
which case `".rkt"` is not automatically added.
Example of this form include `(lib "racket/date.rkt")` and `(lib
"racket/date")`, which are equivalent to `racket/date`. Other examples
include `(lib "racket")`, `(lib "racket/main")`, and `(lib
"racket/main.rkt")`, which are all equivalent to `racket`.
Examples:

```racket
> (module m (lib "racket")                                    
    (require (lib "racket/date.rkt"))                         
                                                              
    (printf "Today is ~s\n"                                   
            (date->string (seconds->date (current-seconds)))))
> (require 'm)                                                
Today is "Monday, January 21st, 2019"                         
```

```racket
(planet id)
```
Accesses a third-party library that is distributed through the PLaneT
server. The library is downloaded the first time that it is needed, and
then the local copy is used afterward.
The `id` encodes several pieces of information separated by a `/`: the
package owner, then package name with optional version information, and
an optional path to a specific library with the package. Like `id` as
shorthand for a `lib` path, a `".rkt"` suffix is added automatically,
and `/main` is used as the path if no sub-path element is supplied.
Examples:

```racket
> (module m (lib "racket")                                   
    ; Use "schematics"'s "random.plt" 1.0, file "random.rkt":
    (require (planet schematics/random:1/random))            
    (display (random-gaussian)))                             
> (require 'm)                                               
0.9050686838895684                                           
```
As with other forms, an implementation file ending with `".ss"` can be
substituted automatically if no implementation file ending with `".rkt"`
exists.

```racket
(planet package-string)
```
Like the symbol form of a `planet`, but using a string instead of an
identifier. Also, the `package-string` can end with a file suffix, in
which case `".rkt"` is not added.
As with other forms, an `".ss"` extension is converted to `".rkt"`,
while an implementation file ending with `".ss"` can be substituted
automatically if no implementation file ending with `".rkt"` exists.

```racket
(planet rel-string (user-string pkg-string vers ...))
                                                     
vers = nat                                           
     | (nat nat)                                     
     | (= nat)                                       
     | (+ nat)                                       
     | (- nat)                                       
```
A more general form to access a library from the PLaneT server. In this
general form, a PLaneT reference starts like a `lib` reference with a
relative path, but the path is followed by information about the
producer, package, and version of the library. The specified package is
downloaded and installed on demand.
The `vers`es specify a constraint on the acceptable version of the
package, where a version number is a sequence of non-negative integers,
and the constraints determine the allowable values for each element in
the sequence. If no constraint is provided for a particular element,
then any version is allowed; in particular, omitting all `vers`es means
that any version is acceptable. Specifying at least one `vers` is
strongly recommended.
For a version constraint, a plain `nat` is the same as `(+ nat)`, which
matches `nat` or higher for the corresponding element of the version
number.  A `(start-nat end-nat)` matches any number in the range
`start-nat` to `end-nat`, inclusive. A `(= nat)` matches only exactly
`nat`. A `(- nat)` matches `nat` or lower.
Examples:

```racket
> (module m (lib "racket")                                         
    (require (planet "random.rkt" ("schematics" "random.plt" 1 0)))
    (display (random-gaussian)))                                   
> (require 'm)                                                     
0.9050686838895684                                                 
```
The automatic `".ss"` and `".rkt"` conversions apply as with other
forms.

```racket
(file string)
```
Refers to a file, where `string` is a relative or absolute path using
the current platform’s conventions. This form is not portable, and it
should _not_ be used when a plain, portable `rel-string` suffices.
The automatic `".ss"` and `".rkt"` conversions apply as with other
forms.

```racket
(submod base element ...+)
                          
base    = module-path     
        | "."             
        | ".."            
                          
element = id              
        | ".."            
```
Refers to a submodule of `base`. The sequence of `element`s within
`submod` specify a path of submodule names to reach the final submodule.
Examples:

```racket
> (module zoo racket                    
    (module monkey-house racket         
      (provide monkey)                  
      (define monkey "Curious George")))
> (require (submod 'zoo monkey-house))  
> monkey                                
"Curious George"                        
```
Using `"."` as `base` within `submod` stands for the enclosing module.
Using `".."` as `base` is equivalent to using `"."` followed by an extra
`".."`. When a path of the form `(quote id)` refers to a submodule, it
is equivalent to `(submod "." id)`.
Using `".."` as an `element` cancels one submodule step, effectively
referring to the enclosing module. For example, `(submod "..")` refers
to the enclosing module of the submodule in which the path appears.
Examples:

```racket
> (module zoo racket                      
    (module monkey-house racket           
      (provide monkey)                    
      (define monkey "Curious George"))   
    (module crocodile-house racket        
      (require (submod ".." monkey-house))
      (provide dinner)                    
      (define dinner monkey)))            
> (require (submod 'zoo crocodile-house)) 
> dinner                                  
"Curious George"                          
```
