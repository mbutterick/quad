# Performance

Alan Perlis famously quipped “Lisp programmers know the value of
everything and the cost of nothing.” A Racket programmer knows, for
example, that a `lambda` anywhere in a program produces a value that is
closed over its lexical environment—but how much does allocating that
value cost? While most programmers have a reasonable grasp of the cost
of various operations and data structures at the machine level, the gap
between the Racket language model and the underlying computing machinery
can be quite large.

In this chapter, we narrow the gap by explaining details of the Racket
compiler and run-time system and how they affect the run-time and memory
performance of Racket code.

## 1. Performance in DrRacket

By default, DrRacket instruments programs for debugging, and debugging
instrumentation \(provided by the \[missing\] library\) can
significantly degrade performance for some programs. Even when debugging
is disabled through the Choose Language... dialog’s Show Details panel,
the Preserve stacktrace checkbox is clicked by default, which also
affects performance. Disabling debugging and stacktrace preservation
provides performance results that are more consistent with running in
plain `racket`.

Even so, DrRacket and programs developed within DrRacket use the same
Racket virtual machine, so garbage collection times \(see Memory
Management\) may be longer in DrRacket than when a program is run by
itself, and DrRacket threads may impede execution of program threads.
**For the most reliable timing results for a program, run in plain
`racket` instead of in the DrRacket development environment.**
Non-interactive mode should be used instead of the REPL to benefit from
the module system. See Modules and Performance for details.

## 2. The Bytecode and Just-in-Time \(JIT\) Compilers

Every definition or expression to be evaluated by Racket is compiled to
an internal bytecode format. In interactive mode, this compilation
occurs automatically and on-the-fly. Tools like `raco make` and `raco
setup` marshal compiled bytecode to a file, so that you do not have to
compile from source every time that you run a program. \(Most of the
time required to compile a file is actually in macro expansion;
generating bytecode from fully expanded code is relatively fast.\) See
\[missing\] for more information on generating bytecode files.

The bytecode compiler applies all standard optimizations, such as
constant propagation, constant folding, inlining, and dead-code
elimination. For example, in an environment where `+` has its usual
binding, the expression `(let ([x 1] [y (lambda () 4)]) (+ 1 (y)))` is
compiled the same as the constant `5`.

On some platforms, bytecode is further compiled to native code via a
_just-in-time_ or _JIT_ compiler. The JIT compiler substantially speeds
programs that execute tight loops, arithmetic on small integers, and
arithmetic on inexact real numbers. Currently, JIT compilation is
supported for x86, x86\_64 \(a.k.a. AMD64\), ARM, and 32-bit PowerPC
processors. The JIT compiler can be disabled via the `eval-jit-enabled`
parameter or the `--no-jit`/`-j` command-line flag for `racket`.

The JIT compiler works incrementally as functions are applied, but the
JIT compiler makes only limited use of run-time information when
compiling procedures, since the code for a given module body or `lambda`
abstraction is compiled only once. The JIT’s granularity of compilation
is a single procedure body, not counting the bodies of any lexically
nested procedures. The overhead for JIT compilation is normally so small
that it is difficult to detect.

## 3. Modules and Performance

The module system aids optimization by helping to ensure that
identifiers have the usual bindings. That is, the `+` provided by
`racket/base` can be recognized by the compiler and inlined, which is
especially important for JIT-compiled code. In contrast, in a
traditional interactive Scheme system, the top-level `+` binding might
be redefined, so the compiler cannot assume a fixed `+` binding \(unless
special flags or declarations are used to compensate for the lack of a
module system\).

Even in the top-level environment, importing with `require` enables some
inlining optimizations. Although a `+` definition at the top level might
shadow an imported `+`, the shadowing definition applies only to
expressions evaluated later.

Within a module, inlining and constant-propagation optimizations take
additional advantage of the fact that definitions within a module cannot
be mutated when no `set!` is visible at compile time. Such optimizations
are unavailable in the top-level environment. Although this optimization
within modules is important for performance, it hinders some forms of
interactive development and exploration. The
`compile-enforce-module-constants` parameter disables the JIT compiler’s
assumptions about module definitions when interactive exploration is
more important. See \[missing\] for more information.

The compiler may inline functions or propagate constants across module
boundaries. To avoid generating too much code in the case of function
inlining, the compiler is conservative when choosing candidates for
cross-module inlining; see Function-Call Optimizations for information
on providing inlining hints to the compiler.

The later section `letrec` Performance provides some additional caveats
concerning inlining of module bindings.

## 4. Function-Call Optimizations

When the compiler detects a function call to an immediately visible
function, it generates more efficient code than for a generic call,
especially for tail calls. For example, given the program

```racket
(letrec ([odd (lambda (x)              
                (if (zero? x)          
                    #f                 
                    (even (sub1 x))))] 
         [even (lambda (x)             
                 (if (zero? x)         
                     #t                
                     (odd (sub1 x))))])
  (odd 40000000))                      
```

the compiler can detect the `odd`–`even` loop and produce code that runs
much faster via loop unrolling and related optimizations.

Within a module form, `define`d variables are lexically scoped like
`letrec` bindings, and definitions within a module therefore permit call
optimizations, so

```racket
(define (odd x) ....) 
(define (even x) ....)
```

within a module would perform the same as the `letrec` version.

For direct calls to functions with keyword arguments, the compiler can
typically check keyword arguments statically and generate a direct call
to a non-keyword variant of the function, which reduces the run-time
overhead of keyword checking. This optimization applies only for
keyword-accepting procedures that are bound with `define`.

For immediate calls to functions that are small enough, the compiler may
inline the function call by replacing the call with the body of the
function. In addition to the size of the target function’s body, the
compiler’s heuristics take into account the amount of inlining already
performed at the call site and whether the called function itself calls
functions other than simple primitive operations. When a module is
compiled, some functions defined at the module level are determined to
be candidates for inlining into other modules; normally, only trivial
functions are considered candidates for cross-module inlining, but a
programmer can wrap a function definition with `begin-encourage-inline`
to encourage inlining of the function.

Primitive operations like `pair?`, `car`, and `cdr` are inlined at the
machine-code level by the JIT compiler. See also the later section
Fixnum and Flonum Optimizations for information about inlined arithmetic
operations.

## 5. Mutation and Performance

Using `set!` to mutate a variable can lead to bad performance. For
example, the microbenchmark

```racket
#lang racket/base                 
                                  
(define (subtract-one x)          
  (set! x (sub1 x))               
  x)                              
                                  
(time                             
  (let loop ([n 4000000])         
    (if (zero? n)                 
        'done                     
        (loop (subtract-one n)))))
```

runs much more slowly than the equivalent

```racket
#lang racket/base                 
                                  
(define (subtract-one x)          
  (sub1 x))                       
                                  
(time                             
  (let loop ([n 4000000])         
    (if (zero? n)                 
        'done                     
        (loop (subtract-one n)))))
```

In the first variant, a new location is allocated for `x` on every
iteration, leading to poor performance. A more clever compiler could
unravel the use of `set!` in the first example, but since mutation is
discouraged \(see \[missing\]\), the compiler’s effort is spent
elsewhere.

More significantly, mutation can obscure bindings where inlining and
constant-propagation might otherwise apply. For example, in

```racket
(let ([minus1 #f])          
  (set! minus1 sub1)        
  (let loop ([n 4000000])   
    (if (zero? n)           
        'done               
        (loop (minus1 n)))))
```

the `set!` obscures the fact that `minus1` is just another name for the
built-in `sub1`.

## 6. `letrec` Performance

When `letrec` is used to bind only procedures and literals, then the
compiler can treat the bindings in an optimal manner, compiling uses of
the bindings efficiently. When other kinds of bindings are mixed with
procedures, the compiler may be less able to determine the control flow.

For example,

```racket
(letrec ([loop (lambda (x)            
                (if (zero? x)         
                    'done             
                    (loop (next x))))]
         [junk (display loop)]        
         [next (lambda (x) (sub1 x))])
  (loop 40000000))                    
```

likely compiles to less efficient code than

```racket
(letrec ([loop (lambda (x)            
                (if (zero? x)         
                    'done             
                    (loop (next x))))]
         [next (lambda (x) (sub1 x))])
  (loop 40000000))                    
```

In the first case, the compiler likely does not know that `display` does
not call `loop`. If it did, then `loop` might refer to `next` before the
binding is available.

This caveat about `letrec` also applies to definitions of functions and
constants as internal definitions or in modules. A definition sequence
in a module body is analogous to a sequence of `letrec` bindings, and
non-constant expressions in a module body can interfere with the
optimization of references to later bindings.

## 7. Fixnum and Flonum Optimizations

A _fixnum_ is a small exact integer. In this case, “small” depends on
the platform. For a 32-bit machine, numbers that can be expressed in 30
bits plus a sign bit are represented as fixnums. On a 64-bit machine, 62
bits plus a sign bit are available.

A _flonum_ is used to represent any inexact real number. They correspond
to 64-bit IEEE floating-point numbers on all platforms.

Inlined fixnum and flonum arithmetic operations are among the most
important advantages of the JIT compiler. For example, when `+` is
applied to two arguments, the generated machine code tests whether the
two arguments are fixnums, and if so, it uses the machine’s instruction
to add the numbers \(and check for overflow\). If the two numbers are
not fixnums, then it checks whether both are flonums; in that case, the
machine’s floating-point operations are used directly. For functions
that take any number of arguments, such as `+`, inlining works for two
or more arguments \(except for `-`, whose one-argument case is also
inlined\) when the arguments are either all fixnums or all flonums.

Flonums are typically _boxed_, which means that memory is allocated to
hold every result of a flonum computation. Fortunately, the generational
garbage collector \(described later in Memory Management\) makes
allocation for short-lived results reasonably cheap. Fixnums, in
contrast are never boxed, so they are typically cheap to use.

> See \[missing\] for an example use of flonum-specific operations.

The `racket/flonum` library provides flonum-specific operations, and
combinations of flonum operations allow the JIT compiler to generate
code that avoids boxing and unboxing intermediate results. Besides
results within immediate combinations, flonum-specific results that are
bound with `let` and consumed by a later flonum-specific operation are
unboxed within temporary storage. Unboxing applies most reliably to uses
of a flonum-specific operation with two arguments. Finally, the compiler
can detect some flonum-valued loop accumulators and avoid boxing of the
accumulator. The bytecode decompiler \(see \[missing\]\) annotates
combinations where the JIT can avoid boxes with `#%flonum`,
`#%as-flonum`, and `#%from-flonum`.

> Unboxing of local bindings and accumulators is not supported by the JIT
> for PowerPC.

The `racket/unsafe/ops` library provides unchecked fixnum- and
flonum-specific operations. Unchecked flonum-specific operations allow
unboxing, and sometimes they allow the compiler to reorder expressions
to improve performance. See also Unchecked, Unsafe Operations,
especially the warnings about unsafety.

## 8. Unchecked, Unsafe Operations

The `racket/unsafe/ops` library provides functions that are like other
functions in `racket/base`, but they assume \(instead of checking\) that
provided arguments are of the right type. For example,
`unsafe-vector-ref` accesses an element from a vector without checking
that its first argument is actually a vector and without checking that
the given index is in bounds. For tight loops that use these functions,
avoiding checks can sometimes speed the computation, though the benefits
vary for different unchecked functions and different contexts.

Beware that, as “unsafe” in the library and function names suggest,
misusing the exports of `racket/unsafe/ops` can lead to crashes or
memory corruption.

## 9. Foreign Pointers

The `ffi/unsafe` library provides functions for unsafely reading and
writing arbitrary pointer values. The JIT recognizes uses of `ptr-ref`
and `ptr-set!` where the second argument is a direct reference to one of
the following built-in C types: `_int8`, `_int16`, `_int32`, `_int64`,
`_double`, `_float`, and `_pointer`. Then, if the first argument to
`ptr-ref` or `ptr-set!` is a C pointer \(not a byte string\), then the
pointer read or write is performed inline in the generated code.

The bytecode compiler will optimize references to integer abbreviations
like `_int` to C types like `_int32`—where the representation sizes are
constant across platforms—so the JIT can specialize access with those C
types. C types such as `_long` or `_intptr` are not constant across
platforms, so their uses are currently not specialized by the JIT.

Pointer reads and writes using `_float` or `_double` are not currently
subject to unboxing optimizations.

## 10. Regular Expression Performance

When a string or byte string is provided to a function like
`regexp-match`, then the string is internally compiled into a regexp
value. Instead of supplying a string or byte string multiple times as a
pattern for matching, compile the pattern once to a regexp value using
`regexp`, `byte-regexp`, `pregexp`, or `byte-pregexp`. In place of a
constant string or byte string, write a constant regexp using an `#rx`
or `#px` prefix.

```racket
(define (slow-matcher str)                
  (regexp-match? "[0-9]+" str))           
                                          
(define (fast-matcher str)                
  (regexp-match? #rx"[0-9]+" str))        
                                          
(define (make-slow-matcher pattern-str)   
  (lambda (str)                           
    (regexp-match? pattern-str str)))     
                                          
(define (make-fast-matcher pattern-str)   
  (define pattern-rx (regexp pattern-str))
  (lambda (str)                           
    (regexp-match? pattern-rx str)))      
```

## 11. Memory Management

The Racket implementation is available in three variants: _3m_, _CGC_,
and _CS_. The 3m and CS variants use a modern, _generational garbage
collector_ that makes allocation relatively cheap for short-lived
objects. The CGC variant uses a _conservative garbage collector_ which
facilitates interaction with C code at the expense of both precision and
speed for Racket memory management. The 3m variant is currently the
standard one.

Although memory allocation is reasonably cheap, avoiding allocation
altogether is normally faster. One particular place where allocation can
be avoided sometimes is in _closures_, which are the run-time
representation of functions that contain free variables. For example,

```racket
(let loop ([n 40000000] [prev-thunk (lambda () #f)])
  (if (zero? n)                                     
      (prev-thunk)                                  
      (loop (sub1 n)                                
            (lambda () n))))                        
```

allocates a closure on every iteration, since `(lambda () n)`
effectively saves `n`.

The compiler can eliminate many closures automatically. For example, in

```racket
(let loop ([n 40000000] [prev-val #f]) 
  (let ([prev-thunk (lambda () n)])    
    (if (zero? n)                      
        prev-val                       
        (loop (sub1 n) (prev-thunk)))))
```

no closure is ever allocated for `prev-thunk`, because its only
application is visible, and so it is inlined. Similarly, in

```racket
(let n-loop ([n 400000])         
  (if (zero? n)                  
      'done                      
      (let m-loop ([m 100])      
        (if (zero? m)            
            (n-loop (sub1 n))    
            (m-loop (sub1 m))))))
```

then the expansion of the `let` form to implement `m-loop` involves a
closure over `n`, but the compiler automatically converts the closure to
pass itself `n` as an argument instead.

## 12. Reachability and Garbage Collection

In general, Racket re-uses the storage for a value when the garbage
collector can prove that the object is unreachable from any other
\(reachable\) value. Reachability is a low-level, abstraction breaking
concept \(and thus one must understand many details of the runtime
system’s implementation to accurate predicate precisely when values are
reachable from each other\), but generally speaking one value is
reachable from a second one when there is some operation to recover the
original value from the second one.

To help programmers understand when an object is no longer reachable and
its storage can be reused, Racket provides `make-weak-box` and
`weak-box-value`, the creator and accessor for a one-record struct that
the garbage collector treats specially. An object inside a weak box does
not count as reachable, and so `weak-box-value` might return the object
inside the box, but it might also return `#f` to indicate that the
object was otherwise unreachable and garbage collected. Note that unless
a garbage collection actually occurs, the value will remain inside the
weak box, even if it is unreachable.

For example, consider this program:

```racket
#lang racket                              
(struct fish (weight color) #:transparent)
(define f (fish 7 'blue))                 
(define b (make-weak-box f))              
(printf "b has ~s\n" (weak-box-value b))  
(collect-garbage)                         
(printf "b has ~s\n" (weak-box-value b))  
```

It will print `b has #(struct:fish 7 blue)` twice because the definition
of `f` still holds onto the fish. If the program were this, however:

```racket
#lang racket                              
(struct fish (weight color) #:transparent)
(define f (fish 7 'blue))                 
(define b (make-weak-box f))              
(printf "b has ~s\n" (weak-box-value b))  
(set! f #f)                               
(collect-garbage)                         
(printf "b has ~s\n" (weak-box-value b))  
```

the second printout will be `b has #f` because no reference to the fish
exists \(other than the one in the box\).

As a first approximation, all values in Racket must be allocated and
will demonstrate behavior similar to the fish above. There are a number
of exceptions, however:

* Small integers \(recognizable with `fixnum?`\) are always available
  without explicit allocation. From the perspective of the garbage
  collector and weak boxes, their storage is never reclaimed. \(Due to
  clever representation techniques, however, their storage does not
  count towards the space that Racket uses. That is, they are
  effectively free.\)

* Procedures where the compiler can see all of their call sites may
  never be allocated at all \(as discussed above\). Similar
  optimizations may also eliminate the allocation for other kinds of
  values.

* Interned symbols are allocated only once \(per place\). A table inside
  Racket tracks this allocation so a symbol may not become garbage
  because that table holds onto it.

* Reachability is only approximate with the CGC collector \(i.e., a
  value may appear reachable to that collector when there is, in fact,
  no way to reach it anymore\).

## 13. Weak Boxes and Testing

One important use of weak boxes is in testing that some abstraction
properly releases storage for data it no longer needs, but there is a
gotcha that can easily cause such test cases to pass improperly.

Imagine you’re designing a data structure that needs to hold onto some
value temporarily but then should clear a field or somehow break a link
to avoid referencing that value so it can be collected. Weak boxes are a
good way to test that your data structure properly clears the value.
This is, you might write a test case that builds a value, extracts some
other value from it \(that you hope becomes unreachable\), puts the
extracted value into a weak-box, and then checks to see if the value
disappears from the box.

This code is one attempt to follow that pattern, but it has a subtle
bug:

```racket
#lang racket                                       
(let* ([fishes (list (fish 8 'red)                 
                     (fish 7 'blue))]              
       [wb (make-weak-box (list-ref fishes 0))])   
  (collect-garbage)                                
  (printf "still there? ~s\n" (weak-box-value wb)))
```

Specifically, it will show that the weak box is empty, but not because
`fishes` no longer holds onto the value, but because `fishes` itself is
not reachable anymore!

Change the program to this one:

```racket
#lang racket                                      
(let* ([fishes (list (fish 8 'red)                
                     (fish 7 'blue))]             
       [wb (make-weak-box (list-ref fishes 0))])  
  (collect-garbage)                               
  (printf "still there? ~s\n" (weak-box-value wb))
  (printf "fishes is ~s\n" fishes))               
```

and now we see the expected result. The difference is that last
occurrence of the variable `fishes`. That constitutes a reference to the
list, ensuring that the list is not itself garbage collected, and thus
the red fish is not either.

## 14. Reducing Garbage Collection Pauses

By default, Racket’s generational garbage collector creates brief pauses
for frequent _minor collections_, which inspect only the most recently
allocated objects, and long pauses for infrequent _major collections_,
which re-inspect all memory.

For some applications, such as animations and games, long pauses due to
a major collection can interfere unacceptably with a program’s
operation. To reduce major-collection pauses, the Racket garbage
collector supports _incremental garbage-collection_ mode. In incremental
mode, minor collections create longer \(but still relatively short\)
pauses by performing extra work toward the next major collection. If all
goes well, most of a major collection’s work has been performed by minor
collections the time that a major collection is needed, so the major
collection’s pause is as short as a minor collection’s pause.
Incremental mode tends to run more slowly overall, but it can provide
much more consistent real-time behavior.

If the `PLT_INCREMENTAL_GC` environment variable is set to a value that
starts with `1`, `y`, or `Y` when Racket starts, incremental mode is
permanently enabled. Since incremental mode is only useful for certain
parts of some programs, however, and since the need for incremental mode
is a property of a program rather than its environment, the preferred
way to enable incremental mode is with `(collect-garbage 'incremental)`.

Calling `(collect-garbage 'incremental)` does not perform an immediate
garbage collection, but instead requests that each minor collection
perform incremental work up to the next major collection. The request
expires with the next major collection. Make a call to `(collect-garbage
'incremental)` in any repeating task within an application that needs to
be responsive in real time. Force a full collection with
`(collect-garbage)` just before an initial `(collect-garbage
'incremental)` to initiate incremental mode from an optimal state.

To check whether incremental mode is in use and how it affects pause
times, enable `debug`-level logging output for the `GC` topic. For
example,

  `racket -W "debuG@GC error" main.rkt`

runs `"main.rkt"` with garbage-collection logging to stderr \(while
preserving `error`-level logging for all topics\). Minor collections are
reported by `min` lines, increment-mode minor collection are reported
with `mIn` lines, and major collections are reported with `MAJ` lines.
