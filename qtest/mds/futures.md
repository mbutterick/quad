# Parallelism with Futures

The `racket/future` library provides support for performance improvement
through parallelism with _futures_ and the `future` and `touch`
functions. The level of parallelism available from those constructs,
however, is limited by several factors, and the current implementation
is best suited to numerical tasks. The caveats in \[missing\] also apply
to futures; notably, the debugging instrumentation currently defeats
futures.

> Other functions, such as `thread`, support the creation of reliably
> concurrent tasks. However, threads never run truly in parallel, even if
> the hardware and operating system support parallelism.

As a starting example, the `any-double?` function below takes a list of
numbers and determines whether any number in the list has a double that
is also in the list:

```racket
(define (any-double? l)       
  (for/or ([i (in-list l)])   
    (for/or ([i2 (in-list l)])
      (= i2 (* 2 i)))))       
```

This function runs in quadratic time, so it can take a long time \(on
the order of a second\) on large lists like `l1` and `l2`:

```racket
(define l1 (for/list ([i (in-range 5000)])
             (+ (* 2 i) 1)))              
(define l2 (for/list ([i (in-range 5000)])
             (- (* 2 i) 1)))              
(or (any-double? l1)                      
    (any-double? l2))                     
```

The best way to speed up `any-double?`  is to use a different algorithm.
However, on a machine that offers at least two processing units, the
example above can run in about half the time using `future` and `touch`:

```racket
(let ([f (future (lambda () (any-double? l2)))])
  (or (any-double? l1)                          
      (touch f)))                               
```

The future `f` runs `(any-double? l2)` in parallel to `(any-double?
l1)`, and the result for `(any-double? l2)` becomes available about the
same time that it is demanded by `(touch f)`.

Futures run in parallel as long as they can do so safely, but the notion
of “future safe” is inherently tied to the implementation. The
distinction between “future safe” and “future unsafe” operations may be
far from apparent at the level of a Racket program. The remainder of
this section works through an example to illustrate this distinction and
to show how to use the future visualizer can help shed light on it.

Consider the following core of a Mandelbrot-set computation:

```racket
(define (mandelbrot iterations x y n)               
  (let ([ci (- (/ (* 2.0 y) n) 1.0)]                
        [cr (- (/ (* 2.0 x) n) 1.5)])               
    (let loop ([i 0] [zr 0.0] [zi 0.0])             
      (if (> i iterations)                          
          i                                         
          (let ([zrq (* zr zr)]                     
                [ziq (* zi zi)])                    
            (cond                                   
              [(> (+ zrq ziq) 4) i]                 
              [else (loop (add1 i)                  
                          (+ (- zrq ziq) cr)        
                          (+ (* 2 zr zi) ci))]))))))
```

The expressions `(mandelbrot 10000000 62 500 1000)` and `(mandelbrot
10000000 62 501 1000)` each take a while to produce an answer. Computing
them both, of course, takes twice as long:

```racket
(list (mandelbrot 10000000 62 500 1000) 
      (mandelbrot 10000000 62 501 1000))
```

Unfortunately, attempting to run the two computations in parallel with
`future` does not improve performance:

```racket
(let ([f (future (lambda () (mandelbrot 10000000 62 501 1000)))])
  (list (mandelbrot 10000000 62 500 1000)                        
        (touch f)))                                              
```

To see why, use the `future-visualizer`, like this:

```racket
(require future-visualizer)                                       
(visualize-futures                                                
 (let ([f (future (lambda () (mandelbrot 10000000 62 501 1000)))])
   (list (mandelbrot 10000000 62 500 1000)                        
         (touch f))))                                             
```

This opens a window showing a graphical view of a trace of the
computation. The upper-left portion of the window contains an execution
timeline:

`#<pict>`

Each horizontal row represents an OS-level thread, and the colored dots
represent important events in the execution of the program \(they are
color-coded to distinguish one event type from another\).  The
upper-left blue dot in the timeline represents the future’s creation.
The future executes for a brief period \(represented by a green bar in
the second line\) on thread 1, and then pauses to allow the runtime
thread to perform a future-unsafe operation.

In the Racket implementation, future-unsafe operations fall into one of
two categories. A _blocking_ operation halts the evaluation of the
future, and will not allow it to continue until it is touched.  After
the operation completes within `touch`, the remainder of the future’s
work will be evaluated sequentially by the runtime thread.  A
_synchronized_ operation also halts the future, but the runtime thread
may perform the operation at any time and, once completed, the future
may continue running in parallel.  Memory allocation and JIT compilation
are two common examples of synchronized operations.

In the timeline, we see an orange dot just to the right of the green bar
on thread 1 – this dot represents a synchronized operation \(memory
allocation\).  The first orange dot on thread 0 shows that the runtime
thread performed the allocation shortly after the future paused.  A
short time later, the future halts on a blocking operation \(the first
red dot\) and must wait until the `touch` for it to be evaluated
\(slightly after the 1049ms mark\).

When you move your mouse over an event, the visualizer shows you
detailed information about the event and draws arrows connecting all of
the events in the corresponding future. This image shows those
connections for our future.

`#<pict>`

The dotted orange line connects the first event in the future to the
future that created it, and the purple lines connect adjacent events
within the future.

The reason that we see no parallelism is that the `<` and `*` operations
in the lower portion of the loop in `mandelbrot` involve a mixture of
floating-point and fixed \(integer\) values.  Such mixtures typically
trigger a slow path in execution, and the general slow path will usually
be blocking.

Changing constants to be floating-points numbers in `mandelbrot`
addresses that first problem:

```racket
(define (mandelbrot iterations x y n)                 
  (let ([ci (- (/ (* 2.0 y) n) 1.0)]                  
        [cr (- (/ (* 2.0 x) n) 1.5)])                 
    (let loop ([i 0] [zr 0.0] [zi 0.0])               
      (if (> i iterations)                            
          i                                           
          (let ([zrq (* zr zr)]                       
                [ziq (* zi zi)])                      
            (cond                                     
              [(> (+ zrq ziq) 4.0) i]                 
              [else (loop (add1 i)                    
                          (+ (- zrq ziq) cr)          
                          (+ (* 2.0 zr zi) ci))]))))))
```

With that change, `mandelbrot` computations can run in parallel.
Nevertheless, we still see a special type of slow-path operation
limiting our parallelism \(orange dots\):

`#<pict>`

The problem is that most every arithmetic operation in this example
produces an inexact number whose storage must be allocated.  While some
allocation can safely be performed exclusively without the aid of the
runtime thread, especially frequent allocation requires synchronized
operations which defeat any performance improvement.

By using flonum-specific operations \(see \[missing\]\), we can re-write
`mandelbrot` to use much less allocation:

```racket
(define (mandelbrot iterations x y n)                           
  (let ([ci (fl- (fl/ (* 2.0 (->fl y)) (->fl n)) 1.0)]          
        [cr (fl- (fl/ (* 2.0 (->fl x)) (->fl n)) 1.5)])         
    (let loop ([i 0] [zr 0.0] [zi 0.0])                         
      (if (> i iterations)                                      
          i                                                     
          (let ([zrq (fl* zr zr)]                               
                [ziq (fl* zi zi)])                              
            (cond                                               
              [(fl> (fl+ zrq ziq) 4.0) i]                       
              [else (loop (add1 i)                              
                          (fl+ (fl- zrq ziq) cr)                
                          (fl+ (fl* 2.0 (fl* zr zi)) ci))]))))))
```

This conversion can speed `mandelbrot` by a factor of 8, even in
sequential mode, but avoiding allocation also allows `mandelbrot` to run
usefully faster in parallel. Executing this program yields the following
in the visualizer:

`#<pict>`

Notice that only one green bar is shown here because one of the
mandelbrot computations is not being evaluated by a future \(on the
runtime thread\).

As a general guideline, any operation that is inlined by the JIT
compiler runs safely in parallel, while other operations that are not
inlined \(including all operations if the JIT compiler is disabled\) are
considered unsafe. The `raco decompile` tool annotates operations that
can be inlined by the compiler \(see \[missing\]\), so the decompiler
can be used to help predict parallel performance.
