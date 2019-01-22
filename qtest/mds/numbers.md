# Numbers

A Racket _number_ is either exact or inexact:

* An _exact_ number is either

  * an arbitrarily large or small integer, such as `5`,
    `99999999999999999`, or `-17`;

  * a rational that is exactly the ratio of two arbitrarily small or
    large integers, such as `1/2`, `99999999999999999/2`, or `-3/4`; or

  * a complex number with exact real and imaginary parts \(where the
    imaginary part is not zero\), such as `1+2i` or `1/2+3/4i`.

* An _inexact_ number is either

  * an IEEE floating-point representation of a number, such as `2.0` or
    `3.14e+87`, where the IEEE infinities and not-a-number are written
    `+inf.0`, `-inf.0`, and `+nan.0` \(or `-nan.0`\); or

  * a complex number with real and imaginary parts that are IEEE
    floating-point representations, such as `2.0+3.0i` or
    `-inf.0+nan.0i`; as a special case, an inexact complex number can
    have an exact zero real part with an inexact imaginary part.

Inexact numbers print with a decimal point or exponent specifier, and
exact numbers print as integers and fractions.  The same conventions
apply for reading number constants, but `#e` or `#i` can prefix a number
to force its parsing as an exact or inexact number. The prefixes `#b`,
`#o`, and `#x` specify binary, octal, and hexadecimal interpretation of
digits.

> +\[missing\] in \[missing\] documents the fine points of the syntax of
> numbers.

Examples:

```racket
> 0.5   
0.5     
> #e0.5 
1/2     
> #x03BB
955     
```

Computations that involve an inexact number produce inexact results, so
that inexactness acts as a kind of taint on numbers. Beware, however,
that Racket offers no “inexact booleans,” so computations that branch on
the comparison of inexact numbers can nevertheless produce exact
results. The procedures `exact->inexact` and `inexact->exact` convert
between the two types of numbers.

Examples:

```racket
> (/ 1 2)                         
1/2                               
> (/ 1 2.0)                       
0.5                               
> (if (= 3.0 2.999) 1 2)          
2                                 
> (inexact->exact 0.1)            
3602879701896397/36028797018963968
```

Inexact results are also produced by procedures such as `sqrt`, `log`,
and `sin` when an exact result would require representing real numbers
that are not rational. Racket can represent only rational numbers and
complex numbers with rational parts.

Examples:

```racket
> (sin 0)   ; rational...    
0                            
> (sin 1/2) ; not rational...
0.479425538604203            
```

In terms of performance, computations with small integers are typically
the fastest, where “small” means that the number fits into one bit less
than the machine’s word-sized representation for signed numbers.
Computation with very large exact integers or with non-integer exact
numbers can be much more expensive than computation with inexact
numbers.

```racket                                             
(define (sigma f a b)                                 
  (if (= a b)                                         
      0                                               
      (+ (f a) (sigma f (+ a 1) b))))                 
```                                                   
                                                      
```racket                                             
> (time (round (sigma (lambda (x) (/ 1 x)) 1 2000)))  
cpu time: 80 real time: 80 gc time: 17                
8                                                     
> (time (round (sigma (lambda (x) (/ 1.0 x)) 1 2000)))
cpu time: 1 real time: 1 gc time: 0                   
8.0                                                   
```                                                   

The number categories _integer_, _rational_, _real_ \(always rational\),
and _complex_ are defined in the usual way, and are recognized by the
procedures `integer?`, `rational?`, `real?`, and `complex?`, in addition
to the generic `number?`. A few mathematical procedures accept only real
numbers, but most implement standard extensions to complex numbers.

Examples:

```racket
> (integer? 5)                        
#t                                    
> (complex? 5)                        
#t                                    
> (integer? 5.0)                      
#t                                    
> (integer? 1+2i)                     
#f                                    
> (complex? 1+2i)                     
#t                                    
> (complex? 1.0+2.0i)                 
#t                                    
> (abs -5)                            
5                                     
> (abs -5+2i)                         
abs: contract violation               
  expected: real?                     
  given: -5+2i                        
> (sin -5+2i)                         
3.6076607742131563+1.0288031496599337i
```

The `=` procedure compares numbers for numerical equality. If it is
given both inexact and exact numbers to compare, it essentially converts
the inexact numbers to exact before comparing. The `eqv?` \(and
therefore `equal?`\) procedure, in contrast, compares numbers
considering both exactness and numerical equality.

Examples:

```racket
> (= 1 1.0)   
#t            
> (eqv? 1 1.0)
#f            
```

Beware of comparisons involving inexact numbers, which by their nature
can have surprising behavior. Even apparently simple inexact numbers may
not mean what you think they mean; for example, while a base-2 IEEE
floating-point number can represent `1/2` exactly, it can only
approximate `1/10`:

Examples:

```racket
> (= 1/2 0.5)                     
#t                                
> (= 1/10 0.1)                    
#f                                
> (inexact->exact 0.1)            
3602879701896397/36028797018963968
```

> +\[missing\] in \[missing\] provides more on numbers and number
> procedures.
