# Hyphenate

Matthew Butterick <[mb@mbtype.com](mailto:mb@mbtype.com)>

```racket
 (require hyphenate)               package: [hyphenate](https://pkgs.racket-lang.org/package/hyphenate)
 (require (submod hyphenate safe))
```

A simple hyphenation engine that uses the Knuth–Liang hyphenation
algorithm originally developed for TeX. I have added little to their
work. Accordingly, I take little credit.

## 1. Installation

At the command line:

`raco pkg install hyphenate`

After that, you can update the package like so:

`raco pkg update hyphenate`

## 2. Importing the module

The module can be invoked two ways: fast or safe.

Fast mode is the default, which you get by importing the module in the
usual way: `(require` `hyphenate)`.

Safe mode enables the function contracts documented below. Use safe mode
by importing the module as `(require` `(submod` `hyphenate` `safe))`.

## 3. Interface

```racket
(hyphenate  xexpr                                        
           [joiner                                       
            #:exceptions exceptions                      
            #:min-length length                          
            #:min-left-length left-length                
            #:min-right-length right-length              
            #:omit-word word-test                        
            #:omit-string string-test                    
            #:omit-txexpr txexpr-test])     -> xexpr/c   
  xexpr : xexpr/c                                        
  joiner : (or/c char? string?) = (integer->char 173)    
  exceptions : (listof string?) = empty                  
  length : (or/c integer? false?) = 5                    
  left-length : (or/c (and/c integer? positive?) #f) = 2 
  right-length : (or/c (and/c integer? positive?) #f) = 2
  word-test : (string? . -> . any/c) = (λ(x) #f)         
  string-test : (string? . -> . any/c) = (λ(x) #f)       
  txexpr-test : (txexpr? . -> . any/c) = (λ(x) #f)       
```

Hyphenate `xexpr` by calculating hyphenation points and inserting
`joiner` at those points. By default, `joiner` is the soft hyphen
\(Unicode 00AD = decimal 173\). Words shorter than
`#:min-length` `length` will not be hyphenated. To hyphenate words of
any length, use `#:min-length` `#f`.

> The REPL displays a soft hyphen as `\u00AD`. But in ordinary use, you’ll
> only see a soft hyphen when it appears at the end of a line or page as
> part of a hyphenated word. Otherwise it’s not displayed. In most of the
> examples here, I use a standard hyphen for clarity \(by adding `#\-` as
> an argument\).

Examples:

```racket
> (hyphenate "ergo polymorphism")                
"ergo poly\u00ADmor\u00ADphism"                  
> (hyphenate "ergo polymorphism" #\-)            
"ergo poly-mor-phism"                            
> (hyphenate "ergo polymorphism" #:min-length 13)
"ergo polymorphism"                              
> (hyphenate "ergo polymorphism" #:min-length #f)
"ergo poly\u00ADmor\u00ADphism"                  
```

The `#:min-left-length` and `#:min-right-length` keyword arguments set
the minimum distance between a potential hyphen and the left or right
ends of the word. The default is 2 characters. Larger values will reduce
hyphens, but also prevent small words from breaking. These values will
override a smaller `#:min-length` value.

Examples:

```racket
> (hyphenate "ergo polymorphism" #\-)                                   
"ergo poly-mor-phism"                                                   
> (hyphenate "ergo polymorphism" #\- #:min-left-length #f)              
"ergo poly-mor-phism"                                                   
> (hyphenate "ergo polymorphism" #\- #:min-length 2 #:min-left-length 5)
"ergo polymor-phism"                                                    
> (hyphenate "ergo polymorphism" #\- #:min-right-length 6)              
"ergo poly-morphism"                                                    
; Next words won't be hyphenated becase of large #:min-left-length      
> (hyphenate "ergo                                                      
polymorphism" #\- #:min-length #f #:min-left-length 15)                 
"ergo polymorphism"                                                     
```

Because the hyphenation is based on an algorithm rather than a
dictionary, it makes good guesses with unusual words:

Examples:

```racket
> (hyphenate "scraunched strengths" #\-)              
"scraunched strengths"                                
> (hyphenate "RacketCon" #\-)                         
"Rack-et-Con"                                         
> (hyphenate "supercalifragilisticexpialidocious" #\-)
"su-per-cal-ifrag-ilis-tic-ex-pi-ali-do-cious"        
```

Using the `#:exceptions` keyword, you can pass hyphenation exceptions as
a list of words with hyphenation points marked with regular hyphens
\(`"-"`\). If an exception word contains no hyphens, that word will
never be hyphenated.

Examples:

```racket
> (hyphenate "polymorphism" #\-)                                
"poly-mor-phism"                                                
> (hyphenate "polymorphism" #\- #:exceptions '("polymo-rphism"))
"polymo-rphism"                                                 
> (hyphenate "polymorphism" #\- #:exceptions '("polymorphism")) 
"polymorphism"                                                  
```

Knuth & Liang were sufficiently confident about their algorithm that
they originally released it with only 14 exceptions: _associate\[s\],
declination, obligatory, philanthropic, present\[s\], project\[s\],
reciprocity, recognizance, reformation, retribution_, and _table_.
Admirable bravado, but it’s not hard to discover others that need
adjustment.

Examples:

```racket
> (hyphenate "wrong: columns signage lawyers" #\-) 
"wrong: columns sig-nage law-yers"                 
> (hyphenate "right: columns signage lawyers" #\-  
  #:exceptions '("col-umns" "sign-age" "law-yers"))
"right: col-umns sign-age law-yers"                
```

The Knuth–Liang algorithm is designed to omit legitimate hyphenation
points \(i.e., generate false negatives\) more often than it creates
erroneous hyphenation points \(i.e., false positives\). This is good
policy. Perfect hyphenation — that is, hyphenation that represents an
exact linguistic syllabification of each word — is superfluous for
typesetting. Hyphenation simply seeks to mark possible line-break and
page-break locations for whatever layout engine is drawing the text. The
ultimate goal is to permit more even text flow. Like horseshoes and hand
grenades, close is good enough. And a word wrongly hyphenated is more
likely to be noticed by a reader than a word inefficiently hyphenated.

For this reason, certain words can’t be hyphenated algorithmically,
because the correct hyphenation depends on meaning, not merely on
spelling. For instance:

Example:

```racket
> (hyphenate "adder")
"adder"              
```

This is the right result. If you used _adder_ to mean the machine, it
would be hyphenated _add-er_; if you meant the snake, it would be
_ad-der_. Better to avoid hyphenation than to hyphenate incorrectly.

You can send HTML-style X-expressions through `hyphenate`. It will
recursively hyphenate the text strings, while leaving the tags and
attributes alone, as well as non-hyphenatable material \(like character
entities and CDATA\).

Examples:

```racket
> (hyphenate '(p "strangely" (em "formatted" (strong "snowmen"))) #\-)
'(p "strange-ly" (em "for-mat-ted" (strong "snow-men")))              
> (hyphenate '(headline [[class "headline"]] "headline") #\-)         
'(headline ((class "headline")) "head-line")                          
> (hyphenate '(div "The (span epsilon) entity:" epsilon) #\-)         
'(div "The (span ep-silon) en-ti-ty:" epsilon)                        
```

Don’t send raw HTML or XML through `hyphenate`. It can’t distinguish
tags and attributes from textual content, so everything will be
hyphenated, thus goofing up your file. But you can easily convert your
HTML or XML to an X-expression, hyphenate it, and then convert back.

Examples:

```racket
> (define html "<body style=\"background: yellow\">Hello</body>")
> (hyphenate html #\-)                                           
"<body style=\"back-ground: yel-low\">Hel-lo</body>"             
> (xexpr->string (hyphenate (string->xexpr html) #\-))           
"<body style=\"background: yellow\">Hel-lo</body>"               
```

If you’re working with HTML, be careful not to include any `<script>` or
`<style>` blocks, which contain non-hyphenatable data. You can protect
that data by using the `#:omit-txexpr` keyword to specify a
`txexpr-test`. The test will be applied to all tagged X-expressions
\(see `txexpr?`\). When `txexpr-test` evaluates to true, the item will
be skipped.

Examples:

```racket
> (hyphenate '(body "processing" (script "no processing")) #\-)
'(body "pro-cess-ing" (script "no pro-cess-ing"))              
> (hyphenate '(body "processing" (script "no processing")) #\- 
  #:omit-txexpr (λ(tx) (member (get-tag tx) '(script))))       
'(body "pro-cess-ing" (script "no processing"))                
```

You can also use `#:omit-txexpr` to omit tagged X-expressions with
particular attributes. This can be used to selectively suppress
hyphenation at the markup level.

Examples:

```racket
>                                                                        
(hyphenate '(p (span "processing") (span [[klh "no"]] "processing")) #\-)
'(p (span "pro-cess-ing") (span ((klh "no")) "pro-cess-ing"))            
>                                                                        
(hyphenate '(p (span "processing") (span [[klh "no"]] "processing")) #\- 
  #:omit-txexpr (λ(tx) (and (attrs-have-key? tx 'klh)                    
  (equal? (attr-ref tx 'klh) "no"))))                                    
'(p (span "pro-cess-ing") (span ((klh "no")) "processing"))              
```

Similarly, you can use the `#:omit-word` argument to avoid words that
match `word-test`. Convenient if you want to prevent hyphenation of
certain sets of words, like proper names:

Examples:

```racket
> (hyphenate "Brennan Huff likes fancy sauce" #\-)                  
"Bren-nan Huff likes fan-cy sauce"                                  
> (define capitalized? (λ(word) (let ([letter (substring word 0 1)])
  (equal? letter (string-upcase letter)))))                         
> (hyphenate "Brennan Huff likes fancy                              
sauce" #\- #:omit-word capitalized?)                                
"Brennan Huff likes fan-cy sauce"                                   
```

Sometimes you need `#:omit-word` to prevent unintended consequences. For
instance, if you’re using ligatures in CSS, certain groups of characters
\(fi, fl, ffi, et al.\) will be replaced by a single glyph. That looks
snazzy, but adding soft hyphens between any of these pairs will defeat
the ligature substitution, creating inconsistent results. With
`#:omit-word`, you can skip these words:

> “Wouldn’t it be better to exclude certain pairs of letters rather than
> whole words?” Yes. But for now, that’s not supported.

Examples:

```racket
> (hyphenate "Hufflepuff golfing final on Tuesday" #\-)
"Huf-flepuff golf-ing fi-nal on Tues-day"              
> (define (ligs? word)                                 
    (ormap (λ(lig) (regexp-match lig word))            
    '("ff" "fi" "fl" "ffi" "ffl")))                    
> (hyphenate "Hufflepuff golfing final on              
Tuesday" #\- #:omit-word ligs?)                        
"Hufflepuff golfing final on Tues-day"                 
```

```racket
(unhyphenate  xexpr                                  
             [joiner                                 
              #:omit-word word-test                  
              #:omit-string string-test              
              #:omit-txexpr txexpr-test]) -> xexpr/c 
  xexpr : xexpr/c                                    
  joiner : (or/c char? string?) = (integer->char 173)
  word-test : (string? . -> . any/c) = (λ(x) #f)     
  string-test : (string? . -> . any/c) = (λ(x) #f)   
  txexpr-test : (txexpr? . -> . any/c) = (λ(x) #f)   
```

Remove `joiner` from `xexpr`. Like `hyphenate`, it works on nested
X-expressions, and offers the same `#:omit-` options.

Examples:

```racket
> (hyphenate '(p "strangely" (em "formatted" (strong "snowmen"))) #\-)    
'(p "strange-ly" (em "for-mat-ted" (strong "snow-men")))                  
>                                                                         
(unhyphenate '(p "strange-ly" (em "for-mat-ted" (strong "snow-men"))) #\-)
'(p "strangely" (em "formatted" (strong "snowmen")))                      
```

A side effect of using `hyphenate` is that soft hyphens \(or whatever
the `joiner` is\) will be embedded in the output text. If you need to
support copying of text, for instance in a GUI application, you’ll
probably want to strip out the hyphenation before the copied text is
moved to the clipboard.

Examples:

```racket
> (hyphenate "ribbon-cutting ceremony")                
"rib\u00ADbon-cut\u00ADting cer\u00ADe\u00ADmo\u00ADny"
> (unhyphenate (hyphenate "ribbon-cutting ceremony"))  
"ribbon-cutting ceremony"                              
```

Use this function cautiously — if `joiner` appeared in the original
input to `hyphenate`, the output from `unhyphenate` won’t be the same
string.

Examples:

```racket
> (hyphenate "ribbon-cutting ceremony" #\-)                  
"rib-bon-cut-ting cer-e-mo-ny"                               
> (unhyphenate (hyphenate "ribbon-cutting ceremony" #\-) #\-)
"ribboncutting ceremony"                                     
```

Keep in mind that soft hyphens could appear in your input string.
Certain word processors allow users to [insert soft
hyphens](http://practicaltypography.com/optional-hyphens.html) in their
text.

Examples:

```racket
> (hyphenate "True\u00ADType typefaces")                  
"True\u00ADType type\u00ADfaces"                          
> (unhyphenate (hyphenate "True\u00ADType typefaces"))    
"TrueType typefaces"                                      
> (hyphenate (unhyphenate "True\u00ADType typefaces") #\-)
"True-Type type-faces"                                    
```

## 4. French

```racket
 (require hyphenate/fr)               package: [hyphenate](https://pkgs.racket-lang.org/package/hyphenate)
 (require (submod hyphenate/fr safe))
```

French hyphenation is available by importing the module as
`hyphenate/fr` or `(submod hyphenate/fr safe)` and using the `hyphenate`
function normally. Below, notice that the word “formidable” hyphenates
differently in French.

Examples:

```racket
> (hyphenate "formidable" #\-)   
"for-mi-da-ble"                  
> (module fr racket/base         
    (require hyphenate/fr)       
    (hyphenate "formidable" #\-))
> (require 'fr)                  
"for-mi-dable"                   
```

The two languages are in separate submodules for performance reasons.
That way, they can maintain separate caches of hyphenated words.

There is no way to use `hyphenate` in “polyglot” mode, where English and
French are detected automatically. It is possible, however, to mix both
the English and French `hyphenate` functions in a single file, and apply
them as needed. To avoid a name conflict between the two `hyphenate`
functions, you’ll need to use `prefix-in`:

Examples:

```racket
> (require (prefix-in fr: hyphenate/fr))
> (hyphenate "formidable" #\-)          
"for-mi-da-ble"                         
> (fr:hyphenate "formidable" #\-)       
"for-mi-dable"                          
```

## 5. Russian

```racket
 (require hyphenate/ru)               package: [hyphenate](https://pkgs.racket-lang.org/package/hyphenate)
 (require (submod hyphenate/ru safe))
```

Russian hyphenation is available by importing the module as
`hyphenate/ru` or `(submod hyphenate/ru safe)` and using the `hyphenate`
function normally. \(Hat tip to Natanael de Kross for finding the
patterns, originally created by Alexander I. Lebedev.\)

## 6. License & source code

This module is licensed under the LGPL.

Source repository at
[http://github.com/mbutterick/hyphenate](http://github.com/mbutterick/hyphenate).
Suggestions & corrections welcome.
