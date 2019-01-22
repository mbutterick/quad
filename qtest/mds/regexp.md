# Regular Expressions

> This chapter is a modified version of \[Sitaram05\].

A _regexp_ value encapsulates a pattern that is described by a string or
byte string.  The regexp matcher tries to match this pattern against \(a
portion of\) another string or byte string, which we will call the _text
string_, when you call functions like `regexp-match`.  The text string
is treated as raw text, and not as a pattern.

    1 Writing Regexp Patterns                   
                                                
    2 Matching Regexp Patterns                  
                                                
    3 Basic Assertions                          
                                                
    4 Characters and Character Classes          
      4.1 Some Frequently Used Character Classes
      4.2 POSIX character classes               
                                                
    5 Quantifiers                               
                                                
    6 Clusters                                  
      6.1 Backreferences                        
      6.2 Non-capturing Clusters                
      6.3 Cloisters                             
                                                
    7 Alternation                               
                                                
    8 Backtracking                              
                                                
    9 Looking Ahead and Behind                  
      9.1 Lookahead                             
      9.2 Lookbehind                            
                                                
    10 An Extended Example                      

> +\[missing\] in \[missing\] provides more on regexps.

## 1. Writing Regexp Patterns

A string or byte string can be used directly as a regexp pattern, or it
can be prefixed with `#rx` to form a literal regexp value. For example,
`#rx"abc"` is a string-based regexp value, and `#rx#"abc"` is a byte
string-based regexp value. Alternately, a string or byte string can be
prefixed with `#px`, as in `#px"abc"`, for a slightly extended syntax of
patterns within the string.

Most of the characters in a regexp pattern are meant to match
occurrences of themselves in the text string.  Thus, the pattern
`#rx"abc"` matches a string that contains the characters `a`, `b`, and
`c` in succession. Other characters act as _metacharacters_, and some
character sequences act as _metasequences_.  That is, they specify
something other than their literal selves.  For example, in the pattern
`#rx"a.c"`, the characters `a` and `c` stand for themselves, but the
metacharacter `.` can match _any_ character.  Therefore, the pattern
`#rx"a.c"` matches an `a`, any character, and `c` in succession.

> When we want a literal `\` inside a Racket string or regexp literal, we
> must escape it so that it shows up in the string at all. Racket strings
> use `\` as the escape character, so we end up with two `\`s: one
> Racket-string `\` to escape the regexp `\`, which then escapes the `.`.
> Another character that would need escaping inside a Racket string is
> `"`.

If we needed to match the character `.` itself, we can escape it by
precede it with a `\`.  The character sequence `\.` is thus a
metasequence, since it doesn’t match itself but rather just `.`.  So, to
match `a`, `.`, and `c` in succession, we use the regexp pattern
`#rx"a\\.c"`; the double `\` is an artifact of Racket strings, not the
regexp pattern itself.

The `regexp` function takes a string or byte string and produces a
regexp value. Use `regexp` when you construct a pattern to be matched
against multiple strings, since a pattern is compiled to a regexp value
before it can be used in a match. The `pregexp` function is like
`regexp`, but using the extended syntax. Regexp values as literals with
`#rx` or `#px` are compiled once and for all when they are read.

The `regexp-quote` function takes an arbitrary string and returns a
string for a pattern that matches exactly the original string. In
particular, characters in the input string that could serve as regexp
metacharacters are escaped with a backslash, so that they safely match
only themselves.

```racket
> (regexp-quote "cons") 
"cons"                  
> (regexp-quote "list?")
"list\\?"               
```

The `regexp-quote` function is useful when building a composite regexp
from a mix of regexp strings and verbatim strings.

## 2. Matching Regexp Patterns

The `regexp-match-positions` function takes a regexp pattern and a text
string, and it returns a match if the regexp matches \(some part of\)
the text string, or `#f` if the regexp did not match the string. A
successful match produces a list of _index pairs_.

Examples:

```racket
> (regexp-match-positions #rx"brain" "bird")             
#f                                                       
> (regexp-match-positions #rx"needle" "hay needle stack")
'((4 . 10))                                              
```

In the second example, the integers `4` and `10` identify the substring
that was matched. The `4` is the starting \(inclusive\) index, and `10`
the ending \(exclusive\) index of the matching substring:

```racket
> (substring "hay needle stack" 4 10)
"needle"                             
```

In this first example, `regexp-match-positions`’s return list contains
only one index pair, and that pair represents the entire substring
matched by the regexp.  When we discuss subpatterns later, we will see
how a single match operation can yield a list of submatches.

The `regexp-match-positions` function takes optional third and fourth
arguments that specify the indices of the text string within which the
matching should take place.

```racket
> (regexp-match-positions                                   
   #rx"needle"                                              
   "his needle stack -- my needle stack -- her needle stack"
   20 39)                                                   
'((23 . 29))                                                
```

Note that the returned indices are still reckoned relative to the full
text string.

The `regexp-match` function is like `regexp-match-positions`, but
instead of returning index pairs, it returns the matching substrings:

```racket
> (regexp-match #rx"brain" "bird")             
#f                                             
> (regexp-match #rx"needle" "hay needle stack")
'("needle")                                    
```

When `regexp-match` is used with byte-string regexp, the result is a
matching byte substring:

```racket
> (regexp-match #rx#"needle" #"hay needle stack")
'(#"needle")                                     
```

> A byte-string regexp can be applied to a string, and a string regexp can
> be applied to a byte string. In both cases, the result is a byte string.
> Internally, all regexp matching is in terms of bytes, and a string
> regexp is expanded to a regexp that matches UTF-8 encodings of
> characters. For maximum efficiency, use byte-string matching instead of
> string, since matching bytes directly avoids UTF-8 encodings.

If you have data that is in a port, there’s no need to first read it
into a string. Functions like `regexp-match` can match on the port
directly:

```racket
> (define-values (i o) (make-pipe))
> (write "hay needle stack" o)     
> (close-output-port o)            
> (regexp-match #rx#"needle" i)    
'(#"needle")                       
```

The `regexp-match?` function is like `regexp-match-positions`, but
simply returns a boolean indicating whether the match succeeded:

```racket
> (regexp-match? #rx"brain" "bird")             
#f                                              
> (regexp-match? #rx"needle" "hay needle stack")
#t                                              
```

The `regexp-split` function takes two arguments, a regexp pattern and a
text string, and it returns a list of substrings of the text string; the
pattern identifies the delimiter separating the substrings.

```racket
> (regexp-split #rx":" "/bin:/usr/bin:/usr/bin/X11:/usr/local/bin")
'("/bin" "/usr/bin" "/usr/bin/X11" "/usr/local/bin")               
> (regexp-split #rx" " "pea soup")                                 
'("pea" "soup")                                                    
```

If the first argument matches empty strings, then the list of all the
single-character substrings is returned.

```racket
> (regexp-split #rx"" "smithereens")                
'("" "s" "m" "i" "t" "h" "e" "r" "e" "e" "n" "s" "")
```

Thus, to identify one-or-more spaces as the delimiter, take care to use
the regexp `#rx" +"`, not `#rx" *"`.

```racket
> (regexp-split #rx" +" "split pea     soup")                 
'("split" "pea" "soup")                                       
> (regexp-split #rx" *" "split pea     soup")                 
'("" "s" "p" "l" "i" "t" "" "p" "e" "a" "" "s" "o" "u" "p" "")
```

The `regexp-replace` function replaces the matched portion of the text
string by another string.  The first argument is the pattern, the second
the text string, and the third is either the string to be inserted or a
procedure to convert matches to the insert string.

```racket
> (regexp-replace #rx"te" "liberte" "ty")       
"liberty"                                       
> (regexp-replace #rx"." "racket" string-upcase)
"Racket"                                        
```

If the pattern doesn’t occur in the text string, the returned string is
identical to the text string.

The `regexp-replace*` function replaces _all_ matches in the text string
by the insert string:

```racket
> (regexp-replace* #rx"te" "liberte egalite fraternite" "ty")
"liberty egality fratyrnity"                                 
> (regexp-replace* #rx"[ds]" "drracket" string-upcase)       
"Drracket"                                                   
```

## 3. Basic Assertions

The _assertions_ `^` and `$` identify the beginning and the end of the
text string, respectively.  They ensure that their adjoining regexps
match at one or other end of the text string:

```racket
> (regexp-match-positions #rx"^contact" "first contact")
#f                                                      
```

The regexp above fails to match because `contact` does not occur at the
beginning of the text string. In

```racket
> (regexp-match-positions #rx"laugh$" "laugh laugh laugh laugh")
'((18 . 23))                                                    
```

the regexp matches the _last_ `laugh`.

The metasequence `\b` asserts that a word boundary exists, but this
metasequence works only with `#px` syntax. In

```racket
> (regexp-match-positions #px"yack\\b" "yackety yack")
'((8 . 12))                                           
```

the `yack` in `yackety` doesn’t end at a word boundary so it isn’t
matched.  The second `yack` does and is.

The metasequence `\B` \(also `#px` only\) has the opposite effect to
`\b`; it asserts that a word boundary does not exist. In

```racket
> (regexp-match-positions #px"an\\B" "an analysis")
'((3 . 5))                                         
```

the `an` that doesn’t end in a word boundary is matched.

## 4. Characters and Character Classes

Typically, a character in the regexp matches the same character in the
text string.  Sometimes it is necessary or convenient to use a regexp
metasequence to refer to a single character. For example, the
metasequence `\.` matches the period character.

The metacharacter `.` matches _any_ character \(other than newline in
multi-line mode; see Cloisters\):

```racket
> (regexp-match #rx"p.t" "pet")
'("pet")                       
```

The above pattern also matches `pat`, `pit`, `pot`, `put`, and `p8t`,
but not `peat` or `pfffft`.

A _character class_ matches any one character from a set of characters.
A typical format for this is the _bracketed character class_ `[`...`]`,
which matches any one character from the non-empty sequence of
characters enclosed within the brackets.  Thus, `#rx"p[aeiou]t"` matches
`pat`, `pet`, `pit`, `pot`, `put`, and nothing else.

Inside the brackets, a `-` between two characters specifies the Unicode
range between the characters.  For example, `#rx"ta[b-dgn-p]"` matches
`tab`, `tac`, `tad`, `tag`, `tan`, `tao`, and `tap`.

An initial `^` after the left bracket inverts the set specified by the
rest of the contents; i.e., it specifies the set of characters _other
than_ those identified in the brackets. For example, `#rx"do[^g]"`
matches all three-character sequences starting with `do` except `dog`.

Note that the metacharacter `^` inside brackets means something quite
different from what it means outside.  Most other metacharacters \(`.`,
`*`, `+`, `?`, etc.\) cease to be metacharacters when inside brackets,
although you may still escape them for peace of mind. A `-` is a
metacharacter only when it’s inside brackets, and when it is neither the
first nor the last character between the brackets.

Bracketed character classes cannot contain other bracketed character
classes \(although they contain certain other types of character
classes; see below\).  Thus, a `[` inside a bracketed character class
doesn’t have to be a metacharacter; it can stand for itself. For
example, `#rx"[a[b]"` matches `a`, `[`, and `b`.

Furthermore, since empty bracketed character classes are disallowed, a
`]` immediately occurring after the opening left bracket also doesn’t
need to be a metacharacter.  For example, `#rx"[]ab]"` matches `]`, `a`,
and `b`.

### 4.1. Some Frequently Used Character Classes

In `#px` syntax, some standard character classes can be conveniently
represented as metasequences instead of as explicit bracketed
expressions:  `\d` matches a digit \(the same as `[0-9]`\); `\s` matches
an ASCII whitespace character; and `\w` matches a character that could
be part of a “word”.

> Following regexp custom, we identify “word” characters as
> `[A-Za-z0-9_]`, although these are too restrictive for what a Racketeer
> might consider a “word.”

The upper-case versions of these metasequences stand for the inversions
of the corresponding character classes: `\D` matches a non-digit, `\S` a
non-whitespace character, and `\W` a non-“word” character.

Remember to include a double backslash when putting these metasequences
in a Racket string:

```racket
> (regexp-match #px"\\d\\d"                  
   "0 dear, 1 have 2 read catch 22 before 9")
'("22")                                      
```

These character classes can be used inside a bracketed expression. For
example, `#px"[a-z\\d]"` matches a lower-case letter or a digit.

### 4.2. POSIX character classes

A _POSIX character class_ is a special metasequence of the form
`[:`...`:]` that can be used only inside a bracketed expression in `#px`
syntax.  The POSIX classes supported are

* `[:alnum:]` — ASCII letters and digits

* `[:alpha:]` — ASCII letters

* `[:ascii:]` — ASCII characters

* `[:blank:]` — ASCII widthful whitespace: space and tab

* `[:cntrl:]` — “control” characters: ASCII 0 to 32

* `[:digit:]` — ASCII digits, same as `\d`

* `[:graph:]` — ASCII characters that use ink

* `[:lower:]` — ASCII lower-case letters

* `[:print:]` — ASCII ink-users plus widthful whitespace

* `[:space:]` — ASCII whitespace, same as `\s`

* `[:upper:]` — ASCII upper-case letters

* `[:word:]` — ASCII letters and `_`, same as `\w`

* `[:xdigit:]` — ASCII hex digits

For example, the `#px"[[:alpha:]_]"` matches a letter or underscore.

```racket
> (regexp-match #px"[[:alpha:]_]" "--x--")
'("x")                                    
> (regexp-match #px"[[:alpha:]_]" "--_--")
'("_")                                    
> (regexp-match #px"[[:alpha:]_]" "--:--")
#f                                        
```

The POSIX class notation is valid _only_ inside a bracketed expression.
For instance, `[:alpha:]`, when not inside a bracketed expression, will
not be read as the letter class.  Rather, it is \(from previous
principles\) the character class containing the characters `:`, `a`,
`l`, `p`, `h`.

```racket
> (regexp-match #px"[:alpha:]" "--a--")
'("a")                                 
> (regexp-match #px"[:alpha:]" "--x--")
#f                                     
```

## 5. Quantifiers

The _quantifiers_ `*`, `+`, and `?` match respectively: zero or more,
one or more, and zero or one instances of the preceding subpattern.

```racket
> (regexp-match-positions #rx"c[ad]*r" "cadaddadddr")
'((0 . 11))                                          
> (regexp-match-positions #rx"c[ad]*r" "cr")         
'((0 . 2))                                           
> (regexp-match-positions #rx"c[ad]+r" "cadaddadddr")
'((0 . 11))                                          
> (regexp-match-positions #rx"c[ad]+r" "cr")         
#f                                                   
> (regexp-match-positions #rx"c[ad]?r" "cadaddadddr")
#f                                                   
> (regexp-match-positions #rx"c[ad]?r" "cr")         
'((0 . 2))                                           
> (regexp-match-positions #rx"c[ad]?r" "car")        
'((0 . 3))                                           
```

In `#px` syntax, you can use braces to specify much finer-tuned
quantification than is possible with `*`, `+`, `?`:

* The quantifier `{`_m_`}` matches _exactly_ _m_ instances of the
  preceding subpattern; _m_ must be a nonnegative integer.

* The quantifier `{`_m_`,`_n_`}` matches at least _m_ and at most _n_
  instances.  `m` and `n` are nonnegative integers with _m_ less or
  equal to _n_.  You may omit either or both numbers, in which case _m_
  defaults to __0__ and _n_ to infinity.

It is evident that `+` and `?` are abbreviations for `{1,}` and `{0,1}`
respectively, and `*` abbreviates `{,}`, which is the same as `{0,}`.

```racket
> (regexp-match #px"[aeiou]{3}" "vacuous") 
'("uou")                                   
> (regexp-match #px"[aeiou]{3}" "evolve")  
#f                                         
> (regexp-match #px"[aeiou]{2,3}" "evolve")
#f                                         
> (regexp-match #px"[aeiou]{2,3}" "zeugma")
'("eu")                                    
```

The quantifiers described so far are all _greedy_: they match the
maximal number of instances that would still lead to an overall match
for the full pattern.

```racket
> (regexp-match #rx"<.*>" "<tag1> <tag2> <tag3>")
'("<tag1> <tag2> <tag3>")                        
```

To make these quantifiers _non-greedy_, append a `?` to them.
Non-greedy quantifiers match the minimal number of instances needed to
ensure an overall match.

```racket
> (regexp-match #rx"<.*?>" "<tag1> <tag2> <tag3>")
'("<tag1>")                                       
```

The non-greedy quantifiers are `*?`, `+?`, `??`, `{`_m_`}?`, and
`{`_m_`,`_n_`}?`, although `{`_m_`}?` is always the same as `{`_m_`}`.
Note that the metacharacter `?` has two different uses, and both uses
are represented in `??`.

## 6. Clusters

_Clustering_—enclosure within parens `(`...`)`—identifies the enclosed
_subpattern_ as a single entity.  It causes the matcher to capture the
_submatch_, or the portion of the string matching the subpattern, in
addition to the overall match:

```racket
> (regexp-match #rx"([a-z]+) ([0-9]+), ([0-9]+)" "jan 1, 1970")
'("jan 1, 1970" "jan" "1" "1970")                              
```

Clustering also causes a following quantifier to treat the entire
enclosed subpattern as an entity:

```racket
> (regexp-match #rx"(pu )*" "pu pu platter")
'("pu pu " "pu ")                           
```

The number of submatches returned is always equal to the number of
subpatterns specified in the regexp, even if a particular subpattern
happens to match more than one substring or no substring at all.

```racket
> (regexp-match #rx"([a-z ]+;)*" "lather; rinse; repeat;")
'("lather; rinse; repeat;" " repeat;")                    
```

Here, the `*`-quantified subpattern matches three times, but it is the
last submatch that is returned.

It is also possible for a quantified subpattern to fail to match, even
if the overall pattern matches.  In such cases, the failing submatch is
represented by `#f`

```racket
> (define date-re                             
    ; match ‘month year' or ‘month day, year';
    ; subpattern matches day, if present      
    #rx"([a-z]+) +([0-9]+,)? *([0-9]+)")      
> (regexp-match date-re "jan 1, 1970")        
'("jan 1, 1970" "jan" "1," "1970")            
> (regexp-match date-re "jan 1970")           
'("jan 1970" "jan" #f "1970")                 
```

### 6.1. Backreferences

Submatches can be used in the insert string argument of the procedures
`regexp-replace` and `regexp-replace*`.  The insert string can use
`\`_n_ as a _backreference_ to refer back to the _n_th submatch, which
is the substring that matched the _n_th subpattern.  A `\0` refers to
the entire match, and it can also be specified as `\&`.

```racket
> (regexp-replace #rx"_(.+?)_"                      
    "the _nina_, the _pinta_, and the _santa maria_"
    "*\\1*")                                        
"the *nina*, the _pinta_, and the _santa maria_"    
> (regexp-replace* #rx"_(.+?)_"                     
    "the _nina_, the _pinta_, and the _santa maria_"
    "*\\1*")                                        
"the *nina*, the *pinta*, and the *santa maria*"    
> (regexp-replace #px"(\\S+) (\\S+) (\\S+)"         
    "eat to live"                                   
    "\\3 \\2 \\1")                                  
"live to eat"                                       
```

Use `\\` in the insert string to specify a literal backslash. Also, `\$`
stands for an empty string, and is useful for separating a backreference
`\`_n_ from an immediately following number.

Backreferences can also be used within a `#px` pattern to refer back to
an already matched subpattern in the pattern. `\`_n_ stands for an exact
repeat of the _n_th submatch. Note that `\0`, which is useful in an
insert string, makes no sense within the regexp pattern, because the
entire regexp has not matched yet so you cannot refer back to it.}

```racket
> (regexp-match #px"([a-z]+) and \\1"   
                "billions and billions")
'("billions and billions" "billions")   
```

Note that the backreference is not simply a repeat of the previous
subpattern.  Rather it is a repeat of the particular substring already
matched by the subpattern.

In the above example, the backreference can only match `billions`.  It
will not match `millions`, even though the subpattern it harks back
to—`([a-z]+)`—would have had no problem doing so:

```racket
> (regexp-match #px"([a-z]+) and \\1"   
                "billions and millions")
#f                                      
```

The following example marks all immediately repeating patterns in a
number string:

```racket
> (regexp-replace* #px"(\\d+)\\1"     
    "123340983242432420980980234"     
    "{\\1,\\1}")                      
"12{3,3}40983{24,24}3242{098,098}0234"
```

The following example corrects doubled words:

```racket
> (regexp-replace* #px"\\b(\\S+) \\1\\b"                          
    (string-append "now is the the time for all good men to "     
                   "to come to the aid of of the party")          
    "\\1")                                                        
"now is the time for all good men to come to the aid of the party"
```

### 6.2. Non-capturing Clusters

It is often required to specify a cluster \(typically for
quantification\) but without triggering the capture of submatch
information.  Such clusters are called _non-capturing_.  To create a
non-capturing cluster, use `(?:` instead of `(` as the cluster opener.

In the following example, a non-capturing cluster eliminates the
“directory” portion of a given Unix pathname, and a capturing cluster
identifies the basename.

> But don’t parse paths with regexps. Use functions like `split-path`,
> instead.

```racket
> (regexp-match #rx"^(?:[a-z]*/)*([a-z]+)$"
                "/usr/local/bin/racket")   
'("/usr/local/bin/racket" "racket")        
```

### 6.3. Cloisters

The location between the `?` and the `:` of a non-capturing cluster is
called a _cloister_. You can put modifiers there that will cause the
enclustered subpattern to be treated specially.  The modifier `i` causes
the subpattern to match case-insensitively:

> The term _cloister_ is a useful, if terminally cute, coinage from the
> abbots of Perl.

```racket
> (regexp-match #rx"(?i:hearth)" "HeartH")
'("HeartH")                               
```

The modifier `m` causes the subpattern to match in _multi-line mode_,
where `.` does not match a newline character, `^` can match just after a
newline, and `$` can match just before a newline.

```racket
> (regexp-match #rx"." "\na\n")                             
'("\n")                                                     
> (regexp-match #rx"(?m:.)" "\na\n")                        
'("a")                                                      
> (regexp-match #rx"^A plan$" "A man\nA plan\nA canal")     
#f                                                          
> (regexp-match #rx"(?m:^A plan$)" "A man\nA plan\nA canal")
'("A plan")                                                 
```

You can put more than one modifier in the cloister:

```racket
> (regexp-match #rx"(?mi:^A Plan$)" "a man\na plan\na canal")
'("a plan")                                                  
```

A minus sign before a modifier inverts its meaning.  Thus, you can use
`-i` in a _subcluster_ to overturn the case-insensitivities caused by an
enclosing cluster.

```racket
> (regexp-match #rx"(?i:the (?-i:TeX)book)"
                "The TeXbook")             
'("The TeXbook")                           
```

The above regexp will allow any casing for `the` and `book`, but it
insists that `TeX` not be differently cased.

## 7. Alternation

You can specify a list of _alternate_ subpatterns by separating them by
`|`.  The `|` separates subpatterns in the nearest enclosing cluster
\(or in the entire pattern string if there are no enclosing parens\).

```racket
> (regexp-match #rx"f(ee|i|o|um)" "a small, final fee")          
'("fi" "i")                                                      
> (regexp-replace* #rx"([yi])s(e[sdr]?|ing|ation)"               
                   (string-append                                
                    "analyse an energising organisation"         
                    " pulsing with noisy organisms")             
                   "\\1z\\2")                                    
"analyze an energizing organization pulsing with noisy organisms"
```

Note again that if you wish to use clustering merely to specify a list
of alternate subpatterns but do not want the submatch, use `(?:` instead
of `(`.

```racket
> (regexp-match #rx"f(?:ee|i|o|um)" "fun for all")
'("fo")                                           
```

An important thing to note about alternation is that the leftmost
matching alternate is picked regardless of its length.  Thus, if one of
the alternates is a prefix of a later alternate, the latter may not have
a chance to match.

```racket
> (regexp-match #rx"call|call-with-current-continuation"
                "call-with-current-continuation")       
'("call")                                               
```

To allow the longer alternate to have a shot at matching, place it
before the shorter one:

```racket
> (regexp-match #rx"call-with-current-continuation|call"
                "call-with-current-continuation")       
'("call-with-current-continuation")                     
```

In any case, an overall match for the entire regexp is always preferred
to an overall non-match.  In the following, the longer alternate still
wins, because its preferred shorter prefix fails to yield an overall
match.

```racket
> (regexp-match                                            
   #rx"(?:call|call-with-current-continuation) constrained"
   "call-with-current-continuation constrained")           
'("call-with-current-continuation constrained")            
```

## 8. Backtracking

We’ve already seen that greedy quantifiers match the maximal number of
times, but the overriding priority is that the overall match succeed.
Consider

```racket
> (regexp-match #rx"a*a" "aaaa")
'("aaaa")                       
```

The regexp consists of two subregexps: `a*` followed by `a`.  The
subregexp `a*` cannot be allowed to match all four `a`’s in the text
string `aaaa`, even though `*` is a greedy quantifier.  It may match
only the first three, leaving the last one for the second subregexp.
This ensures that the full regexp matches successfully.

The regexp matcher accomplishes this via a process called
_backtracking_.  The matcher tentatively allows the greedy quantifier to
match all four `a`’s, but then when it becomes clear that the overall
match is in jeopardy, it _backtracks_ to a less greedy match of three
`a`’s.  If even this fails, as in the call

```racket
> (regexp-match #rx"a*aa" "aaaa")
'("aaaa")                        
```

the matcher backtracks even further.  Overall failure is conceded only
when all possible backtracking has been tried with no success.

Backtracking is not restricted to greedy quantifiers. Nongreedy
quantifiers match as few instances as possible, and progressively
backtrack to more and more instances in order to attain an overall
match.  There is backtracking in alternation too, as the more rightward
alternates are tried when locally successful leftward ones fail to yield
an overall match.

Sometimes it is efficient to disable backtracking.  For example, we may
wish to commit to a choice, or we know that trying alternatives is
fruitless.  A nonbacktracking regexp is enclosed in `(?>`...`)`.

```racket
> (regexp-match #rx"(?>a+)." "aaaa")
#f                                  
```

In this call, the subregexp `?>a+` greedily matches all four `a`’s, and
is denied the opportunity to backtrack.  So, the overall match is
denied.  The effect of the regexp is therefore to match one or more
`a`’s followed by something that is definitely non-`a`.

## 9. Looking Ahead and Behind

You can have assertions in your pattern that look _ahead_ or _behind_ to
ensure that a subpattern does or does not occur. These “look around”
assertions are specified by putting the subpattern checked for in a
cluster whose leading characters are: `?=` \(for positive lookahead\),
`?!` \(negative lookahead\), `?<=` \(positive lookbehind\), `?<!`
\(negative lookbehind\).  Note that the subpattern in the assertion does
not generate a match in the final result; it merely allows or disallows
the rest of the match.

### 9.1. Lookahead

Positive lookahead with `?=` peeks ahead to ensure that its subpattern
_could_ match.

```racket
> (regexp-match-positions #rx"grey(?=hound)"
    "i left my grey socks at the greyhound")
'((28 . 32))                                
```

The regexp `#rx"grey(?=hound)"` matches `grey`, but _only_ if it is
followed by `hound`.  Thus, the first `grey` in the text string is not
matched.

Negative lookahead with `?!` peeks ahead to ensure that its subpattern
_could not_ possibly match.

```racket
> (regexp-match-positions #rx"grey(?!hound)"
    "the gray greyhound ate the grey socks")
'((27 . 31))                                
```

The regexp `#rx"grey(?!hound)"` matches `grey`, but only if it is _not_
followed by `hound`.  Thus the `grey` just before `socks` is matched.

### 9.2. Lookbehind

Positive lookbehind with `?<=` checks that its subpattern _could_ match
immediately to the left of the current position in the text string.

```racket
> (regexp-match-positions #rx"(?<=grey)hound"     
    "the hound in the picture is not a greyhound")
'((38 . 43))                                      
```

The regexp `#rx"(?<=grey)hound"` matches `hound`, but only if it is
preceded by `grey`.

Negative lookbehind with `?<!` checks that its subpattern could not
possibly match immediately to the left.

```racket
> (regexp-match-positions #rx"(?<!grey)hound"     
    "the greyhound in the picture is not a hound")
'((38 . 43))                                      
```

The regexp `#rx"(?<!grey)hound"` matches `hound`, but only if it is
_not_ preceded by `grey`.

Lookaheads and lookbehinds can be convenient when they are not
confusing.

## 10. An Extended Example

Here’s an extended example from Friedl’s _Mastering Regular
Expressions_, page 189, that covers many of the features described in
this chapter.  The problem is to fashion a regexp that will match any
and only IP addresses or _dotted quads_: four numbers separated by three
dots, with each number between 0 and 255.

First, we define a subregexp `n0-255` that matches 0 through 255:

```racket
> (define n0-255                    
    (string-append                  
     "(?:"                          
     "\\d|"        ;   0 through   9
     "\\d\\d|"     ;  00 through  99
     "[01]\\d\\d|" ; 000 through 199
     "2[0-4]\\d|"  ; 200 through 249
     "25[0-5]"     ; 250 through 255
     ")"))                          
```

> Note that `n0-255` lists prefixes as preferred alternates, which is
> something we cautioned against in Alternation.  However, since we intend
> to anchor this subregexp explicitly to force an overall match, the order
> of the alternates does not matter.

The first two alternates simply get all single- and double-digit
numbers.  Since 0-padding is allowed, we need to match both 1 and 01.
We need to be careful when getting 3-digit numbers, since numbers above
255 must be excluded.  So we fashion alternates to get 000 through 199,
then 200 through 249, and finally 250 through 255.

An IP-address is a string that consists of four `n0-255`s with three
dots separating them.

```racket
> (define ip-re1                          
    (string-append                        
     "^"        ; nothing before          
     n0-255     ; the first n0-255,       
     "(?:"      ; then the subpattern of  
     "\\."      ; a dot followed by       
     n0-255     ; an n0-255,              
     ")"        ; which is                
     "{3}"      ; repeated exactly 3 times
     "$"))                                
; with nothing following                  
```

Let’s try it out:

```racket
> (regexp-match (pregexp ip-re1) "1.2.3.4")       
'("1.2.3.4")                                      
> (regexp-match (pregexp ip-re1) "55.155.255.265")
#f                                                
```

which is fine, except that we also have

```racket
> (regexp-match (pregexp ip-re1) "0.00.000.00")
'("0.00.000.00")                               
```

All-zero sequences are not valid IP addresses!  Lookahead to the rescue.
Before starting to match `ip-re1`, we look ahead to ensure we don’t have
all zeros.  We could use positive lookahead to ensure there _is_ a digit
other than zero.

```racket
> (define ip-re                                    
    (pregexp                                       
     (string-append                                
       "(?=.*[1-9])" ; ensure there's a non-0 digit
       ip-re1)))                                   
```

Or we could use negative lookahead to ensure that what’s ahead isn’t
composed of _only_ zeros and dots.

```racket
> (define ip-re                                             
    (pregexp                                                
     (string-append                                         
       "(?![0.]*$)" ; not just zeros and dots               
                    ; (note: . is not metachar inside [...])
       ip-re1)))                                            
```

The regexp `ip-re` will match all and only valid IP addresses.

```racket
> (regexp-match ip-re "1.2.3.4")
'("1.2.3.4")                    
> (regexp-match ip-re "0.0.0.0")
#f                              
```
