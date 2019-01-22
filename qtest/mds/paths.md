# Paths

A _path_ encapsulates a filesystem path that \(potentially\) names a
file or directory. Although paths can be converted to and from strings
and byte strings, neither strings nor byte strings are suitable for
representing general paths. The problem is that paths are represented in
the filesystem as either byte sequences or UTF-16 sequences \(depending
on the operating systems\); the sequences are not always human-readable,
and not all sequences can be decoded to Unicode scalar values.

Despite the occasional encoding problems, most paths can be converted to
and from strings. Thus, procedures that accept a path argument always
accept a string, and the printed form of a path uses the string decoding
of the path inside `#<path:` and `>`. The `display` form of a path is
the same as the `display` form of its string encodings.

Examples:

```racket
> (string->path "my-data.txt")               
#<path:my-data.txt>                          
> (file-exists? "my-data.txt")               
#f                                           
> (file-exists? (string->path "my-data.txt"))
#f                                           
> (display (string->path "my-data.txt"))     
my-data.txt                                  
```

Procedures that produce references to the filesystem always produce path
values, instead of strings.

Example:

```racket
> (path-replace-suffix "foo.scm" #".rkt")
#<path:foo.rkt>                          
```

Although it’s sometimes tempting to directly manipulate strings that
represent filesystem paths, correctly manipulating a path can be
surprisingly difficult. Windows path manipulation is especially tricky,
because path elements like `"aux"` can have special meanings.

> +\[missing\] in \[missing\] documents the fine points of Windows
> filesystem paths.

Use procedures like `split-path` and `build-path` to deconstruct and
construct paths. When you must manipulate the name of a specific path
element \(i.e., a file or directory component in a path\), use
procedures like `path-element->bytes` and `bytes->path-element`.

Examples:

```racket
> (build-path "easy" "file.rkt")             
#<path:easy/file.rkt>                        
> (split-path (build-path "easy" "file.rkt"))
#<path:easy/>                                
#<path:file.rkt>                             
#f                                           
```
