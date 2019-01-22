# Scripts

Racket files can be turned into executable scripts on Unix and Mac OS.
On Windows, a compatibility layer like Cygwin support the same kind of
scripts, or scripts can be implemented as batch files.

## 1. Unix Scripts

In a Unix environment \(including Linux and Mac OS\), a Racket file can
be turned into an executable script using the shell’s `#!` convention.
The first two characters of the file must be `#!`; the next character
must be either a space or `/`, and the remainder of the first line must
be a command to execute the script. For some platforms, the total length
of the first line is restricted to 32 characters, and sometimes the
space is required.

> Use `#lang` `racket/base` instead of `#lang` `racket` to produce scripts
> with a faster startup time.

The simplest script format uses an absolute path to a `racket`
executable followed by a module declaration. For example, if `racket` is
installed in `"/usr/local/bin"`, then a file containing the following
text acts as a “hello world” script:

  `#! /usr/local/bin/racket`
  `#lang racket/base`       
  `"Hello, world!"`         

In particular, if the above is put into a file `"hello"` and the file is
made executable \(e.g., with `chmod a+x hello`\), then typing `./hello`
at the shell prompt produces the output `"Hello, world!"`.

The above script works because the operating system automatically puts
the path to the script as the argument to the program started by the
`#!` line, and because `racket` treats a single non-flag argument as a
file containing a module to run.

Instead of specifying a complete path to the `racket` executable, a
popular alternative is to require that `racket` is in the user’s command
path, and then “trampoline” using `/usr/bin/env`:

  `#! /usr/bin/env racket`
  `#lang racket/base`     
  `"Hello, world!"`       

In either case, command-line arguments to a script are available via
`current-command-line-arguments`:

  `#! /usr/bin/env racket`                   
  `#lang racket/base`                        
  `(printf "Given arguments: ~s\n"`          
          `(current-command-line-arguments))`

If the name of the script is needed, it is available via
`(find-system-path 'run-file)`, instead of via
`(current-command-line-arguments)`.

Usually, the best way to handle command-line arguments is to parse them
using the `command-line` form provided by `racket`. The `command-line`
form extracts command-line arguments from
`(current-command-line-arguments)` by default:

  `#! /usr/bin/env racket`                     
  `#lang racket`                               
                                               
  `(define verbose? (make-parameter #f))`      
                                               
  `(define greeting`                           
    `(command-line`                            
     `#:once-each`                             
     `[("-v") "Verbose mode" (verbose? #t)]`   
     `#:args`                                  
     `(str) str))`                             
                                               
  `(printf "~a~a\n"`                           
          `greeting`                           
          `(if (verbose?) " to you, too!" ""))`

Try running the above script with the `--help` flag to see what
command-line arguments are allowed by the script.

An even more general trampoline uses `/bin/sh` plus some lines that are
comments in one language and expressions in the other. This trampoline
is more complicated, but it provides more control over command-line
arguments to `racket`:

  `#! /bin/sh`                                                 
  `#|`                                                         
  `exec racket -e '(printf "Running...\n")' -u "$0" ${1+"$@"}` 
  `|#`                                                         
  `#lang racket/base`                                          
  `(printf "The above line of output had been produced via\n")`
  `(printf "a use of the `-e' flag.\n")`                       
  `(printf "Given arguments: ~s\n"`                            
          `(current-command-line-arguments))`                  

Note that `#!` starts a line comment in Racket, and `#|`...`|#` forms a
block comment. Meanwhile, `#` also starts a shell-script comment, while
`exec racket` aborts the shell script to start `racket`. That way, the
script file turns out to be valid input to both `/bin/sh` and `racket`.

## 2. Windows Batch Files

A similar trick can be used to write Racket code in Windows `.bat` batch
files:

  `; @echo off`           
  `; Racket.exe "%~f0" %*`
  `; exit /b`             
  `#lang racket/base`     
  `"Hello, world!"`       
