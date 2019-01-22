# Running and Creating Executables

While developing programs, many Racket programmers use the DrRacket
programming environment. To run a program without the development
environment, use `racket` \(for console-based programs\) or `gracket`
\(for GUI programs\). This chapter mainly explains how to run `racket`
and `gracket`.

    1 Running `racket` and `gracket`  
      1.1 Interactive Mode            
      1.2 Module Mode                 
      1.3 Load Mode                   
                                      
    2 Scripts                         
      2.1 Unix Scripts                
      2.2 Windows Batch Files         
                                      
    3 Creating Stand-Alone Executables

## 1. Running `racket` and `gracket`

The `gracket` executable is the same as `racket`, but with small
adjustments to behave as a GUI application rather than a console
application. For example, `gracket` by default runs in interactive mode
with a GUI window instead of a console prompt. GUI applications can be
run with plain `racket`, however.

Depending on command-line arguments, `racket` or `gracket` runs in
interactive mode, module mode, or load mode.

### 1.1. Interactive Mode

When `racket` is run with no command-line arguments \(other than
confguration options, like `-j`\), then it starts a REPL with a `> `
prompt:

  `Welcome to Racket v7.1.0.6.`
  `>`                          

> For enhancing your REPL experience, see `xrepl`; for information on GNU
> Readline support, see `readline`.

To initialize the REPL’s environment, `racket` first requires the
`racket/init` module, which provides all of `racket`, and also installs
`pretty-print` for display results. Finally, `racket` loads the file
reported by `(find-system-path 'init-file)`, if it exists, before
starting the REPL.

If any command-line arguments are provided \(other than configuration
options\), add `-i` or `--repl` to re-enable the REPL. For example,

  `racket -e '(display "hi\n")' -i`

displays “hi” on start-up, but still presents a REPL.

If module-requiring flags appear before `-i`/`--repl`, they cancel the
automatic requiring of `racket/init`. This behavior can be used to
initialize the REPL’s environment with a different language. For
example,

  `racket -l racket/base -i`

starts a REPL using a much smaller initial language \(that loads much
faster\). Beware that most modules do not provide the basic syntax of
Racket, including function-call syntax and `require`. For example,

  `racket -l racket/date -i`

produces a REPL that fails for every expression, because `racket/date`
provides only a few functions, and not the `#%top-interaction` and
`#%app` bindings that are needed to evaluate top-level function calls in
the REPL.

If a module-requiring flag appears after `-i`/`--repl` instead of before
it, then the module is required after `racket/init` to augment the
initial environment. For example,

  `racket -i -l racket/date`

starts a useful REPL with `racket/date` available in addition to the
exports of `racket`.

### 1.2. Module Mode

If a file argument is supplied to `racket` before any command-line
switch \(other than configuration options\), then the file is required
as a module, and \(unless `-i`/`--repl` is specified\), no REPL is
started. For example,

  `racket hello.rkt`

requires the `"hello.rkt"` module and then exits. Any argument after the
file name, flag or otherwise, is preserved as a command-line argument
for use by the required module via `current-command-line-arguments`.

If command-line flags are used, then the `-u` or `--require-script` flag
can be used to explicitly require a file as a module.  The `-t` or
`--require` flag is similar, except that additional command-line flags
are processed by `racket`, instead of preserved for the required module.
For example,

  `racket -t hello.rkt -t goodbye.rkt`

requires the `"hello.rkt"` module, then requires the `"goodbye.rkt"`
module, and then exits.

The `-l` or `--lib` flag is similar to `-t`/`--require`, but it requires
a module using a `lib` module path instead of a file path. For example,

  `racket -l raco`

is the same as running the `raco` executable with no arguments, since
the `raco` module is the executable’s main module.

Note that if you wanted to pass command-line flags to `raco` above, you
would need to protect the flags with a `--`, so that `racket` doesn’t
try to parse them itself:

  `racket -l raco -- --help`

### 1.3. Load Mode

The `-f` or `--load` flag supports `load`ing top-level expressions in a
file directly, as opposed to expressions within a module file. This
evaluation is like starting a REPL and typing the expressions directly,
except that the results are not printed. For example,

  `racket -f hi.rkts`

`load`s `"hi.rkts"` and exits. Note that load mode is generally a bad
idea, for the reasons explained in \[missing\]; using module mode is
typically better.

The `-e` or `--eval` flag accepts an expression to evaluate directly.
Unlike file loading, the result of the expression is printed, as in a
REPL. For example,

  `racket -e '(current-seconds)'`

prints the number of seconds since January 1, 1970.

For file loading and expression evaluation, the top-level environment is
created in the same way for interactive mode: `racket/init` is required
unless another module is specified first. For example,

  `racket -l racket/base -e '(current-seconds)'`

likely runs faster, because it initializes the environment for
evaluation using the smaller `racket/base` language, instead of
`racket/init`.

## 2. Scripts

Racket files can be turned into executable scripts on Unix and Mac OS.
On Windows, a compatibility layer like Cygwin support the same kind of
scripts, or scripts can be implemented as batch files.

### 2.1. Unix Scripts

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

### 2.2. Windows Batch Files

A similar trick can be used to write Racket code in Windows `.bat` batch
files:

  `; @echo off`           
  `; Racket.exe "%~f0" %*`
  `; exit /b`             
  `#lang racket/base`     
  `"Hello, world!"`       

## 3. Creating Stand-Alone Executables

For information on creating and distributing executables, see
\[missing\] and \[missing\] in \[missing\].
