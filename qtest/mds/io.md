# Input and Output

> A Racket port corresponds to the Unix notion of a stream \(not to be
> confused with `racket/stream`’s streams\).

A Racket _port_ represents a source or sink of data, such as a file, a
terminal, a TCP connection, or an in-memory string.  Ports provide
sequential access in which data can be read or written a piece of a
time, without requiring the data to be consumed or produced all at once.
More specifically, an _input port_ represents a source from which a
program can read data, and an _output port_ represents a sink to which a
program can write data.

    1 Varieties of Ports              
                                      
    2 Default Ports                   
                                      
    3 Reading and Writing Racket Data 
                                      
    4 Datatypes and Serialization     
                                      
    5 Bytes, Characters, and Encodings
                                      
    6 I/O Patterns                    

## 1. Varieties of Ports

Various functions create various kinds of ports. Here are a few
examples:

* **Files:** The `open-output-file` function opens a   file for writing,
  and `open-input-file` opens a file for   reading.

  Examples:

  ```racket
> (define out (open-output-file "data"))
  > (display "hello" out)                 
  > (close-output-port out)               
  > (define in (open-input-file "data"))  
  > (read-line in)                        
  "hello"                                 
  > (close-input-port in)                 
```

  If a file exists already, then `open-output-file` raises an exception
  by default. Supply an option like `#:exists 'truncate` or `#:exists
  'update` to re-write or update the file:

  Examples:

  ```racket
> (define out (open-output-file "data" #:exists 'truncate))
  > (display "howdy" out)                                    
  > (close-output-port out)                                  
```

  Instead of having to match the open calls with close calls, most
  Racket programmers will use the `call-with-input-file` and
  `call-with-output-file` functions which take a function to call to
  carry out the desired operation. This function gets as its only
  argument the port, which is automatically opened and closed for the
  operation.

  Examples:

  ```racket
> (call-with-output-file "data"                    
                            #:exists 'truncate       
                            (lambda (out)            
                              (display "hello" out)))
  > (call-with-input-file "data"                     
                          (lambda (in)               
                            (read-line in)))         
  "hello"                                            
```

* **Strings:** The `open-output-string` function creates a port that
  accumulates data into a string, and `get-output-string` extracts the
  accumulated string. The `open-input-string` function creates a port to
  read from a string.

  Examples:

  ```racket
> (define p (open-output-string))                    
  > (display "hello" p)                                
  > (get-output-string p)                              
  "hello"                                              
  > (read-line (open-input-string "goodbye\nfarewell"))
  "goodbye"                                            
```

* **TCP Connections:** The `tcp-connect` function creates both an input
  port and an output port for the client side of a TCP communication.
  The `tcp-listen` function creates a server, which accepts connections
  via `tcp-accept`.

  Examples:

  ```racket
> (define server (tcp-listen 12345))                          
  > (define-values (c-in c-out) (tcp-connect "localhost" 12345))
  > (define-values (s-in s-out) (tcp-accept server))            
  > (display "hello\n" c-out)                                   
  > (close-output-port c-out)                                   
  > (read-line s-in)                                            
  "hello"                                                       
  > (read-line s-in)                                            
  #<eof>                                                        
```

* **Process Pipes:** The `subprocess` function runs a new process at the
  OS level and returns ports that correspond to the subprocess’s stdin,
  stdout, and stderr. \(The first three arguments can be certain kinds
  of existing ports to connect directly to the subprocess, instead of
  creating new ports.\)

  Examples:

  ```racket
> (define-values (p stdout stdin stderr)     
      (subprocess #f #f #f "/usr/bin/wc" "-w"))
  > (display "a b c\n" stdin)                  
  > (close-output-port stdin)                  
  > (read-line stdout)                         
  "       3"                                   
  > (close-input-port stdout)                  
  > (close-input-port stderr)                  
```

* **Internal Pipes:** The `make-pipe` function returns two ports that
  are ends of a pipe. This kind of pipe is internal to Racket, and not
  related to OS-level pipes for communicating between different
  processes.

  Examples:

  ```racket
> (define-values (in out) (make-pipe))
  > (display "garbage" out)             
  > (close-output-port out)             
  > (read-line in)                      
  "garbage"                             
```

## 2. Default Ports

For most simple I/O functions, the target port is an optional argument,
and the default is the _current input port_ or _current output port_.
Furthermore, error messages are written to the _current error port_,
which is an output port. The `current-input-port`,
`current-output-port`, and `current-error-port` functions return the
corresponding current ports.

Examples:

```racket
> (display "Hi")                                 
Hi                                               
> (display "Hi" (current-output-port)) ; the same
Hi                                               
```

If you start the `racket` program in a terminal, then the current input,
output, and error ports are all connected to the terminal. More
generally, they are connected to the OS-level stdin, stdout, and stderr.
In this guide, the examples show output written to stdout in purple, and
output written to stderr in red italics.

Examples:

```racket
(define (swing-hammer)                   
  (display "Ouch!" (current-error-port)))
                                         
> (swing-hammer)                         
Ouch!                                    
```

The current-port functions are actually parameters, which means that
their values can be set with `parameterize`.

> See \[missing\] for an introduction to parameters.

Example:

```racket
> (let ([s (open-output-string)])         
    (parameterize ([current-error-port s])
      (swing-hammer)                      
      (swing-hammer)                      
      (swing-hammer))                     
    (get-output-string s))                
"Ouch!Ouch!Ouch!"                         
```

## 3. Reading and Writing Racket Data

As noted throughout \[missing\], Racket provides three ways to print an
instance of a built-in value:

* `print`, which prints a value in the same way that is it printed for a
  REPL result; and

* `write`, which prints a value in such a way that `read` on the output
  produces the value back; and

* `display`, which tends to reduce a value to just its character or byte
  content—at least for those datatypes that are primarily about
  characters or bytes, otherwise it falls back to the same output as
  `write`.

Here are some examples using each:

```racket            ```racket            ```racket             
> (print 1/2)        > (write 1/2)        > (display 1/2)       
1/2                  1/2                  1/2                   
> (print #\x)        > (write #\x)        > (display #\x)       
#\x                  #\x                  x                     
> (print "hello")    > (write "hello")    > (display "hello")   
"hello"              "hello"              hello                 
> (print #"goodbye") > (write #"goodbye") > (display #"goodbye")
#"goodbye"           #"goodbye"           goodbye               
> (print '|pea pod|) > (write '|pea pod|) > (display '|pea pod|)
'|pea pod|           |pea pod|            pea pod               
> (print '("i" pod)) > (write '("i" pod)) > (display '("i" pod))
'("i" pod)           ("i" pod)            (i pod)               
> (print write)      > (write write)      > (display write)     
#<procedure:write>   #<procedure:write>   #<procedure:write>    
```                  ```                  ```                   

Overall, `print` corresponds to the expression layer of Racket syntax,
`write` corresponds to the reader layer, and `display` roughly
corresponds to the character layer.

The `printf` function supports simple formatting of data and text. In
the format string supplied to `printf`, `~a` `display`s the next
argument, `~s` `write`s the next argument, and `~v` `print`s the next
argument.

Examples:

```racket
(define (deliver who when what)                        
  (printf "Items ~a for shopper ~s: ~v" who when what))
                                                       
> (deliver '("list") '("John") '("milk"))              
Items (list) for shopper ("John"): '("milk")           
```

After using `write`, as opposed to `display` or `print`, many forms of
data can be read back in using `read`. The same values `print`ed can
also be parsed by `read`, but the result may have extra quote forms,
since a `print`ed form is meant to be read like an expression.

Examples:

```racket
> (define-values (in out) (make-pipe))           
> (write "hello" out)                            
> (read in)                                      
"hello"                                          
> (write '("alphabet" soup) out)                 
> (read in)                                      
'("alphabet" soup)                               
> (write #hash((a . "apple") (b . "banana")) out)
> (read in)                                      
'#hash((a . "apple") (b . "banana"))             
> (print '("alphabet" soup) out)                 
> (read in)                                      
”("alphabet" soup)                               
> (display '("alphabet" soup) out)               
> (read in)                                      
'(alphabet soup)                                 
```

## 4. Datatypes and Serialization

Prefab structure types \(see \[missing\]\) automatically support
_serialization_: they can be written to an output stream, and a copy can
be read back in from an input stream:

```racket
> (define-values (in out) (make-pipe))
> (write #s(sprout bean) out)         
> (read in)                           
'#s(sprout bean)                      
```

Other structure types created by `struct`, which offer more abstraction
than prefab structure types, normally `write` either using `#<....>`
notation \(for opaque structure types\) or using `#(....)` vector
notation \(for transparent structure types\). In neither case can the
result be read back in as an instance of the structure type:

```racket
> (struct posn (x y))                 
> (write (posn 1 2))                  
#<posn>                               
> (define-values (in out) (make-pipe))
> (write (posn 1 2) out)              
> (read in)                           
pipe::1: read: bad syntax `#<`        
```

```racket
> (struct posn (x y) #:transparent)   
> (write (posn 1 2))                  
#(struct:posn 1 2)                    
> (define-values (in out) (make-pipe))
> (write (posn 1 2) out)              
> (define v (read in))                
> v                                   
'#(struct:posn 1 2)                   
> (posn? v)                           
#f                                    
> (vector? v)                         
#t                                    
```

The `serializable-struct` form defines a structure type that can be
`serialize`d to a value that can be printed using `write` and restored
via `read`. The `serialize`d result can be `deserialize`d to get back an
instance of the original structure type. The serialization form and
functions are provided by the `racket/serialize` library.

Examples:

```racket
> (require racket/serialize)                             
> (serializable-struct posn (x y) #:transparent)         
> (deserialize (serialize (posn 1 2)))                   
(posn 1 2)                                               
> (write (serialize (posn 1 2)))                         
((3) 1 ((#f . deserialize-info:posn-v0)) 0 () () (0 1 2))
> (define-values (in out) (make-pipe))                   
> (write (serialize (posn 1 2)) out)                     
> (deserialize (read in))                                
(posn 1 2)                                               
```

In addition to the names bound by `struct`, `serializable-struct` binds
an identifier with deserialization information, and it automatically
`provide`s the deserialization identifier from a module context. This
deserialization identifier is accessed reflectively when a value is
deserialized.

## 5. Bytes, Characters, and Encodings

Functions like `read-line`, `read`, `display`, and `write` all work in
terms of characters \(which correspond to Unicode scalar values\).
Conceptually, they are implemented in terms of `read-char` and
`write-char`.

More primitively, ports read and write bytes, instead of characters. The
functions `read-byte` and `write-byte` read and write raw bytes. Other
functions, such as `read-bytes-line`, build on top of byte operations
instead of character operations.

In fact, the `read-char` and `write-char` functions are conceptually
implemented in terms of `read-byte` and `write-byte`. When a single
byte’s value is less than 128, then it corresponds to an ASCII
character. Any other byte is treated as part of a UTF-8 sequence, where
UTF-8 is a particular standard way of encoding Unicode scalar values in
bytes \(which has the nice property that ASCII characters are encoded as
themselves\). Thus, a single `read-char` may call `read-byte` multiple
times, and a single `write-char` may generate multiple output bytes.

The `read-char` and `write-char` operations _always_ use a UTF-8
encoding. If you have a text stream that uses a different encoding, or
if you want to generate a text stream in a different encoding, use
`reencode-input-port` or `reencode-output-port`. The
`reencode-input-port` function converts an input stream from an encoding
that you specify into a UTF-8 stream; that way, `read-char` sees UTF-8
encodings, even though the original used a different encoding. Beware,
however, that `read-byte` also sees the re-encoded data, instead of the
original byte stream.

## 6. I/O Patterns

If you want to process individual lines of a file, then you can use
`for` with `in-lines`:

```racket
> (define (upcase-all in)                 
    (for ([l (in-lines in)])              
      (display (string-upcase l))         
      (newline)))                         
> (upcase-all (open-input-string          
               (string-append             
                "Hello, World!\n"         
                "Can you hear me, now?")))
HELLO, WORLD!                             
CAN YOU HEAR ME, NOW?                     
```

If you want to determine whether “hello” appears in a file, then you
could search separate lines, but it’s even easier to simply apply a
regular expression \(see \[missing\]\) to the stream:

```racket
> (define (has-hello? in)                   
    (regexp-match? #rx"hello" in))          
> (has-hello? (open-input-string "hello"))  
#t                                          
> (has-hello? (open-input-string "goodbye"))
#f                                          
```

If you want to copy one port into another, use `copy-port` from
`racket/port`, which efficiently transfers large blocks when lots of
data is available, but also transfers small blocks immediately if that’s
all that is available:

```racket
> (define o (open-output-string))          
> (copy-port (open-input-string "broom") o)
> (get-output-string o)                    
"broom"                                    
```
