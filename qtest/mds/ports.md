# Input and Output Ports

A _port_ encapsulates an I/O stream, normally for just one direction. An
_input port_ reads from a stream, and an _output port_ writes to a
string.

For many procedures that accept a port argument, the argument is
optional, and it defaults to either the _current input port_ or _current
output port_. For `mzscheme`, the current ports are initialized to the
process’s stdin and stdout. The `current-input-port` and
`current-output-port` procedures, when called with no arguments, return
the current output and input port, respectively.

Examples:

```racket
> (display "hello world\n")                      
hello world                                      
> (display "hello world\n" (current-output-port))
hello world                                      
```

Ports are created by various procedures that specific to the different
kinds of streams. For example, `open-input-file` creates an input port
for reading from a file. Procedures like `with-input-from-file` both
create a port and install it as the current port while calling a given
body procedure.

See \[missing\] for information about using ports.
