# Distributed Places

The `racket/place/distributed` library provides support for distributed
programming.

The example bellow demonstrates how to launch a remote racket node
instance, launch remote places on the new remote node instance, and
start an event loop that monitors the remote node instance.

The example code can also be found in
`"racket/distributed/examples/named/master.rkt"`.

```racket
#lang racket/base                                      
(require racket/place/distributed                      
         racket/class                                  
         racket/place                                  
         racket/runtime-path                           
         "bank.rkt"                                    
         "tuple.rkt")                                  
(define-runtime-path bank-path "bank.rkt")             
(define-runtime-path tuple-path "tuple.rkt")           
                                                       
(provide main)                                         
                                                       
(define (main)                                         
  (define remote-node (spawn-remote-racket-node        
                        "localhost"                    
                        #:listen-port 6344))           
  (define tuple-place (supervise-place-at              
                        remote-node                    
                        #:named 'tuple-server          
                        tuple-path                     
                        'make-tuple-server))           
  (define bank-place  (supervise-place-at              
                        remote-node bank-path          
                        'make-bank))                   
                                                       
  (message-router                                      
    remote-node                                        
    (after-seconds 4                                   
      (displayln (bank-new-account bank-place 'user0)) 
      (displayln (bank-add bank-place 'user0 10))      
      (displayln (bank-removeM bank-place 'user0 5)))  
                                                       
    (after-seconds 2                                   
      (define c (connect-to-named-place remote-node    
                                        'tuple-server))
      (define d (connect-to-named-place remote-node    
                                        'tuple-server))
      (tuple-server-hello c)                           
      (tuple-server-hello d)                           
      (displayln (tuple-server-set c "user0" 100))     
      (displayln (tuple-server-set d "user2" 200))     
      (displayln (tuple-server-get c "user0"))         
      (displayln (tuple-server-get d "user2"))         
      (displayln (tuple-server-get d "user0"))         
      (displayln (tuple-server-get c "user2"))         
      )                                                
    (after-seconds 8                                   
      (node-send-exit remote-node))                    
    (after-seconds 10                                  
      (exit 0))))                                      
                                                       
```
Figure 1: examples/named/master.rkt

The `spawn-remote-racket-node` primitive connects to `"localhost"` and
starts a racloud node there that listens on port 6344 for further
instructions.  The handle to the new racloud node is assigned to the
`remote-node` variable. Localhost is used so that the example can be run
using only a single machine.  However localhost can be replaced by any
host with ssh publickey access and racket.  The `supervise-place-at`
creates a new place on the `remote-node`.  The new place will be
identified in the future by its name symbol `'tuple-server`.  A place
descriptor is expected to be returned by invoking `dynamic-place` with
the `tuple-path` module path and the `'make-tuple-server` symbol.

The code for the tuple-server place exists in the file `"tuple.rkt"`.
The `"tuple.rkt"` file contains the use of `define-named-remote-server`
form, which defines a RPC server suitiable for invocation by
`supervise-place-at`.

```racket
#lang racket/base                          
(require racket/match                      
         racket/place/define-remote-server)
                                           
(define-named-remote-server tuple-server   
  (define-state h (make-hash))             
  (define-rpc (set k v)                    
    (hash-set! h k v)                      
    v)                                     
  (define-rpc (get k)                      
    (hash-ref h k #f))                     
  (define-cast (hello)                     
    (printf "Hello from define-cast\n")    
    (flush-output)))                       
                                           
```
Figure 2: examples/named/tuple.rkt

The `define-named-remote-server` form takes an identifier and a list of
custom expressions as its arguments.  From the identifier a place-thunk
function is created by prepending the `make-` prefix. In this case
`make-tuple-server`.  The `make-tuple-server` identifier is the
`place-function-name` given to the `supervise-named-dynamic-place-at`
form above. The `define-state` custom form translates into a simple
`define` form, which is closed over by the `define-rpc` form.

The `define-rpc` form is expanded into two parts. The first part is the
client stubs that call the rpc functions. The client function name is
formed by concatenating the `define-named-remote-server` identifier,
`tuple-server`, with the RPC function name `set` to form
`tuple-server-set`. The RPC client functions take a destination argument
which is a `remote-connection%` descriptor and then the RPC function
arguments. The RPC client function sends the RPC function name, `set`,
and the RPC arguments to the destination by calling an internal function
`named-place-channel-put`. The RPC client then calls
`named-place-channel-get` to wait for the RPC response.

The second expansion part of `define-rpc` is the server implementation
of the RPC call.  The server is implemented by a match expression inside
the `make-tuple-server` function.  The match clause for
`tuple-server-set` matches on messages beginning with the `'set` symbol.
The server executes the RPC call with the communicated arguments and
sends the result back to the RPC client.

The `define-cast` form is similar to the `define-rpc` form except there
is no reply message from the server to client

```racket
(module tuple racket/base                              
  (require racket/place                                
           racket/match)                               
  (define/provide                                      
   (tuple-server-set dest k v)                         
   (named-place-channel-put dest (list 'set k v))      
   (named-place-channel-get dest))                     
  (define/provide                                      
   (tuple-server-get dest k)                           
   (named-place-channel-put dest (list 'get k))        
   (named-place-channel-get dest))                     
  (define/provide                                      
   (tuple-server-hello dest)                           
   (named-place-channel-put dest (list 'hello)))       
  (define/provide                                      
   (make-tuple-server ch)                              
    (let ()                                            
      (define h (make-hash))                           
      (let loop ()                                     
        (define msg (place-channel-get ch))            
        (define (log-to-parent-real                    
                  msg                                  
                  #:severity (severity 'info))         
          (place-channel-put                           
            ch                                         
            (log-message severity msg)))               
        (syntax-parameterize                           
         ((log-to-parent (make-rename-transformer      
                           #'log-to-parent-real)))     
         (match                                        
          msg                                          
          ((list (list 'set k v) src)                  
           (define result (let () (hash-set! h k v) v))
           (place-channel-put src result)              
           (loop))                                     
          ((list (list 'get k) src)                    
           (define result (let () (hash-ref h k #f)))  
           (place-channel-put src result)              
           (loop))                                     
          ((list (list 'hello) src)                    
           (define result                              
             (let ()                                   
               (printf "Hello from define-cast\n")     
               (flush-output)))                        
           (loop))))                                   
        loop))))                                       
```
Figure 3: Expansion of define-named-remote-server
