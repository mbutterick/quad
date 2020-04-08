#lang debug racket
(require racket/logging
         "main.rkt")

(module+ raco
  (with-logging-to-port
   (current-error-port)
   handle-raco-command
   #:logger fontproof-logger
   'info
   'fontproof))

(module+ main
  (println "this is fontproof command"))

(define (handle-raco-command)
  (define command-name (with-handlers ([exn:fail? (λ (exn) #f)])
                         (vector-ref (current-command-line-arguments) 0)))
  (define output-file-path #false)
  (define page-size #false)
  (define font-sizes #false)
  (define line-heights #false)
  (define doc #false)
  (define replace #false)
  (define output-qml? #false)
  (define make-bold? #false)
  (define make-italic? #false)
  (define families
    (command-line
     #:program "fontproof"
     #:argv (current-command-line-arguments)
     #:once-each
     [("-p" "--page") page-size-arg
                      "page size"
                      (set! page-size page-size-arg)]
     [("-r" "--replace") "replace existing"
                         (set! replace #true)]
     [("-d" "--doc") doc-arg
                     "sample text"
                     (set! doc doc-arg)]
     [("-o" "--output") output-file-path-arg
                        "output file path"
                        (set! output-file-path output-file-path-arg)]
     [("-s" "--size") font-sizes-arg
                      "font sizes"
                      (set! font-sizes font-sizes-arg)]
     [("-l" "--leading") line-heights-arg
                         "font size"
                         (set! line-heights line-heights-arg)]
     [("-b" "--bold") "also generate bold proof"
                      (set! make-bold? #true)]
     [("-i" "--italic")  "also generate italic proof"
                         (set! make-italic? #true)]
     [("-q" "--qml")  "output QML"
                      (set! output-qml? #true)]
     #:args families
     families))
  (match families
    [(? null?) (raise-user-error "No font to proof. Exiting.")]
    [_ (for ([family (in-list families)])
            (make-proof family
                        (or doc (match (current-input-port)
                                  ;; pull text out of stdin, if any
                                  ;; use `terminal-port?` to distinguish piped input from tty input
                                  [(not (? terminal-port?))
                                   (string-join (for/list ([t (in-port read)])
                                                          (format "~a" t)) " ")]
                                  [_ #false]))
                        #:page-size page-size
                        #:bold make-bold?
                        #:italic make-italic?
                        #:font-sizes font-sizes
                        #:line-heights line-heights
                        #:output-file-path output-file-path
                        #:replace replace
                        #:qml output-qml?))]))


