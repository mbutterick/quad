#lang debug racket
(require racket/logging
         racket/match
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
  (define query-mode? #false)
  (define families
    (command-line
     #:program "fontproof"
     #:argv (current-command-line-arguments)
     #:once-each
     [("-p" "--page") page-size-arg
                      "page size (e.g., letter, A4)"
                      (set! page-size page-size-arg)]
     [("-r" "--replace") "replace existing proof with same name"
                         (set! replace #true)]
     [("-d" "--doc") doc-arg
                     "source for sample text"
                     (set! doc doc-arg)]
     [("-o" "--output") output-file-path-arg
                        "output file path"
                        (set! output-file-path output-file-path-arg)]
     [("-s" "--size") font-sizes-arg
                      "font sizes to proof"
                      (set! font-sizes font-sizes-arg)]
     [("-l" "--leading") line-heights-arg
                         "line height"
                         (set! line-heights line-heights-arg)]
     [("-b" "--bold") "also generate bold proof (only works with family name)"
                      (set! make-bold? #true)]
     [("-i" "--italic")  "also generate italic proof (only works with family name)"
                         (set! make-italic? #true)]
     [("-q" "--qml")  "output QML file"
                      (set! output-qml? #true)]
     [("--query")  "resolve each family name and font path and show status"
                   (set! query-mode? #true)]
     #:args font-family-names-or-font-paths
     font-family-names-or-font-paths))
  (match families
    [(? null?) (raise-user-error "no fonts to proof; exiting")]
    [_ (for ([family (in-list families)])
            (cond
              [query-mode?
               (define status
                 (match family
                   [(? font-path-string? ps)
                    (format (if (file-exists? ps) "found at ~v" "not found at ~v") (path->string (path->complete-path ps)))]
                   [_ (match-define-values (trimmed-family bold? italic?) (resolve-family-bold-italic family))
                      (match ((dynamic-require 'fontland/font-path 'family->path) trimmed-family #:bold bold? #:italic italic?)
                        [#false "not found among installed fonts"]
                        [(app path->string ps) (format "found installed at ~v" ps)])]))
               (log-info (format "family ~v ~a" family status))]
              [else
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
                           #:qml output-qml?)]))]))


