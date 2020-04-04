#lang debug racket
(require "main.rkt")

(module+ raco
  ;; pull text out of stdin, if any
  ;; todo: make this cooperate with `read` for confirmation of replace
  (define text (string-join (for/list ([t (in-port read)])
                          (format "~a" t)) " "))
  (define command-name (with-handlers ([exn:fail? (λ (exn) #f)])
                         (vector-ref (current-command-line-arguments) 0)))
  (dispatch command-name text))

(module+ main
  (println "this is fontproof command"))

(define (dispatch command-name text)
  (when (positive? (vector-length (current-command-line-arguments)))
    (define output-file-path #false)
    (define page-size #false)
    (define font-sizes #false)
    (define line-heights #false)
    (define doc #false)
    (define replace #false)
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
       #:args families
       families))
    (cond
      [(null? families) (displayln "no family given")]
      [else (for ([family (in-list families)])
                 (make-proof family (if (non-empty-string? text) text doc)
                             #:page-size page-size
                             #:font-sizes font-sizes
                             #:line-heights line-heights
                             #:output-file-path output-file-path
                             #:replace replace))])))


