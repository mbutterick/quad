#lang debug racket
(require "main.rkt")

(module+ raco
  (define command-name (with-handlers ([exn:fail? (λ (exn) #f)])
                         (vector-ref (current-command-line-arguments) 0)))
  (dispatch command-name))

(module+ main
  (println "this is fontproof command"))

(define (dispatch command-name)
  (when (positive? (vector-length (current-command-line-arguments)))
    (define output-file-path #false)
    (define page-size #false)
    (define font-sizes #false)
    (define line-heights #false)
    (define families
      (command-line #:program "fontproof"
                    #:argv (current-command-line-arguments)
                    #:once-each
                    [("-p" "--page") page-size-arg "page size" (set! page-size page-size-arg)]
                    [("-o" "--output") output-file-path-arg "output file path" (set! output-file-path output-file-path-arg)]
                    [("-s" "--size") font-sizes-arg "font sizes" (set! font-sizes font-sizes-arg)]
                    [("-l" "--line") line-heights-arg "line heights" (set! line-heights line-heights-arg)]
                    #:args families
                    families))
    (for ([family (in-list families)])
         (make-proof family
                     #:page-size page-size
                     #:font-sizes font-sizes
                     #:line-heights line-heights
                     #:output-file-path output-file-path))))


