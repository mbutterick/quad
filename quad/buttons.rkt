#lang racket/base
(require racket/runtime-path
         racket/gui/base
         racket/class
         quad/render
         quad/typeset
         racket/system)

(provide make-drracket-buttons)
(define-namespace-anchor cache-module-ns)
(module test racket/base) ; suppress testing by `raco test`

(define-runtime-path html-png-path "cmd-char.png")

#| for toolbar callbacks, see

http://pkg-build.racket-lang.org/doc/tools/drracket_module-language-tools.html#%28elem._drracket~3atoolbar-buttons%29

|#


(define (make-command-char-button)
  (let ([label "Render PDF"]
        [bitmap (make-object bitmap% html-png-path 'png/mask)]
        [callback (λ (drr-frame)
                    (define fn (send (send drr-frame get-definitions-text) get-filename))
                    (define pdfn (path-replace-suffix fn #".pdf"))
                    (define fn-out (parameterize ([current-namespace (make-base-namespace)])
                                     (namespace-attach-module (namespace-anchor->namespace cache-module-ns) 'quad/typeset)
                                     (dynamic-require `(submod ,fn outy) 'out)))
                    (when fn-out
                      (define-values (fn-dir name dir?) (split-path fn))
                      (parameterize ([current-directory fn-dir])
                        (local-require "render.rkt" racket/class profile sugar/debug quad/logger quad/world)
                        (activate-logger quad-logger)
                        (send (new pdf-renderer%) render-to-file (typeset fn-out) pdfn))
                      (parameterize ([current-input-port (open-input-string "")])
                        (system (format "open \"~a\"" (path->string pdfn))))))]
        [number 99])
    
    (list label bitmap callback number)))

(define (make-drracket-buttons)
  (let ([command-char-button (make-command-char-button)])
    (list command-char-button)))
