#lang debug br
(require racket/runtime-path pitfall/document)
(define-runtime-path fira "fira.ttf")
(define PS "typewriter-raw.pdf")
(define doc (make-object PDFDocument
              (hasheq 'compress #t
                      'autoFirstPage #t
                      'size '(300 200))))
(time (send* doc
  [pipe (open-output-file PS #:exists 'replace)]
  [registerFont "Fira" (path->string fira)]
  [font "Fira"]
  [fontSize 12]
  [text "Hello world" 36 36]))
(send doc end)