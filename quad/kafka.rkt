#lang racket/base
(require quad/typewriter racket/file racket/runtime-path racket/path)
(define-runtime-path kafka "kafka.txt")
(run (qexpr->quad (quad #:fontsize "18" (file->string kafka))) (path-replace-extension kafka #".pdf"))

