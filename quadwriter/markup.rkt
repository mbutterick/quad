#lang debug racket/base
;; `quadwriter/markup` was a misleading name for this dialect
;; it is now `quadwriter/html`,
;; but we will make `quadwriter/markup` work the same way for compatibility
(require "html.rkt")
(provide (all-from-out "html.rkt"))

(module reader racket/base
  (require (submod "html.rkt" reader))
  (provide (all-from-out (submod "html.rkt" reader))))