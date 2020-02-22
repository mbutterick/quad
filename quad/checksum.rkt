#lang racket/base
(require racket/match pkg/path)
(provide pkg-checksum)

(define (pkg-checksum [pkg "quad"] #:short [short? #false])
  (match (for/or ([scope (in-list '(user installation shared))])
           (hash-ref (read-pkgs-db scope) pkg #false))
    [(pkg-info _ (? string? checksum) _) (if short? (substring checksum 0 7) checksum)]
    [_ "checksum not found"]))
