#lang racket
(require racket/dict
         (for-syntax racket/syntax))
(provide (all-defined-out))

(define proto (make-hasheq))
(define (proto-ref d k) (dict-ref d k #false))
(define (proto-set! d k v) (dict-set! d k v) d)
(define (proto? x) (dict? x))
(define type-key 'type)

(define-syntax (define-prototype stx)
  (syntax-case stx ()
    [(_ TYPE [K V] ...) #'(define-prototype TYPE proto [K V] ...)]
    [(_ TYPE BASE-TYPE [K V] ...)
     (with-syntax ([MAKE-TYPE (format-id #'TYPE "make-~a" #'TYPE)]
                   [TYPE? (format-id #'TYPE "~a?" #'TYPE)]
                   [((K-GET K-SET) ...) (for/list ([kstx (in-list (syntax->list #'(K ...)))])
                                          (list
                                           (format-id #'TYPE "~a-~a" #'TYPE kstx)
                                           (format-id #'TYPE "set-~a-~a!" #'TYPE kstx)))])
       #'(begin
           (when BASE-TYPE
             (unless (proto? BASE-TYPE)
               (raise-argument-error 'define-prototype "base type must derive from proto" BASE-TYPE)))
           (define MAKE-TYPE
             (make-keyword-procedure 
              (λ (kws kwargs . rest)
                (define allowed-ks (list 'K ...))
                (define d (proto-copy BASE-TYPE [K V] ...))
                (for ([kw (in-list kws)]
                      [kwarg (in-list kwargs)])
                  (define k (string->symbol (keyword->string kw)))
                  (unless (memq k allowed-ks)
                    (raise-argument-error 'MAKE-TYPE (format "valid keyword arg ~v" (map string->keyword (map symbol->string allowed-ks))) kw))
                  (proto-set! d k kwarg))
                ;; set type last so it overrides anything else
                (proto-set! d type-key (cons 'TYPE (dict-ref BASE-TYPE type-key null))))))
           (define (K-GET d) (proto-ref d 'K)) ...
           (define (K-SET d v) (proto-set! d 'K v)) ...
           (define TYPE (MAKE-TYPE))
           (define (TYPE? x) (and (dict? x) (memq 'TYPE (dict-ref x type-key null)) #true))))]))

(define-prototype foo [xs null])
(define-prototype bar foo [b 42])
(module+ test
  (require rackunit)
  (check-true (proto? foo))
  (check-true (proto? bar))
  (check-true (foo? foo))
  (check-true (foo? bar))
  (check-false (bar? foo))
  (check-true (bar? bar)))

(module+ test
  (make-bar)
  (proto-set! foo 'z 12)
  (make-bar))

(define-syntax-rule (proto-copy PID [K V] ...)
  (let ([d (dict-copy PID)])
    (for/last ([k (in-list (list 'K ...))]
               [v (in-list (list V ...))])
      (proto-set! d k v))))

(define (proto-equal? d1 d2)
  (and (= (length (dict-keys d1)) (length (dict-keys d2)))
       (for/and ([(k v) (in-dict d1)])
         (equal? v (proto-ref d2 k)))))