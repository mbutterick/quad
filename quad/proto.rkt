#lang debug racket
(require racket/dict
         (for-syntax racket/syntax))
(provide (all-defined-out))

(define proto (make-vector 1 '(proto)))
(define (proto-ref d k) (dict-ref d k #false))
(define (proto-set! d k v) (dict-set! d k v) d)
(define (proto? x) (dict? x))
(define type-key 0)

(define-syntax (define-prototype stx)
  (syntax-case stx ()
    [(_ TYPE [K V] ...) #'(define-prototype TYPE proto [K V] ...)]
    [(_ TYPE BASE-TYPE [K V] ...)
     (with-syntax ([MAKE-TYPE (format-id #'TYPE "make-~a" #'TYPE)]
                   [TYPE-COPY (format-id #'TYPE "~a-copy" #'TYPE)]
                   [TYPE? (format-id #'TYPE "~a?" #'TYPE)]
                   [((K-GET K-SET) ...) (for/list ([kstx (in-list (syntax->list #'(K ...)))])
                                          (list
                                           (format-id #'TYPE "~a-~a" #'TYPE kstx)
                                           (format-id #'TYPE "set-~a-~a!" #'TYPE kstx)))])
       #'(begin
           (when BASE-TYPE
             (unless (proto? BASE-TYPE)
               (raise-argument-error 'define-prototype "base type must derive from proto" BASE-TYPE)))
           
           (...
            (define-syntax-rule (TYPE-COPY PID [K2 V2] ...)
              (let ([d (vector-append PID (make-vector (length (list 'K2 ...))))])
                (for ([(k ki) (in-indexed (list 'K2 ...))]
                      [v (in-list (list V2 ...))])
                  (proto-set! d (+ ki (vector-length BASE-TYPE)) v))
                d)))

           (define allowed-ks (reverse (list 'K ...)))
           (define (MAKE-TYPE #:printable [printable #f]
                              #:id [id #f]
                              #:draw [draw #f]
                              #:draw-end [draw-end #f]
                              #:from [from #f]
                              #:elems [elems null])
                              
             (define d (TYPE-COPY BASE-TYPE [K V] ...))
             
             ;; set type last so it overrides anything else
             (proto-set! d type-key (cons 'TYPE (dict-ref BASE-TYPE type-key null))))
           (define (K-GET d) (proto-ref d 'K)) ...
           (define (K-SET d v) (proto-set! d (length (memq 'K allowed-ks)) v)) ...
           (define TYPE (MAKE-TYPE))
           (define (TYPE? x) (and (proto? x) (memq 'TYPE (dict-ref x type-key null)) #true))))]))

(define-prototype x) 
(define-prototype foo [xs 'hoo])
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
  (set-foo-xs! foo 12)
  (make-bar))

(define (proto-equal? d1 d2)
  (and (= (length (dict-keys d1)) (length (dict-keys d2)))
       (for/and ([(k v) (in-dict d1)])
         (equal? v (proto-ref d2 k)))))