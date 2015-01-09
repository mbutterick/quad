#lang racket/base
(require (for-syntax racket/base racket/syntax racket/string) racket/string racket/contract racket/serialize sugar/list racket/format racket/list sugar/debug sugar/coerce racket/bool racket/function sugar/string)
(require "world.rkt")
(provide (all-defined-out))

;; struct implementation

(serializable-struct quad (name attrs list) #:transparent
                     #:methods gen:custom-write
                     [(define write-proc (λ(b port mode)
                                           (display (format "(~a)" (string-join (filter-not void? (list
                                                                                                   (~a (quad-name b)) 
                                                                                                   (if (and (hash? (quad-attrs b)) (> (length (hash-keys (quad-attrs b))) 0)) (~v (flatten (hash->list (quad-attrs b)))) (void))
                                                                                                   (if (> (length (quad-list b)) 0) (~a (string-join (map ~v (quad-list b)) "")) (void)))) " ")) port)))]
                     #:property prop:sequence (λ(q) (quad-list q)))



;; vector implementation
#|
(define (quad-name q) (vector-ref q 0))
(define (quad-attrs q) (vector-ref q 1))
(define (quad-list q) (vector-ref q 2))

(define (quad? x)
  (and (vector? x)
       (symbol? (quad-name x))
       (or (false? (quad-attrs x)) (hash? (quad-attrs x)))
       (list? (quad-list x))))

(define (quad name attrs xs)
  (vector name attrs xs))
|#
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; hash implementation
#|
(define (quad-name q) (hash-ref q 'name))
(define (quad-attrs q) (hash-ref q 'attrs))
(define (quad-list q) (hash-ref q 'list))

(define (quad? x)
  (and (hash? x)
       (andmap (λ(k) (hash-has-key? x k)) (list 'name 'attrs 'list))
       (symbol? (quad-name x))
       (ormap (λ(pred) (pred (quad-attrs x))) (list false? hash?))
       (list? (quad-list x))))

(define (quad name attrs xs)
  (hash 'name name 'attrs attrs 'list xs))
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (quad-name? x) (symbol? x))
(define (hashable-list? x) (and (list? x) (even? (length x))))
(define (quad-attrs? x) (or (false? x) (hash? x)))
(define (quad-list? x) (and (list? x) (andmap (λ(xi) (or (quad? xi) (and (string? xi) (< 0 (string-length xi))))) x)))
(define (quads? x) (and (list? x) (andmap quad? x)))
(define (lists-of-quads? x) (and (list? x) (andmap quads? x)))

(define quad= equal?)  

(define token? quad?)

(define (quad/c x) (λ(x) (and (quad? x) (symbol? (quad-name x)) (hash? (quad-attrs x)) 
                              (andmap (λ(xi) (or (quad/c xi) (string? xi))) (quad-list x))))) 

(define quad-attr-ref
  (case-lambda
    [(q key) 
     (if (quad-attrs q) 
         (hash-ref (quad-attrs q) key)
         (error 'quad-attr-ref (format "no attrs in quad ~a" q)))]
    [(q key default) 
     (if (quad-attrs q) 
         (hash-ref (quad-attrs q) key default)
         default)]))

(define-syntax (quad-attr-ref/parameter stx)
  (syntax-case stx ()
    [(_ q key)
     (with-syntax ([world:key-default (format-id stx "~a-default" (string-trim (symbol->string (syntax->datum #'key)) "-key"))])
       #'(quad-attr-ref q key (world:key-default)))]))


(define (quad-has-attr? q key)
  (define qa (quad-attrs q))
  (and qa (hash-has-key? qa key)))

(define-syntax (define-quad-list-function stx)
  (syntax-case stx ()
    [(_ proc)
     (with-syntax ([quad-proc (format-id stx "quad-~a" #'proc)])
       #'(define (quad-proc q) (proc (quad-list q))))]))

(define-quad-list-function first)
(define-quad-list-function car)
(define-quad-list-function cdr)
(define-quad-list-function last)
(define (quad-cons item q)
  (quad (quad-name q) (quad-attrs q) (cons item (quad-list q))))

(define-syntax-rule (quad-ref q r)
  (list-ref (quad-list q) r))

(define/contract (quad-ends-with? q str)
  (quad? string? . -> . boolean?)
  (cond
    [(not (empty? (quad-list q)))
     (define last-item (last (quad-list q)))
     (cond
       [(string? last-item) (ends-with? last-item str)]
       [(quad? last-item) (quad-ends-with? last-item str)])]
    [else #f]))


(define/contract (quad-append q new-item)
  (quad? (or/c quad? string?) . -> . quad?)
  (quad (quad-name q) (quad-attrs q) (append (quad-list q) (list new-item))))

(define/contract (quad->string x)
  (quad? . -> . string?)
  (cond
    [(quad? x) (string-append* (map quad->string (quad-list x)))]
    [(string? x) x]
    [else ""]))

(define-syntax-rule (report-quadstring q)
  (begin
    (report (quad->string q) 'q)
    q))

(define cannot-be-common-attrs '(width x y page)) ;; todo: how to specify these better? this-* prefix?

;; make this a macro because qs-in is often huge
;; and the macro avoids allocation + garbage collection
(define attr-missing (gensym))
(define (gather-common-attrs qs)
  (let loop ([qs qs]
             [common-attrs (if (quad-attrs (car qs))
                               (for/list ([kv-pair (in-hash-pairs (quad-attrs (car qs)))] 
                                          #:unless (member (car kv-pair) cannot-be-common-attrs))  
                                 kv-pair)
                               empty)])
    (cond
      [(empty? common-attrs) #f]
      [(empty? qs) (flatten common-attrs)]
      [else (loop (cdr qs) 
                  (filter (λ(ca) (equal? (quad-attr-ref (car qs) (car ca) attr-missing) (cdr ca)))
                          common-attrs))])))


(define-syntax (define-box-type stx)
  (syntax-case stx ()
    [(_ id) 
     (with-syntax ([id? (format-id #'id "~a?" #'id)]
                   [ids? (format-id #'id "~as?" #'id)]
                   [lists-of-ids? (format-id #'id "list-of-~as?" #'id)]
                   [quads->id (format-id #'id "quads->~a" #'id)]
                   [inline/quads->id (format-id #'id "inline/quads->~a" #'id)])
       #'(begin
           ;; quad predicate - ok to be relaxed here if we're strict when making the struct
           (define (id? x)
             (and (quad? x) (equal? (quad-name x) 'id)))
           ;; quad constructor
           ;; put contract here rather than on struct, because this is the main interface
           ;; and this contract is more liberal.
           ;; but don't put a separate contract on struct, because it's superfluous.
           (define/contract (id [attrs #f] . xs)
             (() ((or/c quad-attrs? hashable-list?)) #:rest quad-list? . ->* . id?)
             (quad 'id (and attrs (if (hash? attrs) attrs (apply hash attrs))) xs))
           ;; quad list predicate and list-of-list predicate.
           ;; These are faster than the listof contract combinator.
           (define (ids? x)
             (and (list? x) (andmap id? x)))
           (define (lists-of-ids? x)
             (and (list? x) (andmap ids? x)))
           ;; quad converter macro
           (define (quads->id qs)
             (apply id (gather-common-attrs qs) qs))))]))


;; do not treat empty string as whitespace.
;; throws off tests that rely on adjacency to positive whitespace.
(define/contract (whitespace? x [nbsp? #f])
  ((any/c)(boolean?) . ->* . coerce/boolean?)
  (cond
    [(quad? x) (whitespace? (quad-list x) nbsp?)]
    [(string? x) (or (and (regexp-match #px"\\p{Zs}" x) ; Zs = unicode whitespace category
                          (or nbsp? (not (regexp-match #px"\u00a0" x)))))] ; 00a0: nbsp
    [(list? x) (and (not (empty? x)) (andmap (curryr whitespace? nbsp?) x))] ; andmap returns #t for empty lists
    [else #f]))

(define (whitespace/nbsp? x)
  (whitespace? x #t))


(define-syntax (define-break-type stx)
  (syntax-case stx ()
    [(_ id) 
     (with-syntax ([split-on-id-breaks (format-id #'id "split-on-~a-breaks" #'id)]
                   [id-break (format-id #'id "~a-break" #'id)]
                   [id-break? (format-id #'id "~a-break?" #'id)]
                   [multi-id (format-id #'id "multi~a" #'id)]
                   [multi-id? (format-id #'id "multi~a?" #'id)]
                   [quads->multi-id (format-id #'id "quads->multi~a" #'id)])
       #'(begin
           (define-box-type id)
           (define-box-type id-break)
           (define-box-type multi-id)
           ;; breaker
           (define/contract (split-on-id-breaks x)
             (quads? . -> . lists-of-quads?)
             ;; omit leading & trailing whitespace, because they're superfluous next to a break
             (map (curryr trimf whitespace?) (filter-split x id-break?)))))]))

(define-box-type box)

(define-break-type word)
(define (word-string c) (car (quad-list c)))

(define-box-type spacer)
(define-box-type kern)
(define-box-type optical-kern)
(define-box-type flag)
(define-box-type doc)
(define-box-type input)
(define-box-type piece)
(define-box-type run)

(define-break-type page)
(define-break-type column)
(define-break-type block)
(define-break-type line)

(define (->input q) (input #f q))
(define coerce/input? (make-coercion-contract input))

