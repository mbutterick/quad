#lang typed/racket/base
(require (for-syntax typed/racket/base racket/syntax))

;; struct implementation

(struct: quad ([name : Symbol] [attrs : (HashTable Symbol Any)] [list : (Listof quad)]) #:transparent
  
  #:property prop:sequence (λ(q) (quad-list q)))



(define h (quad 'hello #hash((foo . bar)) (list (quad 'hello #hash((foo . bar)) '()))))

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
           (provide id?)
           (: id? (Any . -> . Boolean))
           (define (id? x)
             (and (quad? x) (equal? (quad-name x) 'id)))
           
           ;; quad constructor
           ;; put contract here rather than on struct, because this is the main interface
           ;; and this contract is more liberal.
           ;; but don't put a separate contract on struct, because it's superfluous.
           (define/contract (id [attrs #f] . xs)
             (() ((or/c quad-attrs? hashable-list?)) #:rest quad-list? . ->* . id?)
             (quad 'id (and attrs (if (hash? attrs) attrs (apply hash attrs))) xs))))]))


(define-box-type hello)
(define-box-type gbye)


#|
;; make this a macro because qs-in is often huge
;; and the macro avoids allocation + garbage collection
(define attr-missing (gensym))
(: gather-common-attrs ((Listof quad) . -> . (Listof CommonAttr)))
(define (gather-common-attrs qs)
  (let loop : (Listof Commonattr) 
    ([qs : (Listof quad) qs]
             [common-attrs : (Listof Commonattr) (if (quad-attrs (car qs))
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
|#

