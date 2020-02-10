#lang debug racket
(require "attrs.rkt"
         "struct.rkt"
         "block.rkt"
         quad/base
         quad/wrap)
(provide (all-defined-out))

(define q:column (make-quad
                  #:type column-quad
                  #:id 'col
                  #:from 'ne
                  #:to 'nw))

(define q:column-spacer (make-quad
                         #:type column-spacer-quad
                         #:from 'ne
                         #:to 'nw
                         #:printable only-prints-in-middle))

(define ((column-wrap-finish col-quad) lns q0 ending-q idx [reversed-fn-lines null])
  (define fn-lines
    (from-parent (for/list ([fn-line (in-list reversed-fn-lines)])
                           ;; position bottom to top, in reverse
                           (quad-update! fn-line
                                         [from 'nw]
                                         [to 'sw])) 'sw))

  (append
   (match lns
     [(cons line _)
      (list (quad-copy column-quad col-quad
                       ;; move block attrs up, so they are visible in page wrap
                       [attrs (copy-block-attrs (quad-attrs line)
                                                (hash-copy (quad-attrs col-quad)))]
                       [elems (append (from-parent (insert-blocks lns) 'nw) fn-lines)]))]
     [_ null])
   (match ending-q
     [(? page-break-quad? page-break) (list page-break)] ; hard page (or section) break
     [_ null])))

#|
constraint wrapping example
https://github.com/mbutterick/typesetter/blob/882ec681ad1fa6eaee6287e53bc4320d9656046b/pdf/directory-require.rkt#L51
|#
;;


(define (footnote-start? fnq) (quad-ref fnq :fn-text-start))

(define (handle-leftover-footnote ymax leftover-qs fn-qs)
  (let loop ([ymax ymax][leftover-qs leftover-qs][fn-qs fn-qs])
    (define ydist (and (pair? fn-qs) (pt-y (size (car fn-qs)))))
    ;; take all fn lines that are not footnote-start?
    ;; and that fit within ymax remaining
    (if (and ydist (not (footnote-start? (car fn-qs))) (<= ydist ymax))
        (loop (- ymax ydist) (cons (car fn-qs) leftover-qs) (cdr fn-qs))
        (values ymax leftover-qs fn-qs))))

(define (handle-new-footnote ymax leftover-qs fn-qs fn-ref-q)
  (define ydist-fn (and (pair? fn-qs)
                        (footnote-start? (car fn-qs))
                        (pt-y (size (car fn-qs)))))
  (define ydist-ref (pt-y (size fn-ref-q)))
  ;; only accept the footnote if both the first line of footnote
  ;; and the line containing the ref will fit.
  (if (and ydist-fn (<= (+ ydist-fn ydist-ref) ymax))
      (values (- ymax ydist-fn) (cons (car fn-qs) leftover-qs) (cdr fn-qs))
      (raise 'boom)))

(define (column-wrap lines fn-lines vertical-height column-gap [col-quad-proto q:column])
  (unless (positive? vertical-height)
    (raise-argument-error 'column-wrap "positive number" vertical-height))
  
  ;; on timing of `insert-blocks`:
  ;; can't do it before because it depends on where columns are broken.
  ;; could do it after, but it would require going back inside each col quad
  ;; which seems overly interdependent, because `insert-blocks` is used to determine break locations.
  ;; `col-wrap` should emit quads that are complete.
  (define cols (wrap lines vertical-height
                     #:soft-break #true
                     #:hard-break column-break-quad?
                     #:no-break (λ (q) (quad-ref q :no-colbr)) ; cooperates with make-nobreak
                     #:distance (λ (q dist-so-far wrap-qs)
                                  ;; do trial block insertions
                                  (sum-y (insert-blocks (reverse wrap-qs))))                     
                     #:finish-wrap (column-wrap-finish col-quad-proto)
                     #:footnote-qs fn-lines
                     #:footnote-leftover-proc handle-leftover-footnote
                     #:footnote-new-proc handle-new-footnote))
  (define reversed-fn-lines
    (from-parent (for/list ([fn-line (in-list (reverse fn-lines))])
                           ;; position bottom to top, in reverse
                           (quad-update! fn-line
                                         [from 'nw]
                                         [to 'sw])) 'sw))
  (when (pair? cols)
    (quad-update! (car cols)
                  [elems (append (quad-elems (car cols)) reversed-fn-lines)]))
  (define col-spacer (quad-copy column-spacer-quad q:column-spacer
                                [size (pt column-gap (and 'arbitrary-irrelevant-value 100))]))
  (add-between cols col-spacer))
