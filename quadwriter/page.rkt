#lang debug racket
(require "struct.rkt"
         "attrs.rkt"
         "param.rkt"
         "debug.rkt"
         "font.rkt"
         "string.rkt"
         quad/base
         racket/date
         pitfall)
(provide (all-defined-out))



(define default-page-size "letter")
(define default-page-orientation "tall")
(define (page-draw-start q doc)
  (match-define (list page-width page-height) (parse-page-size q))
  (add-page doc page-width page-height)
  (scale doc (zoom-factor) (zoom-factor))
  (draw-debug q doc "purple" "purple" 1))


(define q:page (make-quad
                #:type page-quad
                #:tag 'page
                #:from-parent 'nw
                #:draw-start page-draw-start))


(define (parse-page-size q)
  ;; page size can be specified by name, or measurements.
  ;; explicit measurements from page-height and page-width supersede those from page-size.
  (match-define (list page-width page-height)
    (for/list ([k (list :page-width :page-height)])
              (and (quad? q) (match (quad-ref q k)
                               [#false #false]
                               [val (inexact->exact (floor val))]))))
  (resolve-page-size
   (or (debug-page-width) page-width)
   (or (debug-page-height) page-height)
   (quad-ref q :page-size default-page-size)
   (quad-ref q :page-orientation default-page-orientation)))


(define (draw-page-footer q doc)
  (match-define (list x y) (quad-origin q))
  (font-size doc (quad-ref q :font-size))
  (font doc (path->string (quad-ref q font-path-key default-font-face)))
  (fill-color doc default-font-color)
  (define str (or (quad-ref q :footer-text)
                  (format "~a · ~a at ~a" (quad-ref q :page-number 0)
                          (if (quadwriter-test-mode) "test" (quad-ref q :doc-title "untitled"))
                          (date->string (if (quadwriter-test-mode) (seconds->date 0 #f) (current-date)) #t))))
  (text doc str x y)
  #;(set-quad-size! q (make-size-promise-for-string q str)))

(define (make-footer-quad col-q page-idx path)
  (define-values (dir name _) (split-path (path-replace-extension path #"")))
  (define attrs (let ([attrs (make-hasheq)])
                  (hash-set*! attrs
                              :footer-text (quad-ref col-q :footer-text)
                              :page-number (+ (quad-ref col-q :page-number-start (add1 (section-pages-used))) (sub1 page-idx))
                              :doc-title (string-titlecase (path->string name))
                              :font-size (* 0.8 (quad-ref col-q :font-size default-font-size))
                              :line-height (quad-ref col-q :line-height default-line-height)
                              :font-family "text")
                  (resolve-font-path! attrs)
                  attrs))
  (make-quad #:size (pt 50 default-line-height)
             #:attrs attrs
             #:from-parent 'sw
             #:to 'nw
             #:elems null
             #:shift (pt 0 (* 1.5 default-line-height))
             #:printable #true
             #:draw draw-page-footer
             #:draw-end (λ (q doc)
                          (when draw-debug-line?
                            (draw-debug q doc "goldenrod" "goldenrod")))))

(define ((page-wrap-finish make-page-quad path) cols q-before q-after page-idx)
  (define pq (make-page-quad (+ (section-pages-used) page-idx)))
  ;; get attrs from cols if we can, otherwise try q-after or q-before
  (define q-for-attrs (cond
                        [(pair? cols) (car cols)]
                        [q-after]
                        [q-before]
                        [else (raise-argument-error 'page-wrap-finish "quad with attrs" (list cols q-after q-before))]))
  (define elems
    (append
     (match (quad-ref q-for-attrs :footer-display #true)
       [(or #false "none") null]
       [_ (list (make-footer-quad q-for-attrs page-idx path))])
     (from-parent cols 'nw)))
  (list (quad-update! pq
                      [elems elems]
                      [attrs (copy-block-attrs (cond
                                                 [q-for-attrs => quad-attrs]
                                                 [else (hash)])
                                               (hash-copy (quad-attrs pq)))])))

(define (page-wrap qs width [make-page-quad (λ (x) (quad-copy page-quad q:page))])
  (unless (positive? width)
    (raise-argument-error 'page-wrap "positive number" width))
  (wrap qs width
        #:soft-break #true
        #:hard-break page-break-quad?
        #:no-break (λ (q) (quad-ref q :no-pbr))
        #:distance (λ (q dist-so-far wrap-qs) (sum-x wrap-qs))
        #:finish-wrap (page-wrap-finish make-page-quad (pdf-output-path (current-pdf)))))