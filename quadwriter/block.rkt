#lang debug racket
(require "attrs.rkt"
         "param.rkt"
         "log.rkt"
         "debug.rkt"
         "struct.rkt"
         "line.rkt"
         quad/quad
         quad/util
         quad/position
         pitfall)
(provide (all-defined-out))


(define ((block-draw-start first-line) q doc)
  ;; adjust drawing coordinates for border inset
  (match-define (list bil bit bir bib)
    (for/list ([k (in-list (list :border-inset-left :border-inset-top :border-inset-right :border-inset-bottom))])
              (quad-ref first-line k 0)))
  (match-define (list left top) (pt+ (quad-origin q) (list bil bit)))
  (match-define (list width height) (pt- (size q) (list (+ bil bir) (+ bit bib))))
  ;; fill rect
  (let ([bgc (quad-ref first-line :background-color)])
    (when bgc
      (rect doc left top width height)
      (fill doc bgc)))
  ;; draw border
  (match-define (list bw-left bw-top bw-right bw-bottom)
    (map (λ (k) (max 0  (quad-ref first-line k 0)))
         (list
          :border-width-left
          :border-width-top
          :border-width-right
          :border-width-bottom)))
  ;; adjust start and end points based on adjacent border width
  ;; so all borders overlap rectangularly
  (define (half x) (/ x 2.0))
  (define right (+ left width))
  (define bottom (+ top height))
  (define (box-side x1 y1 x2 y2 color stroke-width)
    (when (positive? stroke-width)
      (move-to doc x1 y1)
      (line-to doc x2 y2)
      (stroke doc (or color "black") stroke-width)))
  (box-side (- left (half bw-left)) top (+ right (half bw-right)) top
            (quad-ref first-line :border-color-top) bw-top)
  (box-side right (- top (half bw-top)) right (+ bottom (half bw-bottom))
            (quad-ref first-line :border-color-right) bw-right)
  (box-side (+ right (half bw-right)) bottom (- left (half bw-left)) bottom
            (quad-ref first-line :border-color-bottom) bw-bottom)
  (box-side left (+ bottom (half bw-bottom)) left (- top (half bw-top))
            (quad-ref first-line :border-color-left) bw-left)
  (case (quad-ref first-line :block-clip)
    [(#true)
     (when (eq? (log-clipping?) 'warn)
       (for ([line (in-list (quad-elems q))])
            (define line-width (pt-x (size line)))
            (define line-elem-width (sum-x (quad-elems line)))
            (when (< line-width line-elem-width)
              (define error-str (apply string-append (for/list ([q (in-list (quad-elems line))])
                                                               (match (quad-elems q)
                                                                 [(list (? string? str)) str]
                                                                 [_ ""]))))
              (log-quadwriter-warning (format "clipping overfull line: ~v" error-str)))))
     (save doc)
     (rect doc left top width height)
     (clip doc)]))

(define ((block-draw-end first-line) q doc)
  (case (quad-ref first-line :block-clip)
    [(#true) (restore doc)])
  (when (draw-debug-block?)
    (draw-debug q doc "#6c6" "#9c9")))

(define (insert-blocks lines)
  (define groups-of-lines (contiguous-group-by (λ (x) (quad-ref x :display)) lines))
  (append* (for/list ([line-group (in-list groups-of-lines)])
                     (if (quad-ref (car line-group) :display)
                         (list (lines->block line-group))
                         line-group))))

(define (lines->block lines)
  (match lines
    [(cons line _)
     (make-quad
      #:type block-quad
      #:from 'sw
      #:to 'nw
      #:elems (from-parent lines 'nw)
      #:id 'block
      #:attrs (quad-attrs line)
      #:size (delay (pt (pt-x (size line)) ; 
                        (+ (sum-y lines)
                           (quad-ref line :inset-top 0)
                           (quad-ref line :inset-bottom 0))))
      #:shift-elems (pt 0 (quad-ref line :inset-top 0))
      #:draw-start (block-draw-start line)
      #:draw-end (block-draw-end line))]))