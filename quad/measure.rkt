#lang quad/dev
(require "freetype-ffi.rkt")
(provide (all-defined-out))

(define (measure! q)
  (quad-dim-set! q
                 (cond
                   [(quad-printable? q)
                    (* (measure-char (quad-font q) (quad-val q)) (quad-font-size q))]
                   [else 0])))

(module+ test
  (require rackunit)
  (define q ($black '#hasheq((size . 12) (font . "sc.otf")) 0 #\n))
  (check-equal? (measure-char (quad-font q) (quad-val q)) .6))

(define measure-char
  (let ([measure-cache (make-hash)]
        [glyph-idx-cache (make-hash)]
        [glyph-width-cache (make-hash)]
        [em-size-cache (make-hash)]
        [ft-library (FT_Init_FreeType)]
        [ft-face-cache (make-hash)])
    (λ (font-pathstring char)
      (define (do-measure)
        (define ft-face (hash-ref! ft-face-cache font-pathstring
                                   (λ () (unless (file-exists? font-pathstring)
                                           (error 'measure-char (format "font path ~v does not exist" font-pathstring)))
                                     (FT_New_Face ft-library font-pathstring 0))))
        (define width
          (let ([glyph-idx (hash-ref! glyph-idx-cache (cons char font-pathstring)
                                      (λ () (FT_Get_Char_Index ft-face (char->integer char))))])
            (hash-ref! glyph-width-cache (cons glyph-idx font-pathstring)
                       (λ ()
                         (FT_Load_Glyph ft-face glyph-idx FT_LOAD_NO_RECURSE) ; loads into FTFace's 'glyph' slot
                         (define width (FT_Vector-x (FT_GlyphSlotRec-advance (FT_FaceRec-glyph ft-face))))
                         (* width 1.0))))) ; store as inexact
        (define em-size
          (hash-ref! em-size-cache font-pathstring (λ () (FT_FaceRec-units_per_EM ft-face))))
        (/ width em-size))
      (hash-ref! measure-cache (cons font-pathstring char) do-measure))))

(module+ test
  (require rackunit)
  (check-equal? (measure-char "charter.ttf" #\f) .321))
