#lang quad/dev
(require "freetype-ffi.rkt")
(provide (all-defined-out))

(define measure-char
  (let ([measure-cache (make-hash)]
        [glyph-idx-cache (make-hash)]
        [glyph-width-cache (make-hash)]
        [em-size-cache (make-hash)])
    (λ (font-pathstring char)
      (define (do-measure)
        (define ft-library (FT_Init_FreeType))
        (define ft-face (FT_New_Face ft-library font-pathstring 0))
        (define prev-glyph-idx #f)
        (define sum
          (let ([glyph-idx (hash-ref! glyph-idx-cache (cons char font-pathstring)
                                      (λ () (FT_Get_Char_Index ft-face (char->integer char))))])
            (hash-ref! glyph-width-cache (cons glyph-idx font-pathstring)
                       (λ ()
                         (FT_Load_Glyph ft-face glyph-idx FT_LOAD_NO_RECURSE) ; loads into FTFace's 'glyph' slot
                         (FT_Vector-x (FT_GlyphSlotRec-advance (FT_FaceRec-glyph ft-face)))))))
        (define em-size
          (hash-ref! em-size-cache font-pathstring (λ () (+ (FT_FaceRec-units_per_EM ft-face) 0.0))))
        ; will anything bad happen if I skip these?
        (FT_Done_Face ft-face)
        (FT_Done_FreeType ft-library) 
        (/ sum em-size)) ; normalize to em size      
      (hash-ref! measure-cache (cons font-pathstring char) do-measure))))

(define measure-chars
  (let ([measure-cache (make-hash)]
        [glyph-idx-cache (make-hash)]
        [glyph-width-cache (make-hash)]
        [kern-cache (make-hash)])
    (λ (font-pathstring chars)
      (define (do-measure)
        (define ft-library (FT_Init_FreeType))
        (define ft-face (FT_New_Face ft-library font-pathstring 0))
        (define prev-glyph-idx #f)
        (define sum
          (for/sum ([char (in-list chars)])
                   (define glyph-idx (hash-ref! glyph-idx-cache (cons char font-pathstring)
                                                (λ () (FT_Get_Char_Index ft-face (char->integer char)))))
                   (define glyph-width (hash-ref! glyph-width-cache (cons glyph-idx font-pathstring)
                                                  (λ ()
                                                    (FT_Load_Glyph ft-face glyph-idx FT_LOAD_NO_RECURSE) ; loads into FTFace's 'glyph' slot
                                                    (FT_Vector-x (FT_GlyphSlotRec-advance (FT_FaceRec-glyph ft-face))))))
                   (define kern (if prev-glyph-idx
                                    (hash-ref! kern-cache (cons prev-glyph-idx (cons glyph-idx font-pathstring))
                                               (λ ()
                                                 (FT_Vector-x (FT_Get_Kerning ft-face prev-glyph-idx glyph-idx FT_KERNING_UNSCALED))))
                                    0))
                   (set! prev-glyph-idx glyph-idx)
                   (+ glyph-width kern)))
        ; will anything bad happen if I skip these?
        (FT_Done_Face ft-face)
        (FT_Done_FreeType ft-library) 
        sum)  
      (hash-ref! measure-cache (cons font-pathstring chars) do-measure))))

(module+ test
  (require rackunit)
  (check-equal? (measure-chars "charter.ttf" '(#\f)) 321))

