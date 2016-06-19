#lang racket/base
(require ffi/unsafe
         ffi/unsafe/define
         racket/draw/private/libs)

(define-syntax-rule (define+provide id val)
  (begin
    (define id val)
    (provide id)))

(define-runtime-lib freetype-lib
  [(unix) #f] ; todo: get unix runtime path
  [(macosx) (ffi-lib "libfreetype.6.dylib")]
  [(windows) (ffi-lib "libfreetype-6.dll")])

(define-ffi-definer define-freetype freetype-lib #:provide provide)

;; types
(define _void-pointer (_cpointer 'void-pointer))
(define _char _byte)
(define _char-pointer (_cpointer 'char-pointer))
(define _uchar _ubyte)
(define _FT_Byte _ubyte)
(define _FT_Bytes _bytes)
(define _FT_Char _char)
(define _FT_Int _int)
(define _FT_UInt _uint)
(define _FT_Int16 _short)
(define _FT_UInt16 _ushort)
(define _FT_Int32 _int32)
(define _FT_UInt32 _uint32)
(define _FT_Short _short)
(define _FT_UShort _ushort)
(define _FT_Long _long)
(define _FT_ULong _ulong)
(define _FT_Bool _byte)
(define _FT_Offset _size) ;; equivalent to _size_t?
(define _FT_PtrDist _ptrdiff) ;; equivalent to _longlong?
(define _FT_String _char) 
(define _FT_String-pointer (_cpointer 'FT_String-pointer)) ;; char*
(define _FT_Tag _FT_UInt32)
(define _FT_Error _int)
(define _FT_Fixed _long) 
(define _FT_Pointer _void-pointer)
(define _FT_Pos _long)
(define _FT_FWord _short)
(define _FT_UFWord _ushort)
(define _FT_F26Dot16 _short)
(define _FT_F26Dot6 _long)
(define _FT_Glyph_Format _int)
(define _FT_Encoding _int)
(define _FT_Generic_Finalizer (_cpointer '_FT_Generic_Finalizer (_fun _void-pointer -> _void)))

(define _FT_LibraryRec (_cpointer 'FT_LibraryRec))
(define _FT_Library (_cpointer 'FT_Library))

(define-cstruct _FT_Bitmap_Size
  ([height _FT_Short]
   [width _FT_Short]
   [size _FT_Pos]
   [x_ppem _FT_Pos]
   [y_ppem _FT_Pos]))

(define-cstruct _FT_CharMapRec
  ([face _void-pointer] ; should be FT_Face
   [encoding _FT_Encoding]
   [platform_id _FT_UShort]
   [encoding_id _FT_UShort]))

(define _FT_Charmap _FT_CharMapRec-pointer)
(define _FT_CharMap-pointer (_cpointer 'FT_CharMap-pointer))

(define-cstruct _FT_Generic
  ([data _void-pointer]
   [finalizer _FT_Generic_Finalizer]))

(define-cstruct _FT_BBox
  ([xMin _FT_Pos]
   [yMin _FT_Pos]
   [xMax _FT_Pos]
   [yMax _FT_Pos]))

(define-cstruct _FT_Glyph_Metrics
  ([width _FT_Pos]
   [height _FT_Pos]
   [horiBearingX _FT_Pos]
   [horiBearingY _FT_Pos]
   [horiAdvance _FT_Pos]
   [vertBearingX _FT_Pos]
   [vertBearingY _FT_Pos]
   [vertAdvance _FT_Pos]))

(define-cstruct _FT_Vector
  ([x _FT_Pos]
   [y _FT_Pos]))

(provide (struct-out FT_Vector)
         _FT_Vector _FT_Vector-pointer)

(define-cstruct _FT_Bitmap
  ([rows _int]
   [width _int]
   [pitch _int]
   [buffer (_cpointer 'buffer)]
   [num_grays _short]
   [pixel_mode _ubyte]
   [palette_mode _char]
   [palette _void-pointer]))

(define-cstruct _FT_Outline
  ([n_contours _short]
   [n_points _short]
   [points _FT_Vector-pointer]
   [tags (_cpointer 'tags)]
   [contours (_cpointer 'contours)]
   [flags _int]))

(define-cstruct _FT_GlyphSlotRec
  ([library           _FT_Library]
   [face              _void-pointer]
   [next              _void-pointer]
   [reserved          _uint]
   [generic           _FT_Generic]
   [metrics           _FT_Glyph_Metrics]
   [linearHoriAdvance _FT_Fixed]
   [linearVertAdvance _FT_Fixed]
   [advance           _FT_Vector]
   [format            _FT_Glyph_Format]
   [bitmap            _FT_Bitmap]
   [bitmap_left       _FT_Int]
   [bitmap_top        _FT_Int]
   [outline           _FT_Outline]
   [num_subglyphs     _FT_UInt]
   [subglyphs         _void-pointer]
   [control_data      _void-pointer]
   [control_len       _long]
   [lsb_delta         _FT_Pos]
   [rsb_delta         _FT_Pos]
   [other             _void-pointer]
   [internal          _void-pointer]))

(define _FT_GlyphSlot _FT_GlyphSlotRec-pointer)

(provide (struct-out FT_GlyphSlotRec)
         _FT_GlyphSlotRec _FT_GlyphSlotRec-pointer)

(define-cstruct _FT_Size_Metrics
  ([x_ppem _FT_UShort]
   [y_ppem _FT_UShort]
   [x_scale _FT_Fixed]
   [y_scale _FT_Fixed]
   [ascender _FT_Pos]
   [descender _FT_Pos]
   [height _FT_Pos]
   [max_advance _FT_Pos]))

(define-cstruct _FT_SizeRec
  ([face _void-pointer]
   [generic _FT_Generic]
   [metrics _FT_Size_Metrics]
   [internal _void-pointer]))

(define _FT_Size _FT_SizeRec-pointer)

(define-cstruct _FT_FaceRec
  ([num_faces _FT_Long]
   [face_index _FT_Long]
   [face_flag _FT_Long]
   [style_flags _FT_Long]
   [num_glyphs _FT_Long]
   [family_name _string] ; probably _string is a better choice
   [style_name _string]
   [num_fixed_sizes _FT_Int]
   [available_sizes _FT_Bitmap_Size-pointer]
   [num_charmaps _FT_Int]
   [charmaps _FT_CharMap-pointer]
   [generic _FT_Generic]
   [bbox _FT_BBox]
   [units_per_EM _FT_UShort]
   [ascender _FT_Short]
   [descender _FT_Short]
   [height _FT_Short]
   [max_advance_width _FT_Short]
   [max_advance_height _FT_Short]
   [underline_position _FT_Short]
   [underline_thickness _FT_Short]
   [glyph _FT_GlyphSlot]
   [size _FT_Size]
   [charmap _FT_Charmap]
   [driver _void-pointer]
   [memory _void-pointer]
   [stream _void-pointer]
   [sizes_list_head _void-pointer]
   [sizes_list_tail _void-pointer]
   [autohint _FT_Generic]
   [extensions _void-pointer]
   [internal _void-pointer]))

(define _FT_Face _FT_FaceRec-pointer)
(provide (struct-out FT_FaceRec)
         _FT_FaceRec _FT_FaceRec-pointer)

(define _full-path
  (make-ctype _path
              path->complete-path
              values))

(define-freetype FT_Init_FreeType (_fun (ftl : (_ptr o _FT_Library))
                                        -> (err : _FT_Error) 
                                        -> (if (zero? err) ftl (error 'FT_Init_FreeType))))

(define-freetype FT_New_Face (_fun _FT_Library _full-path _FT_Long 
                                   (ftf : (_ptr o (_or-null _FT_Face)))
                                   -> (err : _FT_Error)
                                   -> (if (zero? err) ftf (error 'FT_New_Face (format "error ~a" err)))))

(define-freetype FT_Done_Face (_fun _FT_Face
                                   -> (err : _FT_Error)
                                   -> (unless (zero? err) (error 'FT_Done_Face (format "error ~a" err)))))

(define-freetype FT_Done_FreeType (_fun _FT_Library -> (err : _FT_Error) -> (if (zero? err) (void) (error 'FT_Done_FreeType))))

(define-freetype FT_Get_Kerning (_fun _FT_Face _FT_UInt _FT_UInt _FT_UInt
                                      (ftv : (_ptr o _FT_Vector))
                                      -> (err : _FT_Error)
                                      -> (if (zero? err) ftv (error 'FT_Get_Kerning (format "error ~a" err)))))

(define-freetype FT_Get_Char_Index (_fun _FT_Face _FT_ULong
                                         -> _FT_UInt))

(define-freetype FT_Load_Glyph (_fun _FT_Face _FT_UInt _FT_Int32
                                     -> (err : _FT_Error)))

(define-freetype FT_Load_Char (_fun _FT_Face _FT_ULong _FT_Int32
                                    -> (err : _FT_Error)))

(define+provide FT_KERNING_UNSCALED 2)
(define+provide FT_LOAD_DEFAULT 0)
(define+provide FT_LOAD_RENDER (expt 2 2))
(define+provide FT_LOAD_LINEAR_DESIGN (expt 2 13))
(define+provide FT_LOAD_NO_RECURSE (expt 2 10))


         
