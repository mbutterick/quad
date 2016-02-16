#lang racket/base
(require ffi/unsafe
         ffi/unsafe/define
         ffi/unsafe/alloc
         setup/dirs
         racket/draw/private/libs
         racket/draw/private/utils)

(define-runtime-lib fontconfig-lib
  [(unix) (ffi-lib "libfontconfig" '("1" ""))]
  [(macosx)
   (ffi-lib "libpng16.16.dylib")
   (ffi-lib "libexpat.1.dylib")
   (ffi-lib "libfreetype.6.dylib")
   (ffi-lib "libfontconfig.1.dylib")]
  [(windows)
   (ffi-lib "zlib1.dll")
   (ffi-lib "libintl-8.dll")
   (ffi-lib "libpng16-16.dll")
   (ffi-lib "libexpat-1.dll")
   (ffi-lib "libfreetype-6.dll")
   (ffi-lib "libfontconfig-1.dll")])

(define-syntax-rule (_pfun spec ...)
  (_fun #:in-original-place? #t spec ...))

(define-ffi-definer define-fc fontconfig-lib
  #:provide provide)


;; datatype information from
;; http://www.freedesktop.org/software/fontconfig/fontconfig-devel/x31.html
(define FcConfig (_cpointer 'FcConfig))
(define FcPattern (_cpointer 'FcPattern))
(define FcPattern-pointer (_cpointer FcPattern))
(define FcObjectSet (_cpointer 'FcObjectSet))

(define FcBool _bool)
(define FcChar8 _bytes)

(define-cstruct _FcFontSet
  ([nfont _int]
   [sfont _int]
   [fonts FcPattern-pointer])) ;; ?? spec says "FcPattern **fonts" but I don't know how this translates to ffi


;; function information from
;; http://www.freedesktop.org/software/fontconfig/fontconfig-devel/x102.html
(define-fc FcGetVersion (_pfun -> _int))
(define-fc FcConfigCreate (_pfun -> FcConfig))
(define-fc FcInitLoadConfig (_pfun -> FcConfig))
(define-fc FcConfigAppFontAddFile (_pfun FcConfig FcChar8 -> FcBool))
(define-fc FcConfigHome (_pfun -> FcChar8))
(define-fc FcConfigGetSysRoot(_pfun FcConfig -> FcChar8))
(define-fc FcFontList(_pfun FcConfig FcPattern FcObjectSet -> _FcFontSet))
(define-fc FcPatternCreate (_pfun -> FcPattern))
(define-fc FcFontSetCreate (_pfun -> _FcFontSet))
(define-fc FcObjectSetCreate (_pfun -> FcObjectSet))
(define-fc FcPatternPrint (_pfun FcPattern -> _void))
(define-fc FcPatternEqual (_pfun FcPattern FcPattern -> FcBool))

;; attempting to replicate font-loading workaround shown at
;; https://bugzilla.gnome.org/show_bug.cgi?id=347237#c25
(define cfg (FcConfigCreate)) ; workaround step 1
(define path (string->bytes/utf-8 "/Users/MB/Desktop/reporter.otf"))

(FcConfigAppFontAddFile cfg path) ; workaround step 2
(define fcp (FcPatternCreate))
(define fcos (FcObjectSetCreate))
 
(define fs (FcFontList cfg fcp fcos))
(define pat (FcFontSet-fonts fs))

;; this crashes DrRacket, prob because I have mangled the _FcFontSet definition
;; (FcPatternPrint pat) 
