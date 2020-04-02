#lang debug racket/base
(require racket/runtime-path
         racket/string
         racket/path
         racket/match
         fontland/font-path
         "attrs.rkt")
(provide (all-defined-out))

(define-runtime-path quadwriter-fonts-dir "fonts")
(define-runtime-path default-font-face "fonts/default/SourceSerifPro-Regular.otf")
(define default-font-family "text")
(define default-font-size 12)
(define default-line-height 16)
(define default-font-color "black")
(define default-font-features "tnum 1")

(define font-paths (make-hash))

(define top-font-directory "fonts")
(define font-file-extensions '(#".otf" #".ttf" #".woff"))

(define (setup-font-path-table! base-path)
  ;; create a table of font paths that we can use to resolve references to font names.
  
  ;; rules for font naming
  ;; "fonts" subdirectory on top
  ;; family directories inside: each named with font family name
  ;; this makes it possible to give font families generic names (e.g., "body-text")
  ;; and change the font files without disturbing anything else.
  (hash-clear! font-paths)
  (define doc-fonts-dir
    (simple-form-path
     (build-path (match/values (split-path base-path)
                               [(base name #true) (build-path base name)]
                               [(dir _ _) dir]) top-font-directory)))
  ;; run doc-fonts-dir first because earlier fonts take precedence (using hash-ref! below)
  (for* ([fonts-dir (in-list (list doc-fonts-dir quadwriter-fonts-dir))]
         #:when (directory-exists? fonts-dir)
         [font-family-subdir (in-directory fonts-dir)]
         #:when (directory-exists? font-family-subdir)
         [font-path (in-directory font-family-subdir)]
         #:when (member (path-get-extension font-path) font-file-extensions))
    (match-define (list font-path-string family-name)
      (for/list ([x (list font-path font-family-subdir)])
        (path->string (find-relative-path fonts-dir x))))
    (define path-parts (for/list ([part (in-list (explode-path (string->path (string-downcase font-path-string))))])
                         (path->string part)))
    (define key
      (cons (string-downcase family-name)
            (cond
              [(member "bold-italic" path-parts) 'bi]
              [(member "bold" path-parts) 'b]
              [(member "italic" path-parts) 'i]
              [else 'r])))
    ;; only set value if there's not one there already.
    ;; this means that we only use the first eligible font we find.
    (hash-ref! font-paths key font-path)))

(define (make-key font-family [bold #f] [italic #f])
  (cons (string-downcase font-family)
        (cond
          [(and bold italic) 'bi]
          [bold 'b]
          [italic 'i]
          [else 'r])))

(define (font-attrs->path font-family bold italic)
  ;; find the font-path corresponding to a certain family name and style.
  (define regular-key (make-key font-family))
  
  ;; if it's not already in font paths, it might be a system font
  ;; we use `family->path` to try to resolve the ribbi styles
  ;; if the font is a system font, we will end up with paths.
  ;; if it's not, we will end up with #false for those entries in `font-paths`,
  ;; and fall through to the default font when we do the `cond` below.
  (unless (hash-has-key? font-paths regular-key)
    (for* ([bold (in-list (list #false #true))]
           [italic (in-list (list #false #true))])
      (hash-set! font-paths
                 (make-key font-family bold italic)
                 (family->path font-family #:bold bold #:italic italic))))
  (cond
    [(hash-ref font-paths (make-key font-family bold italic) #false)]
    ;; try regular style if style-specific key isn't there for b i or bi
    [(or bold italic (hash-ref font-paths regular-key #false))]
    ;; otherwise use default
    [else default-font-face]))

(define (resolve-font-path! attrs)
  ;; convert references to a font family and style to an font path on disk
  ;; we trust it exists because we used `setup-font-path-table!` earlier,
  ;; but if not, fallback fonts will kick in, on the idea that a missing font shouldn't stop the show
  (define this-font-family (hash-ref! attrs :font-family default-font-family))
  (unless (complete-path? this-font-family)
    (define this-bold (hash-ref! attrs :font-bold #false))
    (define this-italic (hash-ref! attrs :font-italic #false))
    (hash-set! attrs :font-path (font-attrs->path this-font-family this-bold this-italic))))

(define (parse-em pstr)
  (define em-suffix "em")
  (and
   pstr
   (string? pstr)
   (string-suffix? pstr em-suffix)
   (string->number (string-trim pstr em-suffix))))

(define (resolve-font-size! attrs)
  ;; convert font-size attributes into a simple font size
  ;; we stashed the previous size in private key 'font-size-previous
  (define prev-font-size-key 'font-size-previous)
  (define val (parse-dimension (hash-ref attrs :font-size default-font-size)))
  (define adjustment (parse-em val))
  ;; if our value represents an adjustment, we apply the adjustment to the previous value
  ;; otherwise we use our value directly
  (define base-size (if adjustment (hash-ref attrs prev-font-size-key default-font-size) val))
  (define base-size-adjusted (and base-size (* base-size (or adjustment 1))))
  ;; we write our new value into both font-size and font-size-previous
  ;; because as we cascade down, we're likely to come across superseding values
  ;; of font-size (but font-size-previous will persist)
  (hash-set! attrs :font-size base-size-adjusted)
  (hash-set! attrs prev-font-size-key base-size-adjusted))
