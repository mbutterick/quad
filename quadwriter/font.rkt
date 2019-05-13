#lang debug racket/base
(require racket/runtime-path
         racket/string
         racket/path
         racket/match)
(provide (all-defined-out))

(define-runtime-path quadwriter-fonts-dir "fonts")
(define-runtime-path default-font-face "fonts/default/SourceSerifPro-Regular.otf")
(define default-font-family "default-serif")
(define default-font-size 12)
(define default-line-height 16)
(define default-font-color "black")

(define font-paths (make-hash))

(define (setup-font-path-table! base-path)
  ;; populate `font-paths` table with font paths
  ;; search "fonts" subdirectory in project for other subdirectories
  ;; which are presumed to contain fonts.
  ;; and link them to their family names & styles.
  ;; this allows a flexible mapping from internal to external names, like @font-face
  ;; note that all the semantics are derived from the file system
  ;; not any metadata fields within the font.
  ;; this is faster and easier, because you can just muck with the directory and filenames
  ;; to change the font mapping.
  ;; though it also creates the potential for mischief,
  ;; if a font is named something that doesn't reflect its visual reality.
  ;; but we are not the font police.

  ;; rules for font naming
  ;; "fonts" subdirectory on top
  ;; family directories inside: each named with font family name
  ;; this makes it possible to give font families generic names (e.g., "body-text")
  ;; and change the font files without disturbing anything else.
  (hash-clear! font-paths)
  (define-values (dir path _) (split-path base-path))
  (define doc-fonts-dir (build-path dir "fonts"))
  ;; run doc-fonts-dir first because earlier fonts take precedence
  (for* ([fonts-dir (in-list (list quadwriter-fonts-dir doc-fonts-dir ))]
         #:when (directory-exists? fonts-dir)
         [font-family-subdir (in-directory fonts-dir)]
         #:when (directory-exists? font-family-subdir)
         [font-path (in-directory font-family-subdir)]
         #:when (member (path-get-extension font-path) '(#".otf" #".ttf")))
    (match-define (list font-path-string family-name)
      (map (λ (x) (path->string (find-relative-path fonts-dir x))) (list font-path font-family-subdir)))
    ;; search for subdir in path matching style name
    ;; note that this will work if fonts are contained in another subdirectory (e.g., real font name)
    (define path-parts (map path->string (explode-path (string->path (string-downcase font-path-string)))))
    (define key
      (cons family-name
            (cond
              [(member "bold-italic" path-parts) 'bi]
              [(member "bold" path-parts) 'b]
              [(member "italic" path-parts) 'i]
              [else 'r])))
    ;; only set value if there's not one there already.
    ;; this means that we only use the first eligible font we find.
    (hash-ref! font-paths key font-path)))

(define (font-attrs->path font-family bold italic)
  ;; find the font-path corresponding to a certain family name and style.
  (define key (cons font-family
                    (cond
                      [(and bold italic) 'bi]
                      [bold 'b]
                      [italic 'i]
                      [else 'r])))
  (define regular-key (cons font-family 'r))
  (cond
    [(hash-ref font-paths key #false)]
    ;; if there isn't one, try the regular style.
    [(hash-ref font-paths regular-key #false)]
    ;; If there isn't one, use the default.
    [else default-font-face]))

(define (resolve-font-path attrs)
  (define this-font-family (hash-ref! attrs 'font-family default-font-family))
  (unless (complete-path? this-font-family)
    (define this-bold (hash-ref! attrs 'font-bold #false))
    (define this-italic (hash-ref! attrs 'font-italic #false))
    (hash-set! attrs 'font-path (font-attrs->path this-font-family this-bold this-italic))))

(define (parse-percentage pstr)
  (/ (string->number (string-trim pstr "%")) 100.0))

(define (resolve-font-size attrs)
  (define this-font-size (hash-ref! attrs 'font-size default-font-size))
  (define this-font-size-adjust (parse-percentage (hash-ref! attrs 'font-size-adjust "100%")))
  ;; we bake the adjustment into the font size...
  (hash-set! attrs 'font-size (* this-font-size this-font-size-adjust))
  ;; and then set the adjustment back to 100% (since it's now accounted for)
  (hash-set! attrs 'font-size-adjust "100%"))
