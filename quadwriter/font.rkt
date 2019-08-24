#lang debug racket/base
(require racket/runtime-path
         racket/string
         racket/path
         racket/match
         "attrs.rkt")
(provide (all-defined-out))

(define-runtime-path quadwriter-fonts-dir "fonts")
(define-runtime-path default-font-face "fonts/default/SourceSerifPro-Regular.otf")
(define default-font-family "text")
(define default-font-size 12)
(define default-line-height 16)
(define default-font-color "black")
(define default-font-features (list (cons #"tnum" 1)))

(define font-paths (make-hash))

(define top-font-directory "fonts")
(define font-file-extensions '(#".otf" #".ttf" #".woff"))

(define (setup-font-path-table! base-path)
  ;; rules for font naming
  ;; "fonts" subdirectory on top
  ;; family directories inside: each named with font family name
  ;; this makes it possible to give font families generic names (e.g., "body-text")
  ;; and change the font files without disturbing anything else.
  (hash-clear! font-paths)
  (define-values (dir path _) (split-path base-path))
  (define doc-fonts-dir (build-path dir top-font-directory))
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

(define (font-attrs->path font-family bold italic)
  ;; find the font-path corresponding to a certain family name and style.
  (define key (cons (string-downcase font-family)
                    (cond
                      [(and bold italic) 'bi]
                      [bold 'b]
                      [italic 'i]
                      [else 'r])))
  (define regular-key (cons font-family 'r))
  (cond
    [(hash-ref font-paths key #false)]
    [(hash-ref font-paths regular-key #false)]
    [else default-font-face]))

(define (resolve-font-path! attrs)
  (define this-font-family (hash-ref! attrs :font-family default-font-family))
  (unless (complete-path? this-font-family)
    (define this-bold (hash-ref! attrs :font-bold #false))
    (define this-italic (hash-ref! attrs :font-italic #false))
    (hash-set! attrs :font-path (font-attrs->path this-font-family this-bold this-italic))))

(define (parse-percentage pstr)
  (/ (string->number (string-trim pstr "%")) 100.0))

(define (adjuster-base attrs key adjustment-key default-value)
  (define this-val (hash-ref! attrs key default-value))
  (define this-val-adjust (parse-percentage (hash-ref! attrs adjustment-key "100%")))
  ;; we bake the adjustment into the val...
  (hash-set! attrs key (and this-val (* this-val this-val-adjust)))
  ;; and then set the adjustment back to 100% (since it's now accounted for)
  (hash-set! attrs adjustment-key "100%"))

(define (resolve-font-size! attrs)
  (adjuster-base attrs :font-size :font-size-adjust default-font-size))

(define (resolve-line-height! attrs)
  (adjuster-base attrs :line-height :line-height-adjust default-line-height))
