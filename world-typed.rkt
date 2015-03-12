#lang typed/racket/base
(require (for-syntax racket/base racket/syntax))
(provide (prefix-out world: (all-defined-out)))

(define-syntax-rule (define-parameter name val)
  (define name (make-parameter val)))

(define-syntax (define-key-and-parameter stx)
  (syntax-case stx ()
    [(_ name keyname val)
     (with-syntax ([name-key (format-id #'name "~a-key" #'name)]
                   [name-default (format-id #'name "~a-default" #'name)])
       #'(begin
           (define name-key keyname)
           (define-parameter name-default val)))]))

(define-key-and-parameter measure 'measure 300.0)


(define-key-and-parameter font-size 'size 13)
(define-key-and-parameter font-name 'font "Triplicate T4")
(define-key-and-parameter font-weight 'weight 'normal)
(define-key-and-parameter font-style 'style 'normal)
(define-key-and-parameter font-color 'color "black")
(define-key-and-parameter font-background 'background "none")

(define-key-and-parameter column-count 'column-count 2)
(define-key-and-parameter column-gutter 'column-gutter 30.0)


(define max-quality 100)
(define adaptive-quality 50)
(define draft-quality 20)
(define-key-and-parameter quality 'quality max-quality)


(define-key-and-parameter horiz-alignment 'x-align 'left)
(define-key-and-parameter leading 'leading (floor (* (font-size-default) 1.4)))


(define-key-and-parameter paper-width 'paper-width (* 8.5 72))
(define-key-and-parameter paper-height 'paper-height (* 11.0 72))

(define line-looseness-key 'looseness)
(define width-key 'width)
(define horiz-alignment-last-line-key 'x-align-last-line)
(define word-break-key 'word-break)
(define no-break-key 'nb)
(define before-break-key 'bb)
(define ascent-key 'ascent)
(define height-key 'height)
(define unbreakable-key 'no-break)

(define split-quad-key 'word)


(define line-index-key 'line-idx)
(define total-lines-key 'lines)
(define page-index-key 'page-idx)
(define column-index-key 'column-idx)

(define x-position-key 'x)
(define y-position-key 'y)

(define page-key 'page)

(define soft-hyphen #\u00AD)
(define hyphens-and-dashes (list "-" "–" "—" (format "~a" soft-hyphen)))
(define spaces '(" "))
(define empty-string '"")

(define mergeable-quad-types '(char run word))



(define-parameter default-word-break-list '(nb "" bb "-"))

(define-parameter optical-overhang 0.8)

(define line-looseness-tolerance 0.05) ; 0.04 seems to be the magic point that avoids a lot of hyphenation
(define hyphen-limit 1) ; does not work with first-fit wrapping
(define minimum-last-line-chars 5)
(define allow-hyphenated-last-word-in-paragraph #t)
(define allowed-overfull-ratio 1.015)
(define last-line-can-be-short #t)
(define use-optical-kerns? #t)
(define use-hyphenation? #t)

(define new-line-penalty 5000)
(define hyphen-penalty 5000)

(define hanging-chars '("." "-" "," "‘" "’" "“" "”" "'" "\"" ")" "(" "[" "]" "{" "}" ":" ";"))

(define minimum-lines-per-column 4)
(define min-first-lines 2)
(define min-last-lines 2)
(define default-lines-per-column 36)

(define-parameter logging-level 'debug) ;; usually 'debug for dev. change to 'info for less