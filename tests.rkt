#lang racket/base
(require "utils.rkt" "wrap.rkt" "quads.rkt" "world.rkt" racket/list racket/format)
(require rackunit)

(check-equal? (join-attrs (list (box '(width 10)) (box #f "foobar") (hash 'x 10) (list 'width 20))) 
              (list (cons 'width 10) (cons 'x 10) (cons 'width 20)))

(check-equal? (flatten-attrs (hash 'foo 'bar) (hash 'x 10)) (apply hash '(foo bar x 10)))
(check-equal? (flatten-attrs (hash 'x -5) (hash 'x 10)) (apply hash '(x 5)))
(check-equal? (merge-attrs (hash 'x -5) (hash 'x 10)) (apply hash '(x 10)))

(check-equal? (gather-common-attrs (list (box '(foo bar)) (box '(foo bar goo bar zam zino)) (box '(foo bar)))) '(foo bar))
(check-equal? (gather-common-attrs (list (box) (box '(foo bar goo bar zam zino)) (box '(foo bar)))) #f)
(check-equal? (gather-common-attrs (list (box '(width bar)) (box '(width bar)) (box '(width bar)))) #f)

(define b1 (box '(x 10) "1st" (box '(foo bar) "2nd") "3rd"))
(define b1-flattened (list (box '(x 10) "1st") (box '(x 10 foo bar) "2nd") (box '(x 10) "3rd")))


(define b3 (box #f (word) (line) (page)))
(check-true (sequence? b3))
;(check-equal? (for/list ([i (in-quad b3)]) i) (list (word) (line) (page)))

(check-true (quad= (flatten-quad b1) b1-flattened))

(define b2 (box '(x 10) (spacer) (box '(x 15) (spacer) (spacer)) (spacer)))
(define b2-flattened (list (spacer '(x 10)) (spacer '(x 25)) (spacer '(x 25)) (spacer '(x 10))))

(check-true (quad= (flatten-quad b2) b2-flattened))
(check-true (quad= (split-quad b2) b2-flattened))

(check-true (quad= (flatten-quad (box '(foo 10) (spacer) (box) (spacer))) (list (spacer '(foo 10)) (box '(foo 10)) (spacer '(foo 10)))))


(check-equal? (compute-absolute-positions (page '(x 100 y 100) (line '(x 10 y 10) (word '(x 1 y 1) "hello")
                                                                     (word '(x 2 y 2) "world"))))
              (page '(y 100.0 x 100.0) (line '(y 110.0 x 110.0) (word '(y 111.0 x 111.0) "hello")(word '(y 112.0 x 112.0) "world"))))


(define b2-exploded (list (word '(x 10) "1") (word '(x 10) "s") (word '(x 10) "t") (word '(x 10 foo bar) "2") (word '(x 10 foo bar) "n") (word '(x 10 foo bar) "d") (word '(x 10) "3") (word '(x 10) "r") (word '(x 10) "d")))

(check-true (quad= (split-quad b1) b2-exploded))


(check-false (quad-has-attr? (box) 'foo))
(check-true (quad-has-attr? (box '(foo bar)) 'foo))

(check-equal? (quad-attr-set (box '(foo bar)) 'foo 'zam) (box '(foo zam)))
(check-equal? (quad-attr-set (box #f) 'foo 'zam) (box '(foo zam)))
(check-equal? (quad-attr-set* (box #f) 'foo 'zam 'bar 'boo) (box '(foo zam bar boo)))
(check-equal? (quad-attr-set* (box '(foo bar)) 'foo 'zam 'bar 'boo) (box '(foo zam bar boo)))

(check-equal? (quad-attr-remove (box '(foo bar zim zam)) 'foo) (box '(zim zam)))
(check-equal? (quad-attr-remove (box #f) 'zim) (box))
(check-equal? (quad-attr-remove* (box '(foo bar zim zam ding dong)) 'foo 'ding) (box '(zim zam)))
(check-equal? (quad-attr-remove* (box #f) 'zim) (box))

(check-true (quad-ends-with? (box #f "foo") "foo"))
(check-false (quad-ends-with? (box #f "foo") "food"))
(check-false (quad-ends-with? (box #f (box #f "foo")) "food"))
(check-true (quad-ends-with? (box #f (box #f "foo")) "foo"))
(check-true (quad-ends-with? (box #f (box #f "foo")) "o"))
(check-true (quad-ends-with? (box #f (box #f (box #f (box #f (box #f "foo-"))))) "-"))

(check-equal? (quad-append (box #f "foo") "bar") (box #f "foo" "bar"))
(check-equal? (quad-append (box #f "foo") (box #f "bar")) (box #f "foo" (box #f "bar")))

(check-equal? (quad-last-char (box #f (box #f "foo") "food")) "d")
(check-equal? (quad-last-char (box #f (box #f "foo"))) "o")
(check-equal? (quad-last-char (box #f "foo")) "o")
(check-false (quad-last-char (box)))

(check-equal? (quad-first-char (box #f (box #f "foo") "bar")) "f")
(check-equal? (quad-first-char (box #f (box #f "foo") "bar")) "f")
(check-equal? (quad-first-char (box #f "foo")) "f")
(check-false (quad-first-char (box)))

(check-equal? (quad->string (box '(width 100) "foo")) "foo")
(check-equal? (quad->string (box '(width 100) "foo" (box '(width 100) "bar"))) "foobar")
(check-equal? (quad->string (box '(width 100) "foo" (box '(width 100) "bar") "ino")) "foobarino")
(check-equal? (quad->string (box '(width 100))) "")


(check-false (whitespace? (~a #\u00A0)))
(check-true (whitespace/nbsp? (~a #\u00A0)))
(check-true (whitespace/nbsp? (word #f (~a #\u00A0))))
(check-false (whitespace? (format " ~a " #\u00A0)))
(check-true (whitespace/nbsp? (format " ~a " #\u00A0)))
(define funny-unicode-spaces (map ~a (list #\u2000 #\u2007 #\u2009 #\u200a #\u202f)))
(check-true (andmap whitespace? funny-unicode-spaces))
(check-true (andmap whitespace/nbsp? funny-unicode-spaces))


(require "experimental.rkt")
(define ti (block '(measure 54) "Meg is " (box '(foo 42)) " ally."))
(define-values (tokens attrs) (make-tokens-and-attrs ti))
(current-tokens tokens)
(current-token-attrs attrs)
(check-equal? tokens (vector #\M #\e #\g #\space #\i #\s #\space (box) #\space #\a #\l #\l #\y #\.))
(check-equal? attrs '(#(#hash((measure . 54)) 0 14) #(#hash((foo . 42)) 7 8)))

(let ([world:minimum-last-line-chars 0])
  (check-equal? (map (compose1 quad-list last quad-list) (make-pieces (split-quad (block #f "Foo-dog and " (box) " mas\u00adsachu.")))) '(("o") ("g") ("d") () ("s") ("."))))
