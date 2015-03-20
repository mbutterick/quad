#lang racket/base
(require "utils-typed.rkt" "quads-typed.rkt" "world-typed.rkt" "measure-typed.rkt" racket/list racket/format)
(require rackunit)

(check-equal? (join-attrs (list (box '(width 10.0)) (quad-attrs (box '(x 10.0))) (list 'width 20.0))) 
              (list (cons 'width 10.0) (cons 'x 10.0) (cons 'width 20.0)))

(check-equal? (flatten-attrs (box '(foo bar)) (hash 'x 10.0)) (apply hash '(foo bar x 10.0)))
(check-equal? (flatten-attrs (hash 'x -5.0) (hash 'x 10.0)) (apply hash '(x 5.0)))
(check-equal? (merge-attrs (hash 'x -5.0) (hash 'x 10.0)) (apply hash '(x 10.0)))

(check-equal? (gather-common-attrs (list (box '(foo bar)) (box '(foo bar goo bar zam zino)) (box '(foo bar goo rab)))) '(foo bar))
(check-false (gather-common-attrs (list (box) (box '(foo bar goo bar zam zino)) (box '(foo bar)))))
(check-false (gather-common-attrs (list (box '(width bar)) (box '(width bar)) (box '(width bar)))))
(check-false (gather-common-attrs (list (box '(foo bar)) (box '(foo zam)))))

(define b1 (box '(x 10.0) "1st" (box '(foo bar) "2nd") "3rd"))
(define b1-flattened (list (box '(x 10.0) "1st") (box '(x 10.0 foo bar) "2nd") (box '(x 10.0) "3rd")))

(define b3 (box #f (word) (line) (page)))

(check-true (quad= (flatten-quad b1) b1-flattened))

(define b2 (box '(x 10.0) (spacer) (box '(x 15.0) (spacer) (spacer)) (spacer)))
(define b2-flattened (list (spacer '(x 10.0)) (spacer '(x 25.0)) (spacer '(x 25.0)) (spacer '(x 10.0))))

(check-true (quad= (flatten-quad b2) b2-flattened))
(check-true (quad= (split-quad b2) b2-flattened))

(check-true (quad= (flatten-quad (box '(foo 10) (spacer) (box) (spacer))) (list (spacer '(foo 10)) (box '(foo 10)) (spacer '(foo 10)))))

(check-equal? (compute-absolute-positions (page '(x 100.0 y 100.0) (line '(x 10.0 y 10.0) (word '(x 1.0 y 1.0) "hello")
                                                                     (word '(x 2.0 y 2.0) "world"))))
              (page '(y 100.0 x 100.0) (line '(y 110.0 x 110.0) (word '(y 111.0 x 111.0) "hello")(word '(y 112.0 x 112.0) "world"))))

(define b2-exploded (list (word '(x 10.0) "1") (word '(x 10.0) "s") (word '(x 10.0) "t") (word '(x 10.0 foo bar) "2") (word '(x 10.0 foo bar) "n") (word '(x 10.0 foo bar) "d") (word '(x 10.0) "3") (word '(x 10.0) "r") (word '(x 10.0) "d")))

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

(check-equal? (measure-text "foobar" 10.0 "Courier") 36.0059)