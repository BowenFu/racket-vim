#lang typed/racket
(require "core.rkt")

(provide (all-defined-out))

(module+ test (require typed/rackunit))

(: line-end-col (-> String Natural))
(define (line-end-col l)
  (max 0 (sub1 (string-length l))))

(: after-line-end-col (-> String Natural))
(define (after-line-end-col l)
  (string-length l))

(module+ test
  (check-equal? (line-end-col "abc") 2)
  (check-equal? (after-line-end-col "abc") 3))

(: left-point (->* (Point) (Natural) Point))
(define (left-point p [count 1])
  (define-values (row col) (Point-row-col p))
  (define start-col 0)
  (define new-col (- col count))
  (define real-new-col
    (max new-col start-col))
  (Point row real-new-col real-new-col))

(module+ test
  (check-equal? (left-point (Point 1 2 1) 2) (Point 1 0 0))
  (check-equal? (left-point (Point 1 0 1)) (Point 1 0 0)))

(: right-point-base (-> Point String Boolean Natural Point))
(define (right-point-base p l *-mode? count)
  (match-define (Point row col max-col) p)
  (define end-col ((if *-mode? after-line-end-col line-end-col) l))
  (define new-col (+ col count))
  (define real-new-col
    (min new-col end-col))
  (define new-max-col
    (cast
     (max real-new-col max-col)
     (U +inf.0 Natural)))
  (Point row real-new-col new-max-col))

(: right-point (->* (Point String) (Natural) Point))
(define (right-point p l [count 1])
  (right-point-base p l #f count))

(module+ test
  (check-equal? (right-point (Point 1 2 2) "abc") (Point 1 2 2))
  (check-equal? (right-point (Point 1 0 2) "abcd" 2) (Point 1 2 2))
  (check-equal? (right-point* (Point 1 2 2) "abc") (Point 1 3 3))
  (check-equal? (right-point* (Point 1 0 2) "abcd" 2) (Point 1 2 2))
  (check-equal? (right-point* (Point 1 0 0) "abc" 2) (Point 1 2 2)))

(: right-point* (->* (Point String) (Natural) Point))
(define (right-point* p l [count 1])
  (right-point-base p l #t count))

(: line-end-point (-> Natural String Point))
(define (line-end-point row l)
  (define end-col (line-end-col l))
  (Point row end-col +inf.0))

(: \|-point (-> Natural String Natural Point))
(define (\|-point row l count)
  (define end-col (line-end-col l))
  (define real-col
    (cast (min end-col (sub1 count)) Natural))
  (Point row real-col real-col))

(module+ test
  (check-equal? (line-end-point 1 "abc") (Point 1 2 +inf.0))
  (check-equal? (line-end-point 0 "abc") (Point 0 2 +inf.0))
  (check-equal? (after-line-end-point 1 "abc") (Point 1 3 3))
  (check-equal? (after-line-end-point 0 "abc") (Point 0 3 3)))

(: $-point-base (-> Natural Lines Boolean Natural Point))
(define ($-point-base row lines *-mode? count)
  (define len (length lines))
  (define new-row (sub1 (max (min (+ row count) len) 1)))
  (define line (list-ref lines new-row))
  ((if *-mode? after-line-end-point line-end-point) new-row line))
  
(: $-point (-> Natural Lines Natural Point))
(define ($-point row lines count)
  ($-point-base row lines #f count))

(: $-point* (-> Natural Lines Natural Point))
(define ($-point* row lines count)
  ($-point-base row lines #t count))
  
(module+ test
  (check-equal? ($-point 1 '("" "abc") 1) (Point 1 2 +inf.0))
  (check-equal? ($-point 0 '("abc" "de") 2) (Point 1 1 +inf.0)))

(: after-line-end-point (-> Natural String Point))
(define (after-line-end-point row l)
  (define end-col (string-length l))
  (Point row end-col end-col))

(module+ test
  (check-equal? (after-line-end-point 1 "abc") (Point 1 3 3))
  (check-equal? (after-line-end-point 0 "abc") (Point 0 3 3)))

(: line-start-point (-> Natural Point))
(define (line-start-point row)
  (Point row 0 0))

(module+ test
  (check-equal? (line-start-point 1) (Point 1 0 0)))

(: ^-point (-> Natural String Point))
(define (^-point row l)
  (define p (Point row 0 0))
  (right-skip-space-point p l))

(module+ test
  (check-equal? (^-point 1 "  abc") (Point 1 2 2)))

(: up-point-base (-> Point (Listof String) Boolean Natural Point))
(define (up-point-base p lines *-mode? count)
  (define-values (row col) (Point-row-col p))
  (define new-row (max 0 (- row count)))
  (cond 
    [(= new-row row) p]
    [else
     (define max-col (Point-max-col p))
     (define end-col ((if *-mode? after-line-end-col line-end-col) (list-ref lines new-row)))
     (Point new-row (exact-round (min end-col max-col)) max-col)]))

(: up-point (->* (Point (Listof String)) (Natural) Point))
(define (up-point p lines [count 1])
  (up-point-base p lines #f count))

(: up-point* (->* (Point (Listof String)) (Natural) Point))
(define (up-point* p lines [count 1])
  (up-point-base p lines #t count))

(module+ test
  (let ([lines (list "ab" "cdef")])
    (check-equal? (up-point (Point 0 0 0) lines) (Point 0 0 0))
    (check-equal? (up-point (Point 0 0 2) lines) (Point 0 0 2))
    (check-equal? (up-point (Point 0 1 1) lines) (Point 0 1 1))
    (check-equal? (up-point (Point 1 2 3) lines) (Point 0 1 3))
    (check-equal? (up-point (Point 1 2 3) lines 2) (Point 0 1 3))
    (check-equal? (up-point* (Point 0 0 0) lines) (Point 0 0 0))
    (check-equal? (up-point* (Point 0 0 2) lines) (Point 0 0 2))
    (check-equal? (up-point* (Point 0 1 1) lines) (Point 0 1 1))
    (check-equal? (up-point* (Point 1 2 3) lines) (Point 0 2 3))))

(: down-point-base (-> Point (Listof String) Boolean Natural Point))
(define (down-point-base p lines *-mode? count)
  (define-values (row col) (Point-row-col p))
  (define new-row
    (cast (min (sub1 (length lines)) (+ row count)) Natural))
  (cond 
    [(= new-row row) p]
    [else
     (define max-col (Point-max-col p))
     (define end-col ((if *-mode? after-line-end-col line-end-col) (list-ref lines new-row)))
     (Point new-row (exact-round (min end-col max-col)) max-col)]))

(: down-point (->* (Point (Listof String)) (Natural) Point))
(define (down-point p lines [count 1])
  (down-point-base p lines #f count))

(: down-point* (->* (Point (Listof String)) (Natural) Point))
(define (down-point* p lines [count 1])
  (down-point-base p lines #t count))

(module+ test
  (let ([lines (list "ab" "cdef" "gh")])
    (check-equal? (down-point (Point 0 0 0) lines 2) (Point 2 0 0))
    (check-equal? (down-point (Point 0 0 0) lines) (Point 1 0 0))
    (check-equal? (down-point (Point 0 0 2) lines) (Point 1 2 2))
    (check-equal? (down-point (Point 0 1 1) lines) (Point 1 1 1))
    (check-equal? (down-point (Point 1 1 3) lines) (Point 2 1 3))
    (check-equal? (down-point (Point 0 2 1) lines) (Point 1 1 1))
    (check-equal? (down-point (Point 1 2 3) lines) (Point 2 1 3))
    (check-equal? (down-point* (Point 0 0 0) lines) (Point 1 0 0))
    (check-equal? (down-point* (Point 0 0 2) lines) (Point 1 2 2))
    (check-equal? (down-point* (Point 0 1 1) lines) (Point 1 1 1))
    (check-equal? (down-point* (Point 1 1 3) lines) (Point 2 2 3))
    (check-equal? (down-point* (Point 0 2 1) lines) (Point 1 1 1))
    (check-equal? (down-point* (Point 1 2 3) lines) (Point 2 2 3))))

(define-type Char-Pred (-> Char Boolean))

;;; find related util functions, submodule
; str pred -> integer index
(: find-if-backward (-> String Char-Pred (Option Natural)))
(define (find-if-backward str pred?)
  (for/or
      ([i (in-range (- (string-length str) 1) -1 -1)] #:when (pred? (string-ref str i)))
    (cast i Natural)))

(module+ test
  (check-equal? (find-if-backward "abcd0" char-alphabetic?) 3))

; str pred -> integer index
(: find-if-forward (-> String Char-Pred (Option Natural)))
(define (find-if-forward str pred?)
  (for/or
      ([s str] [i (in-naturals)] #:when (pred? s))
    (cast i Natural)))

(module+ test
  (check-equal? (find-if-forward "abcd0" char-numeric?) 4))

(: right-find-if-point (-> Point String Char-Pred Point))
(define (right-find-if-point p l pred)
  (define-values (row col) (Point-row-col p))
  (define end-col (line-end-col l))
  (define line-length (string-length l))
  (define inc-1 (find-if-forward (substring l (+ 1 col) line-length) pred))
  (cond
    [inc-1
     (define new-col (+ col inc-1 1))
     (define new-max-col 
       (if (>= new-col end-col)
           +inf.0
           new-col))
     (define real-new-col (exact-round (min new-col end-col)))
     (Point row real-new-col new-max-col)]
    [else p]))

(module+ test
  (check-equal? (right-find-if-point (Point 1 2 2) "abc" char-alphabetic?) (Point 1 2 2))
  (check-equal? (right-find-if-point (Point 1 0 2) "abc" char-alphabetic?) (Point 1 1 1)))

(: left-find-if-point (-> Point String Char-Pred Point))
(define (left-find-if-point p l pred)
  (define-values (row col) (Point-row-col p))
  (define new-col (find-if-backward (substring l 0 col) pred))
  (cond
    [new-col
     (Point row new-col new-col)]
    [else p]))

(module+ test
  (check-equal? (left-find-if-point (Point 1 2 2) "abc" char-alphabetic?) (Point 1 1 1))
  (check-equal? (left-find-if-point (Point 1 0 2) "abc" char-alphabetic?) (Point 1 0 2)))

(: right-find-before-if-point (-> Point String Char-Pred Point))
(define (right-find-before-if-point p l pred)
  (define-values (row col) (Point-row-col p))
  (define end-col (line-end-col l))
  (define inc (find-if-forward (substring l (+ 1 col) (+ end-col 1)) pred))
  (cond
    [(and inc (not (= inc 0)))
     (define new-col (+ col inc))
     (define new-max-col 
       (if (>= new-col end-col)
           +inf.0
           new-col))
     (define real-new-col (exact-round (min new-col end-col)))
     (Point row real-new-col new-max-col)]
    [else p]))

(module+ test
  (check-equal? (right-find-before-if-point (Point 1 0 2) "abc" char-numeric?) (Point 1 0 2))
  (check-equal? (right-find-before-if-point (Point 1 2 2) "abc" char-alphabetic?) (Point 1 2 2))
  (check-equal? (right-find-before-if-point (Point 1 0 2) "abc" char-alphabetic?) (Point 1 0 2)))

(: left-find-before-if-point (-> Point String Char-Pred Point))
(define (left-find-before-if-point p l pred)
  (define-values (row col) (Point-row-col p))
  (define end-col (line-end-col l))
  (define new-col-1 (find-if-backward (substring l 0 col) pred))
  (cond
    [new-col-1
     (define new-col (+ new-col-1 1))
     (Point row new-col new-col)]
    [else p]))

(module+ test
  (check-equal? (left-find-before-if-point (Point 1 0 0) "123abc" char-alphabetic?) (Point 1 0 0))
  (check-equal? (left-find-before-if-point (Point 1 3 4) "123abc" char-alphabetic?) (Point 1 3 4))
  (check-equal? (left-find-before-if-point (Point 1 5 5) "abc123" char-alphabetic?) (Point 1 3 3)))

(: right-find-char-point (-> Char Point String Point))
(define (right-find-char-point char p l)
  (right-find-if-point p l (lambda (s) (equal? s char))))

(module+ test
  (check-equal? (right-find-char-point #\a (Point 1 0 0) "123abc") (Point 1 3 3))
  (check-equal? (right-find-char-point #\a (Point 1 2 4) "123abc") (Point 1 3 3))
  (check-equal? (right-find-char-point #\a (Point 1 5 5) "abc123") (Point 1 5 5)))

(: left-find-char-point (-> Char Point String Point))
(define (left-find-char-point char p l)
  (left-find-if-point p l (lambda (s) (equal? s char))))

(module+ test
  (check-equal? (left-find-char-point #\a (Point 1 0 0) "123abc") (Point 1 0 0))
  (check-equal? (left-find-char-point #\a (Point 1 2 4) "123abc") (Point 1 2 4))
  (check-equal? (left-find-char-point #\a (Point 1 5 5) "abc123") (Point 1 0 0)))

(: right-find-before-char-point (-> Char Point String Point))
(define (right-find-before-char-point char p l)
  (right-find-before-if-point p l (lambda (s) (equal? s char))))

(module+ test
  (check-equal? (right-find-before-char-point #\a (Point 1 0 0) "123abc") (Point 1 2 2))
  (check-equal? (right-find-before-char-point #\a (Point 1 2 4) "123abc") (Point 1 2 4))
  (check-equal? (right-find-before-char-point #\a (Point 1 5 5) "abc123") (Point 1 5 5)))

(: left-find-before-char-point (-> Char Point String Point))
(define (left-find-before-char-point char p l)
  (left-find-before-if-point p l (lambda (s) (equal? s char))))

(module+ test
  (check-equal? (left-find-before-char-point #\a (Point 1 0 0) "123abc") (Point 1 0 0))
  (check-equal? (left-find-before-char-point #\a (Point 1 2 4) "123abc") (Point 1 2 4))
  (check-equal? (left-find-before-char-point #\a (Point 1 5 5) "abc123") (Point 1 1 1)))

;;; continuous skip if utils
; str pred -> integer index
(: cont-last-if-backward (-> String Char-Pred (Option Natural)))
(define (cont-last-if-backward str pred)
  (define n
    (for/fold
     ([_ : (Option Natural) #f])
     ([i (in-range (- (string-length str) 1) -1 -1)] #:break (not (pred (string-ref str i))))
      (cast i Natural)))
  (cast n (Option Natural)))

(module+ test
  (check-equal? (cont-last-if-backward "abcd0ef" char-alphabetic?) 5)
  (check-equal? (cont-last-if-backward "abcdef" char-alphabetic?) 0))

; str pred -> integer index
(: cont-last-if-forward (-> String Char-Pred (Option Natural)))
(define (cont-last-if-forward str pred)
  (define n
    (for/fold
     ([_ : (Option Natural) #f])
     ([s str] [i (in-naturals)] #:break (not (pred s)))
      (cast i Natural)))
  (cast n (Option Natural)))

(module+ test
  (check-equal? (cont-last-if-forward "abcd0de" char-alphabetic?) 3)
  (check-equal? (cont-last-if-forward "abcdde" char-alphabetic?) 5))

;;; skip until utils
; str pred -> integer index
(: skip-until-backward (-> String Char-Pred (Option Natural)))
(define (skip-until-backward str pred)
  (define n
    (for/fold
     ([_ : (Option Natural) #f])
     ([i (in-range (- (string-length str) 1) -1 -1)] #:final (pred (string-ref str i)))
      (cast i Natural)))
  (cast n (Option Natural)))

(module+ test
  (check-equal? (skip-until-backward "abcd0ef" char-numeric?) 4)
  (check-equal? (skip-until-backward "abcdef" char-numeric?) 0))

; str pred -> integer index
(: skip-until-forward (-> String Char-Pred (Option Natural)))
(define (skip-until-forward str pred)
  (define n
    (for/fold
     ([_ :(Option Natural) #f])
     ([s str] [i (in-naturals)] #:final (pred s))
      (cast i Natural)))
  (cast n (Option Natural)))

(module+ test
  (check-equal? (skip-until-forward "abcd0de" char-numeric?) 4)
  (check-equal? (skip-until-forward "abcdde" char-numeric?) 5))

;;; skip the current Point first
(: right-cont-last-if-point (-> Char-Pred Point String Point))
(define (right-cont-last-if-point pred p l)
  ;(when #t (displayln (list 'right-cont-last-if-point pred p l)))
  (define-values (row col) (Point-row-col p))
  (define length (string-length l))
  (define inc-1 (cont-last-if-forward (substring l (+ 1 col) length) pred))
  (cond
    [inc-1
     (define new-col (+ col inc-1 1))
     (Point row new-col new-col)]
    [else p]))

(module+ test
  (check-equal? (right-cont-last-if-point char-alphabetic? (Point 0 1 1) "abcd0de") (Point 0 3 3))
  (check-equal? (right-cont-last-if-point char-alphabetic? (Point 0 4 4) "abcd0de") (Point 0 6 6))
  (check-equal? (right-cont-last-if-point char-alphabetic? (Point 0 1 1) "abcdde") (Point 0 5 5))
  (check-equal? (right-cont-last-if-point char-alphabetic? (Point 0 4 4) "abcdde") (Point 0 5 5)))

(: left-cont-last-if-point (-> Char-Pred Point String Point))
(define (left-cont-last-if-point pred p l)
  (define-values (row col) (Point-row-col p))
  (define new-col (cont-last-if-backward (substring l 0 col) pred))
  (cond
    [new-col
     (Point row new-col new-col)]
    [else p]))

(module+ test
  (check-equal? (left-cont-last-if-point char-alphabetic? (Point 0 1 1) "abcd0de") (Point 0 0 0))
  (check-equal? (left-cont-last-if-point char-alphabetic? (Point 0 4 4) "abcd0de") (Point 0 0 0))
  (check-equal? (left-cont-last-if-point char-alphabetic? (Point 0 1 1) "abcdde") (Point 0 0 0))
  (check-equal? (left-cont-last-if-point char-alphabetic? (Point 0 4 4) "abcdde") (Point 0 0 0))
  (check-equal? (left-cont-last-if-point char-alphabetic? (Point 0 1 1) "1abcd0de") (Point 0 1 1))
  (check-equal? (left-cont-last-if-point char-alphabetic? (Point 0 4 4) "1abcd0de") (Point 0 1 1))
  (check-equal? (left-cont-last-if-point char-alphabetic? (Point 0 1 1) "1abcdde") (Point 0 1 1))
  (check-equal? (left-cont-last-if-point char-alphabetic? (Point 0 4 4) "1abcdde") (Point 0 1 1)))


;;; skip the current Point first
;;; at least skip one
(: right-skip-until-point-from-next (-> Char-Pred Point String Point))
(define (right-skip-until-point-from-next pred p l)
  (define-values (row col) (Point-row-col p))
  (define length (string-length l))
  (define start-col (+ 1 col))
  (define inc-1 (skip-until-forward (substring l start-col length) pred))
  (cond
    [inc-1
     (define new-col (+ col inc-1 1))
     (Point row new-col new-col)]
    [else p]))

(module+ test
  (check-equal? (right-skip-until-point-from-next char-numeric? (Point 0 1 1) "abcd0de") (Point 0 4 4))
  (check-equal? (right-skip-until-point-from-next char-numeric? (Point 0 4 4) "abcd0de") (Point 0 6 6))
  (check-equal? (right-skip-until-point-from-next char-numeric? (Point 0 1 1) "abcdde") (Point 0 5 5))
  (check-equal? (right-skip-until-point-from-next char-numeric? (Point 0 4 4) "abcdde") (Point 0 5 5)))

;;; does not skip the current Point first
(: right-skip-until-point (-> Char-Pred Point String Point))
(define (right-skip-until-point pred p l)
  (define-values (row col) (Point-row-col p))
  (define length (string-length l))
  (define inc (skip-until-forward (substring l col length) pred))
  (cond
    [(and inc (not (= inc 0)))
     (define new-col (+ col inc))
     (Point row new-col new-col)]
    [else p]))

(module+ test
  (check-equal? (right-skip-until-point char-numeric? (Point 0 1 1) "abcd0de") (Point 0 4 4))
  (check-equal? (right-skip-until-point char-numeric? (Point 0 4 4) "abcd0de") (Point 0 4 4))
  (check-equal? (right-skip-until-point char-numeric? (Point 0 1 1) "abcdde") (Point 0 5 5))
  (check-equal? (right-skip-until-point char-numeric? (Point 0 4 4) "abcdde") (Point 0 5 5)))

(: left-skip-until-point (-> Char-Pred Point String Point))
(define (left-skip-until-point pred p l)
  (define-values (row col) (Point-row-col p))
  (define new-col (skip-until-backward (substring l 0 col) pred))
  (cond
    [new-col
     (Point row new-col new-col)]
    [else p]))

(module+ test
  (check-equal? (left-skip-until-point char-numeric? (Point 0 1 1) "abcd0de") (Point 0 0 0))
  (check-equal? (left-skip-until-point char-numeric? (Point 0 4 4) "abcd0de") (Point 0 0 0))
  (check-equal? (left-skip-until-point char-numeric? (Point 0 1 1) "abcdde") (Point 0 0 0))
  (check-equal? (left-skip-until-point char-numeric? (Point 0 4 4) "abcdde") (Point 0 0 0))
  (check-equal? (left-skip-until-point char-numeric? (Point 0 1 1) "123abcd0de") (Point 0 0 0))
  (check-equal? (left-skip-until-point char-numeric? (Point 0 4 4) "123abcd0de") (Point 0 2 2))
  (check-equal? (left-skip-until-point char-numeric? (Point 0 1 1) "123abcdde") (Point 0 0 0))
  (check-equal? (left-skip-until-point char-numeric? (Point 0 4 4) "123abcdde") (Point 0 2 2)))

(: right-skip-space-point (-> Point String Point))
(define (right-skip-space-point p l)
  (define pred (lambda (s) (not (equal? s #\space))))
  (right-skip-until-point pred p l))

(: right-skip-space-point-from-next (-> Point String Point))
(define (right-skip-space-point-from-next p l)
  (define pred (lambda ([s : Char]) (not (equal? s #\space))))
  (right-skip-until-point-from-next pred p l))

(module+ test
  (check-equal? (right-skip-space-point-from-next (Point 1 0 0) "123 abc") (Point 1 1 1))
  (check-equal? (right-skip-space-point-from-next (Point 1 2 4) "123  abc") (Point 1 5 5))
  (check-equal? (right-skip-space-point-from-next (Point 1 3 4) "123  abc") (Point 1 5 5))
  (check-equal? (right-skip-space-point-from-next (Point 1 5 5) "abc    123") (Point 1 7 7))
  (check-equal? (right-skip-space-point (Point 1 0 0) "123 abc") (Point 1 0 0))
  (check-equal? (right-skip-space-point (Point 1 2 4) "123  abc") (Point 1 2 4))
  (check-equal? (right-skip-space-point (Point 1 3 4) "123  abc") (Point 1 5 5))
  (check-equal? (right-skip-space-point (Point 1 5 5) "abc    123") (Point 1 7 7)))

(: left-skip-space-point (-> Point String Point))
(define (left-skip-space-point p l)
  (left-skip-until-point (negate char-whitespace?) p l))

(module+ test
  (check-equal? (left-skip-space-point (Point 1 0 0) "123, abc") (Point 1 0 0))
  (check-equal? (left-skip-space-point (Point 1 6 7) "123,  abc") (Point 1 3 3))
  (check-equal? (left-skip-space-point (Point 1 8 8) "abc,  123") (Point 1 7 7)))

(: is-single-word-char? Char-Pred)
(define (is-single-word-char? char)
  (or (char-alphabetic? char) (char-numeric? char)))

(: left-skip-word-char-point (-> Point String Point))
(define (left-skip-word-char-point p l)
  (left-skip-until-point (negate is-single-word-char?) p l))

(module+ test
  (check-equal? (left-skip-word-char-point (Point 1 0 0) "123 abc") (Point 1 0 0))
  (check-equal? (left-skip-word-char-point (Point 1 2 4) "123  abc") (Point 1 0 0))
  (check-equal? (left-skip-word-char-point (Point 1 5 5) "abc    123") (Point 1 4 4)))

(: right-skip-word-char-point (-> Point String Point))
(define (right-skip-word-char-point p l)
  (define pred (Point-char-pred p l))
  (right-skip-until-point (negate pred) p l))

(module+ test
  (check-equal? (right-skip-word-char-point (Point 1 0 0) "123 abc") (Point 1 3 3))
  (check-equal? (right-skip-word-char-point (Point 1 2 4) "123  abc") (Point 1 3 3))
  (check-equal? (right-skip-word-char-point (Point 1 5 5) "abc    123") (Point 1 7 7)))

;;; move with find
(: e-point (->* (Point String) (Natural) Point))
(define (e-point p l [count 1])
  (for/fold
   ([e-p p])
   ([_ (in-range count)])
    (define skip-space (right-skip-space-point-from-next e-p l))
    (define pred (Point-char-pred skip-space l))
    (define new-e-p (right-cont-last-if-point pred skip-space l))
    #:break (equal? new-e-p e-p)
    new-e-p))

(module+ test
  (check-equal? (e-point (Point 1 0 0) "123, abc") (Point 1 2 2))
  (check-equal? (e-point (Point 1 2 4) "123,  abc") (Point 1 3 3))
  (check-equal? (e-point (Point 1 5 5) "abc,    123") (Point 1 10 10))
  (check-equal? (e-point (Point 1 0 0) "123, abc" 2) (Point 1 3 3))
  (check-equal? (e-point (Point 1 2 4) "123,  abc" 2) (Point 1 8 8)))

(: E-point (->* (Point String) (Natural) Point))
(define (E-point p l [count 1])
  (for/fold
   ([E-p p])
   ([_ (in-range count)])
    (define skip-space (right-skip-space-point-from-next E-p l))
    (define new-E-p (right-cont-last-if-point (negate char-whitespace?) skip-space l))
    #:break (equal? new-E-p E-p)
    new-E-p))

(module+ test
  (check-equal? (E-point (Point 1 0 0) "123, abc") (Point 1 3 3))
  (check-equal? (E-point (Point 1 2 4) "123,  abc") (Point 1 3 3))
  (check-equal? (E-point (Point 1 5 5) "abc,    123") (Point 1 10 10))
  (check-equal? (E-point (Point 1 0 0) "123, abc" 2) (Point 1 7 7))
  (check-equal? (E-point (Point 1 2 4) "123,  abc" 2) (Point 1 8 8)))

(: w-point (->* (Point String) (Natural) Point))
(define (w-point p l [count 1])
  (for/fold
   ([w-p p])
   ([_ (in-range count)])
    (define pred (Point-char-pred w-p l))
    (define skip-word (right-skip-until-point (negate pred) w-p l))
    (define new-w-p (right-skip-space-point skip-word l))
    #:break (equal? new-w-p w-p)
    new-w-p))

(module+ test
  (check-equal? (w-point (Point 1 0 0) "123abc") (Point 1 5 5))
  (check-equal? (w-point (Point 1 0 0) "123, abc") (Point 1 3 3))
  (check-equal? (w-point (Point 1 2 4) "123,  abc") (Point 1 3 3))
  (check-equal? (w-point (Point 1 5 5) "abc,    123") (Point 1 8 8))
  (check-equal? (w-point (Point 1 0 0) "123abc" 2) (Point 1 5 5))
  (check-equal? (w-point (Point 1 0 0) "123, abc" 2) (Point 1 5 5))
  (check-equal? (w-point (Point 1 2 4) "123,  abc" 2) (Point 1 6 6)))

(: before-w-point (->* (Point String) (Natural) Point))
(define (before-w-point pp l [count 1])
  (define p : Point (w-point pp l (cast (sub1 count) Natural)))
  (define pred (Point-char-pred p l))
  (define skip-word (right-skip-until-point (negate pred) p l))
  (define w-p (right-skip-space-point skip-word l))
  (define skip-p-pred (Point-char-pred skip-word l))
  (if (equal? pred skip-p-pred) ; not at end
      w-p
      (left-point w-p)))

(module+ test
  (check-equal? (before-w-point (Point 1 0 0) "123abc") (Point 1 5 5))
  (check-equal? (before-w-point (Point 1 0 0) "123, abc") (Point 1 2 2))
  (check-equal? (before-w-point (Point 1 2 4) "123,  abc") (Point 1 2 2))
  (check-equal? (before-w-point (Point 1 5 5) "abc,    123") (Point 1 7 7))
  (check-equal? (before-w-point (Point 1 0 0) "123abc" 2) (Point 1 5 5))
  (check-equal? (before-w-point (Point 1 0 0) "123, abc" 2) (Point 1 4 4))
  (check-equal? (before-w-point (Point 1 2 4) "123,  abc" 2) (Point 1 5 5)))

(: W-point (->* (Point String) (Natural) Point))
(define (W-point p l [count 1])
  (for/fold
   ([W-p p])
   ([_ (in-range count)])
    (define pred (Point-char-pred W-p l))
    (define skip-word (right-skip-until-point char-whitespace? W-p l))
    (define new-W-p (right-skip-space-point skip-word l))
    #:break (equal? new-W-p W-p)
    new-W-p))

(module+ test
  (check-equal? (W-point (Point 1 0 0) "123, abc") (Point 1 5 5))
  (check-equal? (W-point (Point 1 2 4) "123,  abc") (Point 1 6 6))
  (check-equal? (W-point (Point 1 5 5) "abc,    123") (Point 1 8 8))
  (check-equal? (W-point (Point 1 5 5) "abc, x 123" 2) (Point 1 9 9)))

(: before-W-point (->* (Point String) (Natural) Point))
(define (before-W-point pp l [count 1])
  (define p : Point (w-point pp l (cast (sub1 count) Natural)))
  (define skip-word (right-skip-until-point char-whitespace? p l))
  (define W-p (right-skip-space-point skip-word l))
  (if (not (Point-char-blank? skip-word l)) ; at end
      W-p
      (left-point W-p)))

(module+ test
  (check-equal? (before-W-point (Point 1 0 0) "123abc") (Point 1 5 5))
  (check-equal? (before-W-point (Point 1 0 0) "123, abc") (Point 1 4 4))
  (check-equal? (before-W-point (Point 1 2 4) "123,  abc") (Point 1 5 5))
  (check-equal? (before-W-point (Point 1 5 5) "abc,    123") (Point 1 7 7))
  (check-equal? (before-W-point (Point 1 0 0) "abc,  x  123" 2) (Point 1 5 5))
  (check-equal? (before-W-point (Point 1 0 0) "abc,  x  123" 3) (Point 1 8 8)))

(: char-pred (-> Char (Option Char-Pred)))
(define (char-pred c)
  (for/or ([pred
            (list is-single-word-char?
                  char-symbolic?
                  char-punctuation?
                  char-blank?)]
           #:when (pred c))
    pred))

(: Point-char-pred (-> Point String Char-Pred))
(define (Point-char-pred p l)
  (define c (string-ref l (Point-col p)))
  (cast (char-pred c) Char-Pred))

(: Point-char-blank? (-> Point String Boolean))
(define (Point-char-blank? p l)
  (define c (string-ref l (Point-col p)))
  (char-blank? c))

(: b-point (->* (Point String) (Natural) Point))
(define (b-point p l [count 1])
  (for/fold
   ([b-p p])
   ([_ (in-range count)])
    (define skip-space (left-skip-space-point b-p l))
    (define pred (Point-char-pred skip-space l))
    (define new-b-p (left-cont-last-if-point pred skip-space l))
    #:break (equal? new-b-p b-p)
    new-b-p))

(module+ test
  (check-equal? (b-point (Point 1 4 4) ",123abc") (Point 1 1 1))
  (check-equal? (b-point (Point 1 6 6) ",123abc") (Point 1 1 1))
  (check-equal? (b-point (Point 1 0 0) ",123, abc") (Point 1 0 0))
  (check-equal? (b-point (Point 1 2 4) ",123,  abc") (Point 1 1 1))
  (check-equal? (b-point (Point 1 5 5) ",123,  abc") (Point 1 4 4))
  (check-equal? (b-point (Point 1 6 6) ",123,  abc") (Point 1 4 4))
  (check-equal? (b-point (Point 1 8 8) ",123,  abc") (Point 1 7 7))
  (check-equal? (b-point (Point 1 9 9) ",123,  abc") (Point 1 7 7))
  (check-equal? (b-point (Point 1 5 5) ",abc,    123") (Point 1 4 4))
  (check-equal? (b-point (Point 1 9 9) ",123,  abc" 2) (Point 1 4 4)))

(: B-point (->* (Point String) (Natural) Point))
(define (B-point p l [count 1])
  (for/fold
   ([B-p p])
   ([_ (in-range count)])
    (define skip-space (left-skip-space-point p l))
    (define new-B-p (left-cont-last-if-point (negate char-whitespace?) skip-space l))
    #:break (equal? new-B-p B-p)
    new-B-p))

(module+ test
  (check-equal? (B-point (Point 1 0 0) ",123, abc") (Point 1 0 0))
  (check-equal? (B-point (Point 1 2 4) ",123,  abc") (Point 1 0 0))
  (check-equal? (B-point (Point 1 8 8) ",123,  abc") (Point 1 7 7))
  (check-equal? (B-point (Point 1 9 9) ",123,  abc") (Point 1 7 7))
  (check-equal? (B-point (Point 1 5 5) ",abc,    123") (Point 1 0 0))
  (check-equal? (B-point (Point 1 9 9) ",abc,    123" 2) (Point 1 0 0)))

(: t-point (->* (Char Point String) (Natural) Point))
(define (t-point k p l [count 1])
  (define pp (f-point k p l (cast (sub1 count) Natural)))
  (right-find-before-char-point k pp l))

(module+ test
  (check-equal? (t-point #\3 (Point 1 0 0) ",123, abc") (Point 1 2 2))
  (check-equal? (t-point #\3 (Point 1 2 4) ",123,  abc") (Point 1 2 4))
  (check-equal? (t-point #\3 (Point 1 8 8) ",123,  abc") (Point 1 8 8))
  (check-equal? (t-point #\3 (Point 1 5 5) ",abc,    123") (Point 1 10 10))
  (check-equal? (t-point #\3 (Point 1 0 0) ",abc,3   123" 2) (Point 1 10 10)))

(: f-point (->* (Char Point String) (Natural) Point))
(define (f-point k p l [count 1])
  (for/fold
   ([f-p p])
   ([_ (in-range count)])
    (define new-f-p (right-find-char-point k f-p l))
    #:break (equal? new-f-p f-p)
    new-f-p))

(module+ test
  (check-equal? (f-point #\3 (Point 1 0 0) ",123, abc") (Point 1 3 3))
  (check-equal? (f-point #\3 (Point 1 2 4) ",123,  abc") (Point 1 3 3))
  (check-equal? (f-point #\3 (Point 1 8 8) ",123,  abc") (Point 1 8 8))
  (check-equal? (f-point #\3 (Point 1 9 9) ",123,  abc") (Point 1 9 9))
  (check-equal? (f-point #\3 (Point 1 5 5) ",abc,    123") (Point 1 11 +inf.0))
  (check-equal? (f-point #\3 (Point 1 0 0) ",123, 3abc" 2) (Point 1 6 6)))

(: T-point (->* (Char Point String) (Natural) Point))
(define (T-point k p l [count 1])
  (define pp (F-point k p l (cast (sub1 count) Natural)))
  (left-find-before-char-point k pp l))

(module+ test
  (check-equal? (T-point #\3 (Point 1 0 0) ",123, abc") (Point 1 0 0))
  (check-equal? (T-point #\3 (Point 1 2 4) ",123,  abc") (Point 1 2 4))
  (check-equal? (T-point #\3 (Point 1 8 8) ",123,  abc") (Point 1 4 4))
  (check-equal? (T-point #\3 (Point 1 9 9) ",123,  abc") (Point 1 4 4))
  (check-equal? (T-point #\3 (Point 1 5 5) ",abc,    123") (Point 1 5 5))
  (check-equal? (T-point #\3 (Point 1 9 9) ",123, 3abc" 2) (Point 1 4 4)))

(: F-point (->* (Char Point String) (Natural) Point))
(define (F-point k p l [count 1])
  (for/fold
   ([F-p p])
   ([_ (in-range count)])
    (define new-F-p (left-find-char-point k F-p l))
    #:break (equal? new-F-p F-p)
    new-F-p))

(module+ test
  (check-equal? (F-point #\3 (Point 1 0 0) ",123, abc") (Point 1 0 0))
  (check-equal? (F-point #\3 (Point 1 2 4) ",123,  abc") (Point 1 2 4))
  (check-equal? (F-point #\3 (Point 1 8 8) ",123,  abc") (Point 1 3 3))
  (check-equal? (F-point #\3 (Point 1 9 9) ",123,  abc") (Point 1 3 3))
  (check-equal? (F-point #\3 (Point 1 5 5) ",abc,    123") (Point 1 5 5))
  (check-equal? (F-point #\3 (Point 1 9 9) ",ab3c, 3  123" 2) (Point 1 3 3)))

(: reverse-tf (-> Symbol Symbol))
(define (reverse-tf tf-f)
  (match tf-f
    ['t 'T]
    ['T 't]
    ['f 'F]
    ['F 'f]
    [_ (error 'missing-case-in-reverse-tf)]))

(: G-point (-> (Listof String) Point))
(define (G-point lines)
  (line-start-point (max 0 (sub1 (length lines)))))

(module+ test
  (check-equal? (G-point (list)) (Point 0 0 0))
  (check-equal? (G-point (list "abc")) (Point 0 0 0))
  (check-equal? (G-point (list "abc" "")) (Point 1 0 0))
  (check-equal? (G-point (list "a" "b" "c")) (Point 2 0 0)))

(: n%-point (-> (Listof String) Natural Point))
(define (n%-point lines count)
  (when (> count 100) (error 'out-of-percentage-range))
  (define row
    (cast (exact-round (max (sub1 (* count (length lines) 1/100)) 0)) Natural))
  (Point row 0 0))

(: after-lines-point (->* (Point Lines) (Symbol) Point))
(define (after-lines-point start lines [mode 'char])
  (define start-row (Point-row start))
  (cond
    [(empty? lines) start]
    [(equal? mode 'line)
     (define new-row (+ start-row (length lines)))
     (Point new-row 0 0)]
    [(equal? mode 'block)
     (define new-row (+ start-row (max 0 (sub1 (length lines)))))
     (define max-rel-col (apply max (map string-length lines)))
     (define new-col (+ (Point-col start) max-rel-col)) ;not include end
     (Point new-row new-col new-col)]
    [(= 1 (length lines))
     (define count (string-length (first lines)))
     (define col (+ (Point-col start) count))
     (Point start-row col col)]
    [else
     (define last-line (last lines))
     (define row
       (cast (+ start-row (length lines) -1)
             Natural))
     (after-line-end-point row last-line)]))

(module+ test
  (check-equal? (after-lines-point (Point 1 2 2) (list)) (Point 1 2 2))
  (check-equal? (after-lines-point (Point 1 2 2) (list "abc")) (Point 1 5 5))
  (check-equal? (after-lines-point (Point 1 2 +inf.0) (list "abc")) (Point 1 5 5))
  (check-equal? (after-lines-point (Point 1 2 2) (list "abc" "def")) (Point 2 3 3))
  (check-equal? (after-lines-point (Point 1 2 2) (list) 'line) (Point 1 2 2))
  (check-equal? (after-lines-point (Point 1 2 2) (list "abc") 'line) (Point 2 0 0))
  (check-equal? (after-lines-point (Point 1 2 +inf.0) (list "abc") 'line) (Point 2 0 0))
  (check-equal? (after-lines-point (Point 1 2 2) (list "abc" "def") 'line) (Point 3 0 0))
  (check-equal? (after-lines-point (Point 1 2 2) (list "abc0" "def") 'block) (Point 2 6 6)))
