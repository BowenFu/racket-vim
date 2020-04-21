#lang typed/racket
(require "core.rkt" "move.rkt")

(provide (all-defined-out))

(module+ test (require typed/rackunit))

; #t for dir means ->
(struct Scope ([start : Point] [end : Point] [dir : Boolean] [include-real-end? : Boolean] [mode : Symbol]) #:transparent)

(: line-scope (-> Point String Scope))
(define (line-scope p l)
  (Scope p p #t #t 'line))

(: left-scope (-> Point Natural Scope))
(define (left-scope p count)
  (Scope (left-point p count) p #f #f 'char))

(: right-scope* (-> Point String Natural Scope))
(define (right-scope* p l count)
  (define right-p (right-point* p l count))
  (Scope p right-p #t #f 'char))

(: up-scope (-> Point (Listof String) Natural Scope))
(define (up-scope p lines count)
  (Scope (up-point p lines count) p #f #f 'char))

(: down-scope (-> Point (Listof String) Natural Scope))
(define (down-scope p lines count)
  (Scope p (down-point p lines count) #t #f 'char))

(: up-scope-line-mode (-> Point (Listof String) Natural Scope))
(define (up-scope-line-mode p lines count)
  (Scope (up-point p lines count) p #f #t 'line))

(: down-scope-line-mode (-> Point (Listof String) Natural Scope))
(define (down-scope-line-mode p lines count)
  (Scope p (down-point p lines count) #t #t 'line))

(: line-end-scope (-> Point String Scope))
(define (line-end-scope p l)
  (Scope p (line-end-point (Point-row p) l) #t #t 'char))

(: after-line-end-scope (-> Point String Scope))
(define (after-line-end-scope p l)
  (Scope p (after-line-end-point (Point-row p) l) #t #f 'char))

(: line-start-scope (-> Point Scope))
(define (line-start-scope p)
  (Scope (line-start-point (Point-row p)) p #f #f 'char))

(: line-scope-scope (-> Point String Scope))
(define (line-scope-scope p l)
  (define row (Point-row p))
  (Scope (line-start-point row) (line-end-point row l) #t #t 'char))

(: e-scope (-> Point String Natural Scope))
(define (e-scope p l count)
  (Scope p (e-point p l count) #t #t 'char))

(: E-scope (-> Point String Natural Scope))
(define (E-scope p l count)
  (Scope p (E-point p l count) #t #t 'char))

(: w-scope (-> Point String Natural Scope))
(define (w-scope p l count)
  (Scope p (before-w-point p l count) #t #t 'char))

(: i-w-scope (-> Point String Natural Scope)) ;;; buggy when start at space
(define (i-w-scope p l count)
  (define pred (Point-char-pred p l))
  (define p-start (left-cont-last-if-point pred p l))
  (define w-p (w-point p l (cast (sub1 count) Natural)))
  (define p-end (right-cont-last-if-point pred w-p l))
  (Scope p-start p-end #t #t 'char))

(module+ test
  (check-equal? (i-w-scope (Point 1 4 1) "abc def" 1)
                (Scope (Point 1 4 1) (Point 1 6 6) #t #t 'char))
  (check-equal? (i-w-scope (Point 1 2 1) "abc def" 1)
                (Scope (Point 1 0 0) (Point 1 2 1) #t #t 'char))
  (check-equal? (i-w-scope (Point 1 0 1) "abc" 1)
                (Scope (Point 1 0 1) (Point 1 2 2) #t #t 'char)))

(: a-w-scope (-> Point String Natural Scope))
(define (a-w-scope p l count)
  (define start-pred (Point-char-pred p l))
  (define p-start (left-cont-last-if-point start-pred p l))
  (define w-p (w-point p l (cast (sub1 count) Natural)))
  (define p-end (before-w-point w-p l))
  (Scope p-start p-end #t #t 'char))

(module+ test
  (check-equal? (a-w-scope (Point 1 4 1) "abc def" 1)
                (Scope (Point 1 4 1) (Point 1 6 6) #t #t 'char))
  (check-equal? (a-w-scope (Point 1 2 1) "abc def" 1)
                (Scope (Point 1 0 0) (Point 1 3 3) #t #t 'char))
  (check-equal? (a-w-scope (Point 1 0 1) "abc" 1)
                (Scope (Point 1 0 1) (Point 1 2 2) #t #t 'char))
  (check-equal? (a-w-scope (Point 1 2 1) "abc def " 2)
                (Scope (Point 1 0 0) (Point 1 6 6) #t #t 'char))) ; buggy

(: W-scope (-> Point String Natural Scope))
(define (W-scope p l count)
  (Scope p (before-W-point p l count) #t #t 'char))

(: i-W-scope (-> Point String Natural Scope))
(define (i-W-scope p l count)
  (define p-start (left-cont-last-if-point (negate char-whitespace?) p l))
  (define W-p (W-point p l
                       (cast (sub1 count) Natural)))
  (define p-end (right-cont-last-if-point (negate char-whitespace?) W-p l))
  (Scope p-start p-end #t #t 'char))

(module+ test
  (check-equal? (i-W-scope (Point 1 4 1) "abc de," 1)
                (Scope (Point 1 4 1) (Point 1 6 6) #t #t 'char))
  (check-equal? (i-W-scope (Point 1 2 1) "ab, def" 1)
                (Scope (Point 1 0 0) (Point 1 2 1) #t #t 'char))
  (check-equal? (i-W-scope (Point 1 0 1) "ab," 1)
                (Scope (Point 1 0 1) (Point 1 2 2) #t #t 'char))
  (check-equal? (i-W-scope (Point 1 2 1) "ab, def" 2)
                (Scope (Point 1 0 0) (Point 1 6 6) #t #t 'char))
  (check-equal? (i-W-scope (Point 1 0 1) "ab," 2)
                (Scope (Point 1 0 1) (Point 1 2 2) #t #t 'char)))

(: a-W-scope (-> Point String Natural Scope))
(define (a-W-scope p l count)
  (define p-start (left-cont-last-if-point (negate char-whitespace?) p l))
  (define W-p (W-point p l
                       (cast (sub1 count) Natural)))
  (define p-end (before-W-point W-p l))
  (Scope p-start p-end #t #t 'char))

(module+ test
  (check-equal? (a-W-scope (Point 1 4 1) "abc de," 1)
                (Scope (Point 1 4 1) (Point 1 6 6) #t #t 'char))
  (check-equal? (a-W-scope (Point 1 2 1) "ab, def" 1)
                (Scope (Point 1 0 0) (Point 1 3 3) #t #t 'char))
  (check-equal? (a-W-scope (Point 1 0 1) "ab," 1)
                (Scope (Point 1 0 1) (Point 1 2 2) #t #t 'char))
  (check-equal? (a-W-scope (Point 1 2 1) "ab, def" 2)
                (Scope (Point 1 0 0) (Point 1 6 6) #t #t 'char))
  (check-equal? (a-W-scope (Point 1 2 1) "ab, def gh" 2)
                (Scope (Point 1 0 0) (Point 1 7 7) #t #t 'char)))

(: b-scope (-> Point String Natural Scope))
(define (b-scope p l count)
  (Scope (b-point p l count) p #f #t 'char))

(: B-scope (-> Point String Natural Scope))
(define (B-scope p l count)
  (Scope (B-point p l count) p #f #t 'char))

(: t-scope (-> Char Point String Natural Scope))
(define (t-scope k p l count)
  (Scope p (t-point k p l count) #t #t 'char))

(: f-scope (-> Char Point String Natural Scope))
(define (f-scope k p l count)
  (Scope p (f-point k p l count) #t #t 'char))

(: T-scope (-> Char Point String Natural Scope))
(define (T-scope k p l count)
  (Scope (T-point k p l count) p #f #t 'char))

(: F-scope (-> Char Point String Natural Scope))
(define (F-scope k p l count)
  (Scope (F-point k p l count) p #f #t 'char))

(: G-scope (-> Point (Listof String) Scope))
(define (G-scope p lines)
  (Scope p (G-point lines) #t #t 'line))

(module+ test
  (check-equal? (G-scope (Point 0 0 0) (list)) (Scope (Point 0 0 0) (Point 0 0 0) #t #t 'line))
  (check-equal? (G-scope (Point 0 0 0) (list "abc")) (Scope (Point 0 0 0) (Point 0 0 0) #t #t 'line))
  (check-equal? (G-scope (Point 0 1 1) (list "abc" "")) (Scope (Point 0 1 1) (Point 1 0 0) #t #t 'line))
  (check-equal? (G-scope (Point 1 1 1) (list "a" "b" "c")) (Scope (Point 1 1 1) (Point 2 0 0) #t #t 'line)))

(: lines-scope (->* (Point Lines) (Symbol) Scope))
(define (lines-scope start lines [mode 'char])
  (define end (after-lines-point start lines mode))
  (Scope start end #t #f mode))
