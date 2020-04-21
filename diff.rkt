#lang typed/racket
(require "core.rkt" "change.rkt" "wrapped-move-scope.rkt")

(provide (all-defined-out))

(module+ test (require typed/rackunit))

(struct Region ([scope : Scope] [lines : (Listof String)]) #:transparent)

(struct Diff-item ([old-region : Region] [new-region : Region]) #:transparent)

(define-type Diff-items (Listof Diff-item))

(: undo (-> Diff-item (Listof String) (Values Point (Listof String))))
(define (undo diff lines)
  (match-define (Diff-item old new) diff)
  (define new-scope (Region-scope new))
  (define old-lines (Region-lines old))
  (replace-scope new-scope lines old-lines))

(: undo-diffs (-> Diff-items (Listof String) (Values Point (Listof String))))
(define (undo-diffs diff-lst lines)
  (for/fold
   ([_ (Point 0 0 0)]
    [ls lines])
   ([d diff-lst])
    (undo d ls)))

(module+ test
  (let* ([old-scope (Scope (Point 0 0 0) (Point 1 0 0) #t #f 'char)]
         [new-scope (Scope (Point 0 0 0) (Point 0 0 0) #t #f 'char)]
         [old-region (Region old-scope '("123"))]
         [new-region (Region new-scope '())]
         [diff (Diff-item old-region new-region)])
    (let-values ([(p lines) (undo diff (list "abc"))])
      (check-equal? p (Point 0 3 3))
      (check-equal? lines '("123abc"))))
  (let* ([old-scope (Scope (Point 0 0 0) (Point 1 0 0) #t #f 'char)]
         [new-scope (Scope (Point 0 0 0) (Point 0 0 0) #t #f 'char)]
         [old-region (Region old-scope '("123" ""))]
         [new-region (Region new-scope '())]
         [diff (Diff-item old-region new-region)])
    (let-values ([(p lines) (undo diff (list "abc" ""))])
      (check-equal? p (Point 1 0 0))
      (check-equal? lines '("123" "abc" ""))))
  (let ([diff (Diff-item
               (Region (Scope (Point 2 11 11) (Point 2 11 11) #t #f 'char)
                       '())
               (Region (Scope (Point 2 11 11) (Point 3 19 19) #t #f 'char)
                       '("123" "this is a new line ")))])
    (let-values ([(p lines) (undo diff (list "abc" "def" "abcdefghijk123" "this is a new line "))])
      (check-equal? p (Point 2 11 11))
      (check-equal? lines'("abc" "def" "abcdefghijk")))))


(: invert-diff (-> Diff-item Diff-item))
(define (invert-diff diff)
  (match-define (Diff-item old new) diff)
  (Diff-item new old))

(: redo (-> Diff-item (Listof String) (Values Point (Listof String))))
(define (redo diff lines)
  (undo (invert-diff diff) lines))

(: redo-diffs (-> Diff-items (Listof String) (Values Point (Listof String))))
(define (redo-diffs diff-lst lines)
  ;(displayln (~e 'redo-diffs diff-lst lines))
  (for/fold
   ([_ (Point 0 0 0)]
    [ls lines])
   ([d (reverse diff-lst)])
    (redo d ls)))

(module+ test
  (let ([diff (Diff-item
               (Region (Scope (Point 2 11 11) (Point 2 11 11) #t #f 'char)
                       '())
               (Region (Scope (Point 2 11 11) (Point 3 19 19) #t #f 'char)
                       '("123" "this is a new line ")))])
    (let-values ([(p lines) (redo diff '("abc" "def" "abcdefghijk"))])
      (check-equal? p (Point 3 19 19))
      (check-equal? lines '("abc" "def" "abcdefghijk123" "this is a new line ")))
    (let-values ([(p0 lines0) (redo diff '("abc" "def" "abcdefghijk"))])
      (let-values ([(p1 lines1) (undo diff lines0)])
        (check-equal? p1 (Point 2 11 11))
        (check-equal? lines1 '("abc" "def" "abcdefghijk"))))))

(module+ test
  (let ([diffs (list
                (Diff-item
                 (Region (Scope (Point 0 0 0) (Point 0 0 0) #t #f 'char) '())
                 (Region (Scope (Point 0 0 0) (Point 0 4 4) #t #f 'char) '("    "))))])
    (let-values ([(p lines) (undo-diffs diffs '("    abc" "def" "abcdefghijk"))])
      (check-equal? p (Point 0 0 0))
      (check-equal? lines '("abc" "def" "abcdefghijk")))
    (let-values ([(p lines) (redo-diffs diffs '("abc" "def" "abcdefghijk"))])
      (check-equal? p (Point 0 4 4))
      (check-equal? lines '("    abc" "def" "abcdefghijk")))
    (let-values ([(p0 lines0) (redo-diffs diffs '("abc" "def" "abcdefghijk"))])
      (let-values ([(p1 lines1) (undo-diffs diffs lines0)])
        (check-equal? p1 (Point 0 0 0))
        (check-equal? lines1 '("abc" "def" "abcdefghijk"))))))


(module+ test
  (let ([diffs (list (Diff-item
                      (Region (Scope (Point 0 3 +inf.0) (Point 0 3 +inf.0) #t #t 'char) '("t"))
                      (Region (Scope (Point 0 3 +inf.0) (Point 0 3 +inf.0) #t #f 'char) '())))])
    (let-values ([(p lines) (undo-diffs diffs '("abc" "def"))])
      (check-equal? p (Point 0 4 4))
      (check-equal? lines '("abct" "def")))
    (let-values ([(p lines) (redo-diffs diffs '("abct" "def"))])
      (check-equal? p (Point 0 3 +inf.0))
      (check-equal? lines '("abc" "def")))))

(module+ test
  (let ([diffs 
         (list (Diff-item
                (Region (Scope (Point 0 0 0) (Point 0 0 0) #t #f 'line) '())
                (Region (Scope (Point 0 0 0) (Point 1 0 0) #t #f 'line) '("Sing"))))])
    (let-values ([(p lines) (redo-diffs diffs '("abc" "def"))])
      (check-equal? p (Point 0 0 0))
      (check-equal? lines '("Sing" "abc" "def")))
    (let-values ([(p lines) (undo-diffs diffs '("Sing" "abc" "def"))])
      (check-equal? p (Point 0 0 0))
      (check-equal? lines '("abc" "def")))))

(module+ test
  (let ([diffs
         (list (Diff-item (Region (Scope (Point 1 7 7) (Point 2 19 19) #t #t 'block)
                         '("Atreus, " "ther."))
                 (Region (Scope (Point 1 7 7) (Point 2 19 19) #t #f 'block)
                         '("ATREUS, " "THER."))))])
    (let-values ([(p lines) (undo-diffs diffs '("for so were the" "son of ATREUS, " "one anoTHER."))])
      (check-equal? p (Point 1 7 7))
      (check-equal? lines '("for so were the" "son of Atreus, " "one another.")))))
