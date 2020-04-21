#lang racket

(provide get-delete-diffs get-insert-diffs get-replace-diffs get-right-shift-diffs get-left-shift-diffs (struct-out Diff-item) (struct-out Region))

(require "core.rkt" "move.rkt" "wrapped-move-scope.rkt" "change.rkt" "diff.rkt")

(module+ test (require rackunit))

(define (get-deleted-region old-scope lines)
  (define deleted-lines (Scoped-lines old-scope lines))
  (Region old-scope deleted-lines))

(define (get-delete-diffs old-scope lines)
  (define old-region (get-deleted-region old-scope lines))
  (match-define (Scope start end _ _ mode) old-scope)
  (define new-end (if (equal? mode 'block)
                      (struct-copy Point start [row (Point-row end)])
                      start))
  (define new-region (Region (Scope start new-end #t #f mode) '()))
  (list (Diff-item old-region new-region)))

(module+ test
  (check-equal? (get-delete-diffs (Scope (Point 0 0 0) (Point 0 0 0) #t #f 'char) '(""))
                (list
                 (Diff-item
                  (Region (Scope (Point 0 0 0) (Point 0 0 0) #t #f 'char) '())
                  (Region (Scope (Point 0 0 0) (Point 0 0 0) #t #f 'char) '()))))
  (check-equal? (get-delete-diffs (Scope (Point 0 0 0) (Point 0 0 0) #t #t 'char) '("abc"))
                (list
                 (Diff-item
                  (Region (Scope (Point 0 0 0) (Point 0 0 0) #t #t 'char) '("a"))
                  (Region (Scope (Point 0 0 0) (Point 0 0 0) #t #f 'char) '()))))
  (check-equal? (get-delete-diffs (Scope (Point 0 0 0) (Point 0 0 0) #t #f 'line) '("abc"))
                (list
                 (Diff-item
                  (Region (Scope (Point 0 0 0) (Point 0 0 0) #t #f 'line) '())
                  (Region (Scope (Point 0 0 0) (Point 0 0 0) #t #f 'line) '()))))
  (check-equal? (get-delete-diffs (Scope (Point 0 0 0) (Point 0 0 0) #t #t 'char) '("abc"))
                (list
                 (Diff-item
                  (Region (Scope (Point 0 0 0) (Point 0 0 0) #t #t 'char) '("a"))
                  (Region (Scope (Point 0 0 0) (Point 0 0 0) #t #f 'char) '()))))
  
  (check-equal? (get-delete-diffs (Scope (Point 0 1 1) (Point 1 1 1) #t #f 'char) '("abc" "def"))
                (list
                 (Diff-item
                  (Region (Scope (Point 0 1 1) (Point 1 1 1) #t #f 'char) '("bc" "d"))
                  (Region (Scope (Point 0 1 1) (Point 0 1 1) #t #f 'char) '()))))
  (check-equal? (get-delete-diffs (Scope (Point 0 1 1) (Point 1 1 1) #t #t 'char) '("abc" "def"))
                (list
                 (Diff-item
                  (Region (Scope (Point 0 1 1) (Point 1 1 1) #t #t 'char) '("bc" "de"))
                  (Region (Scope (Point 0 1 1) (Point 0 1 1) #t #f 'char) '()))))
  (check-equal? (get-delete-diffs (Scope (Point 0 1 1) (Point 1 1 1) #t #f 'line) '("abc" "def"))
                (list
                 (Diff-item
                  (Region (Scope (Point 0 1 1) (Point 1 1 1) #t #f 'line) '("abc"))
                  (Region (Scope (Point 0 1 1) (Point 0 1 1) #t #f 'line) '()))))
  (check-equal? (get-delete-diffs (Scope (Point 0 1 1) (Point 1 1 1) #t #t 'char) '("abc" "def"))
                (list
                 (Diff-item
                  (Region (Scope (Point 0 1 1) (Point 1 1 1) #t #t 'char) '("bc" "de"))
                  (Region (Scope (Point 0 1 1) (Point 0 1 1) #t #f 'char) '())))))

(define (get-insert-region start inserted-lines mode)
  (define end (after-lines-point start inserted-lines mode))
  (define new-scope (Scope start end #t #f mode))
  (Region new-scope inserted-lines))

(module+ test
  (check-equal? (get-insert-region (Point 0 0 0) '("1") 'block)
                (Region (Scope (Point 0 0 0) (Point 0 1 1) #t #f 'block) '("1"))))
  
(define (get-insert-diffs start inserted-lines mode)
  (define new-region (get-insert-region start inserted-lines mode))
  (define old-region (Region (Scope start start #t #f mode) '()))
  (list (Diff-item old-region new-region)))

(module+ test
  (check-equal? (get-insert-diffs (Point 0 0 0) '("123") 'char)
                (list
                 (Diff-item
                  (Region (Scope (Point 0 0 0) (Point 0 0 0) #t #f 'char) '())
                  (Region (Scope (Point 0 0 0) (Point 0 3 3) #t #f 'char) '("123")))))
  (check-equal? (get-insert-diffs (Point 0 2 2) '("123") 'char)
                (list
                 (Diff-item
                  (Region (Scope (Point 0 2 2) (Point 0 2 2) #t #f 'char) '())
                  (Region (Scope (Point 0 2 2) (Point 0 5 5) #t #f 'char) '("123")))))
  (check-equal? (get-insert-diffs (Point 0 0 0) '("123") 'line)
                (list
                 (Diff-item
                  (Region (Scope (Point 0 0 0) (Point 0 0 0) #t #f 'line) '())
                  (Region (Scope (Point 0 0 0) (Point 1 0 0) #t #f 'line) '("123")))))
  (check-equal? (get-insert-diffs (Point 0 2 2) '("123") 'line)
                (list
                 (Diff-item
                  (Region (Scope (Point 0 2 2) (Point 0 2 2) #t #f 'line) '())
                  (Region (Scope (Point 0 2 2) (Point 1 0 0) #t #f 'line) '("123")))))
  
  (check-equal? (get-insert-diffs (Point 0 1 1) '("123" "456" "789") 'char)
                (list
                 (Diff-item
                  (Region (Scope (Point 0 1 1) (Point 0 1 1) #t #f 'char) '())
                  (Region
                   (Scope (Point 0 1 1) (Point 2 3 3) #t #f 'char)
                   '("123" "456" "789")))))
  (check-equal? (get-insert-diffs (Point 0 2 2) '("123" "456") 'char)
                (list
                 (Diff-item
                  (Region (Scope (Point 0 2 2) (Point 0 2 2) #t #f 'char) '())
                  (Region (Scope (Point 0 2 2) (Point 1 3 3) #t #f 'char) '("123" "456")))))
  (check-equal? (get-insert-diffs (Point 0 1 1) '("123" "") 'line)
                
                (list
                 (Diff-item
                  (Region (Scope (Point 0 1 1) (Point 0 1 1) #t #f 'line) '())
                  (Region (Scope (Point 0 1 1) (Point 2 0 0) #t #f 'line) '("123" "")))))
  (check-equal? (get-insert-diffs (Point 1 2 2) '("" "456") 'line)
                (list
                 (Diff-item
                  (Region (Scope (Point 1 2 2) (Point 1 2 2) #t #f 'line) '())
                  (Region (Scope (Point 1 2 2) (Point 3 0 0) #t #f 'line) '("" "456"))))))


(define (get-replace-diffs old-scope new-start inserted-lines mode lines)
  (define new-region (get-insert-region new-start inserted-lines mode))
  (define old-region (get-deleted-region old-scope lines))
  (list (Diff-item old-region new-region)))

(module+ test
  (check-equal? (get-replace-diffs (Scope (Point 0 1 1) (Point 1 1 1) #t #f 'char) (Point 0 1 1) '("3") 'char '("abcd" "efg"))
                (list
                 (Diff-item
                  (Region (Scope (Point 0 1 1) (Point 1 1 1) #t #f 'char) '("bcd" "e"))
                  (Region (Scope (Point 0 1 1) (Point 0 2 2) #t #f 'char) '("3"))))))
  
(define (get-right-shift-diff row)
  (define start (line-start-point row))
  (define old-scope (Scope start start #t #f 'char))
  (define old-region (Region old-scope '()))
  (define new-end (Point row shift-width shift-width))
  (define new-scope (Scope start new-end #t #f 'char))
  (define new-region (Region new-scope (list (make-indention))))
  (Diff-item old-region new-region))

(define (get-right-shift-diffs scope lines)
  (match-define (Scope (Point row1 _ _) (Point row2 _ _) _ _ _) scope)
  (cond
    [(empty? lines) (error 'empty-lines-for-right-shift)]
    [else
     (define-values (before-middle after) (split-at lines (add1 row2)))
     (define-values (before middle) (split-at before-middle row1))
     (define diffs
       (for/list
           ([i (in-range row1 (add1 row2))])
         (get-right-shift-diff i)))
     diffs]))

(module+ test
  (check-equal? (get-right-shift-diffs (Scope (Point 0 1 1) (Point 1 1 1) #t #f 'char) '("abcd" "efg"))
                (list (Diff-item (Region (Scope (Point 0 0 0) (Point 0 0 0) #t #f 'char) '())
                                 (Region (Scope (Point 0 0 0) (Point 0 4 4) #t #f 'char) '("    ")))
                      (Diff-item (Region (Scope (Point 1 0 0) (Point 1 0 0) #t #f 'char) '())
                                 (Region (Scope (Point 1 0 0) (Point 1 4 4) #t #f 'char) '("    "))))))
  
(define (get-left-shift-diff row l)
  (define start (line-start-point row))
  (define end-col (left-shift-line-drop-length l))
  (define end (Point row end-col end-col))
  (define old-scope (Scope start end #t #f 'char))
  (define old-region (Region old-scope (list (substring l 0 end-col))))
  (define ^-p (^-point row l))
  (define new-col (- (Point-col ^-p) end-col))
  (define new-start (Point row new-col new-col))
  (define new-scope (Scope new-start new-start #t #f 'char))
  (define new-region (Region new-scope '()))
  (Diff-item old-region new-region))

(define (get-left-shift-diffs scope lines)
  ;(displayln (~e 'get-left-shift-diffs scope lines))
  (match-define (Scope (Point row1 _ _) (Point row2 _ _) _ _ _) scope)
  (cond
    [(empty? lines) (error 'empty-lines-for-right-shift)]
    [else
     (define-values (before-middle after) (split-at lines (add1 row2)))
     (define-values (before middle) (split-at before-middle row1))
     (define diffs
       (for/list
           ([i (in-range row1 (add1 row2))]
            [l middle])
         (get-left-shift-diff i l)))
     diffs]))

(module+ test
  (check-equal?
   (get-left-shift-diffs (Scope (Point 0 1 1) (Point 1 1 1) #t #f 'char) '("  abcd" "     efg"))
   (list
    (Diff-item
     (Region (Scope (Point 0 0 0) (Point 0 2 2) #t #f 'char) '("  "))
     (Region (Scope (Point 0 0 0) (Point 0 0 0) #t #f 'char) '()))
    (Diff-item
     (Region (Scope (Point 1 0 0) (Point 1 4 4) #t #f 'char) '("    "))
     (Region (Scope (Point 1 1 1) (Point 1 1 1) #t #f 'char) '())))))
