#lang racket

(provide (all-defined-out))

(require "core.rkt" "wrapped-move-scope.rkt" "scope.rkt" "diff.rkt" "params.rkt" "change.rkt")

(module+ test (require rackunit))

(define (from-mode diffs)
  ;(displayln (~e 'from-mode diffs))
  (cond
    [(empty? diffs) 'char]
    [else
     (define scope (Region-scope (Diff-item-old-region (last diffs))))
     (Scope-mode scope)]))

(define (insert-escape! b leftest-point start-point org-lines start-motions-lst change-motions diff-lst diff-manager reg-manager count)
  (define mode (from-mode diff-lst))
  (define p (Buffer-cur b))
  (define lines (Buffer-lines b))
  (define real-count (or count 1))
  (define-values (new-p new-lines new-diffs)
    (cond
      [(equal? mode 'char)
       (insert-escape-char-mode p lines leftest-point start-point org-lines start-motions-lst change-motions diff-lst real-count)]
      [(equal? mode 'line)
       (insert-escape-line-mode p lines leftest-point start-point org-lines start-motions-lst change-motions diff-lst real-count)]
      [(equal? mode 'block)
       (insert-escape-block-mode p lines leftest-point start-point org-lines start-motions-lst change-motions diff-lst real-count)]))
  (set-Buffer-cur! b new-p)
  (set-Buffer-lines! b new-lines)
  (send diff-manager push-diffs! new-diffs)
  (define inserted-lines (Region-lines (Diff-item-new-region (first new-diffs))))
  (send reg-manager set-last-cmd (make-Command 'change change-motions #:op-params inserted-lines #:start-motions-lst start-motions-lst)))

(define (insert-escape-line-mode p lines leftest-point start-point org-lines start-motions-lst change-motions diff-lst count)
  (insert-escape-char-mode p lines leftest-point start-point org-lines start-motions-lst change-motions diff-lst count))

(define (repeat-lines-char-mode lines count)
  (for/fold ([new-lines '()])
            ([i count])
   (define-values (_ n-lines) (insert-lines-at (Point 0 0 0) new-lines lines 'char))
    n-lines))

(define (insert-escape-char-mode p lines leftest-point start-point org-lines start-motions-lst change-motions diff-lst count)
  (define new-scope (Scope leftest-point p #t #f 'char))
  (define inserted-lines (Scoped-lines new-scope lines))
  (define repeated-inserted-lines (repeat-lines-char-mode inserted-lines count))
  (define repeated-new-point (after-lines-point p repeated-inserted-lines 'char))
  (define repeated-new-scope (Scope leftest-point repeated-new-point #t #f 'char))
  (define new-region (Region repeated-new-scope repeated-inserted-lines))
  (define old-scope (Scope leftest-point start-point #t #f 'char))
  (define deleted-lines (Scoped-lines old-scope org-lines))
  (define old-region (Region old-scope deleted-lines))
  (define new-diff (Diff-item old-region new-region))
  (define-values (new-p new-lines) (redo new-diff org-lines))
  (define diffs (cons new-diff diff-lst))
  (values new-p new-lines diffs))

(define (sublist list start [end #f])
  (define sublist1 (if end (take list end) list))
  (drop sublist1 start))

(define (insert-escape-block-mode p lines leftest-point start-point org-lines start-motions-lst change-motions diff-lst count)
  ;(displayln (~e 'insert-escape-block-mode p lines leftest-point start-point org-lines start-motions-lst change-motions diff-lst))
  (define deleted-scope (Region-scope (Diff-item-old-region (last diff-lst))))
  (match-define (Scope start end _ _ _) deleted-scope)
  (unless (equal? start start-point) (error (~e 'unmatched-scope start "!=" start-point)))
  (define row-min (min (Point-row start) (Point-row end)))
  (define row-max (max (Point-row start) (Point-row end)))
  (define leftest-col (Point-col leftest-point))
  (define start-col (Point-col start-point))
  (define cur-col (Point-col p))
  (define inserted-line (substring (list-ref lines row-min) leftest-col cur-col))
  (define repeated-inserted-line
    (for/fold ([line ""])
              ([i count])
      (string-append line inserted-line)))
  (define inserted-lines
    (for/list
        ([l (sublist lines row-min (add1 row-max))])
      (cond
        [(> start-col (string-length l))
         ""]
        [else repeated-inserted-line])))
  (define new-col (+ start-col (string-length repeated-inserted-line)))
  (define new-scope (Scope leftest-point (Point row-max new-col new-col) #t #f 'block))
  (define new-region (Region new-scope inserted-lines))
  (define old-scope (Scope leftest-point (struct-copy Point start-point [row row-max]) #t #f 'block))
  (define deleted-line (substring (list-ref org-lines row-min) leftest-col start-col))
  (define row-num (- row-max row-min -1))
  (define deleted-lines (make-list row-num deleted-line))
  (define old-region (Region old-scope deleted-lines))
  (define new-diff (Diff-item old-region new-region))
  (define-values (new-p new-lines) (redo new-diff org-lines))
  (define diffs (cons new-diff diff-lst))
  (values p new-lines diffs)) ; be consistent with char/line modes.

(module+ test
  (let-values ([(new-p new-lines new-diffs)
                (insert-escape-block-mode (Point 0 3 3)
                                          '("xyz of Atreus" " another.")
                                          (Point 0 0 0)
                                          (Point 0 0 0)
                                          '(" of Atreus" " another.")
                                          '()
                                          (Visual-Motion 1 3 'block)
                                          (list (Diff-item
                                                 (Region (Scope (Point 0 0 0) (Point 1 3 3) #t #f 'block) '("son" "one"))
                                                 (Region (Scope (Point 0 0 0) (Point 0 0 0) #t #f 'block) '())))
                                          1)])
    (check-equal? new-p (Point 0 3 3))
    (check-equal? new-lines '("xyz of Atreus" "xyz another."))
    (check-equal? new-diffs
                  (list
                   (Diff-item
                    (Region (Scope (Point 0 0 0) (Point 1 0 0) #t #f 'block) '("" ""))
                    (Region (Scope (Point 0 0 0) (Point 1 3 3) #t #f 'block) '("xyz" "xyz")))
                   (Diff-item
                    (Region (Scope (Point 0 0 0) (Point 1 3 3) #t #f 'block) '("son" "one"))
                    (Region (Scope (Point 0 0 0) (Point 0 0 0) #t #f 'block) '()))))
    ))


(define (insert-key-to-scope-motion k)
  (match k
    ['C '$]
    ['s 'right*]
    ['S '$*]
    ['i 'nope]
    ['a 'nope]
    ['I 'nope]
    ['A 'nope]
    ['o 'nope]
    ['O 'nope]
    [_ (error (~e 'missing-case-in-insert-key-to-scope-motion k))]))

(define (insert-key-to-start-motion-lst k)
  (match k
    ['C (list 'nope)]
    ['s (list 'nope)]
    ['S (list '|0|)]
    ['i (list 'nope)]
    ['a (list 'right*)]
    ['I (list '^)]
    ['A (list '$*)]
    ['o (list '$*)]
    ['O (list 'up '$*)]
    [_ (error (~e 'missing-case-in-insert-key-to-start-motion-lst k))]))
