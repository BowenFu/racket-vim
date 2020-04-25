#lang racket

(provide (all-defined-out))

(require "core.rkt" "wrapped-move-scope.rkt" "change.rkt" "params.rkt" "diff-utils.rkt" "reg-manager.rkt")

(module+ test (require rackunit))

(define (delete-op scope p lines reg-manager)
  (define-values (new-point new-lines) (delete-scope scope lines))
  (define deleted-lines (Scoped-lines scope lines))
  (send reg-manager set-yank-reg (make-Reg deleted-lines (Scope-mode scope)))
  (define diffs (get-delete-diffs scope lines))
  (values new-point new-lines diffs))

(module+ test
  (let-values ([(p lines diffs)
                (delete-op (Scope (Point 0 0 0) (Point 0 0 0) #t #t 'line)
                           (Point 0 0 0) '("one another.") (new reg-manager%))])
    (check-equal? p (Point 0 0 0))
    (check-equal? lines '(""))
    (check-equal? diffs (list (Diff-item
                               (Region (Scope (Point 0 0 0) (Point 0 0 0) #t #t 'line) '("one another."))
                               (Region (Scope (Point 0 0 0) (Point 0 0 0) #t #f 'line) '())))))
  (let-values ([(p lines diffs)
                (delete-op (Scope (Point 0 0 0) (Point 0 0 0) #t #t 'char)
                           (Point 0 0 0) '("one another.") (new reg-manager%))])
    (check-equal? p (Point 0 0 0))
    (check-equal? lines '("ne another."))
    (check-equal? diffs (list (Diff-item
                               (Region (Scope (Point 0 0 0) (Point 0 0 0) #t #t 'char) '("o"))
                               (Region (Scope (Point 0 0 0) (Point 0 0 0) #t #f 'char) '())))))
  (let-values ([(p lines diffs)
                (delete-op (Scope (Point 0 0 0) (Point 0 0 0) #t #t 'block)
                           (Point 0 0 0) '("one another.") (new reg-manager%))])
    (check-equal? p (Point 0 0 0))
    (check-equal? lines '("ne another."))
    (check-equal? diffs (list (Diff-item
                               (Region (Scope (Point 0 0 0) (Point 0 0 0) #t #t 'block) '("o"))
                               (Region (Scope (Point 0 0 0) (Point 0 0 0) #t #f 'block) '()))))))

(define (yank-op scope p lines reg-manager)
  (define-values (new-point new-lines) (delete-scope scope lines))
  (define yanked-lines (Scoped-lines scope lines))
  (send reg-manager set-yank-reg (make-Reg yanked-lines (Scope-mode scope)))
  (values p lines '()))

(module+ test
  (let ([reg-manager (new reg-manager%)])
    (define-values (_ __ ___)
      (yank-op (Scope (Point 0 0 0) (Point 0 0 0) #t #t 'line)
               (Point 0 0 0) '("one another.") reg-manager))
    (check-equal? (send reg-manager get-yank-reg) (Reg '("one another.") 'line))))
  
(define (replace scope p lines inserted-lines mode)
  (define updated-scope (struct-copy Scope scope [mode mode]))
  (define start (Scope-start updated-scope))
  (define-values (new-point new-lines)
    (replace-scope updated-scope lines inserted-lines))
  (define replace-diffs
    (get-replace-diffs updated-scope start inserted-lines mode lines))
  (values new-point new-lines replace-diffs))

(module+ test
  (let-values ([(p lines diffs)
                (replace (Scope (Point 0 0 0) (Point 0 1 1) #t #f 'line)
                         (Point 0 4 4) '("one another.")
                         '("x.")
                         'line)])
    (check-equal? p (Point 0 0 0))
    (check-equal? lines '("x." "one another."))
    (check-equal? diffs     (list
                             (Diff-item
                              (Region (Scope (Point 0 0 0) (Point 0 1 1) #t #f 'line) '())
                              (Region (Scope (Point 0 0 0) (Point 1 0 0) #t #f 'line) '("x."))))))
  (let-values ([(p lines diffs)
                (replace (Scope (Point 0 0 0) (Point 0 1 1) #t #f 'char)
                         (Point 0 4 4) '("one another.")
                         '("x.")
                         'char)])
    (check-equal? p (Point 0 2 2))
    (check-equal? lines '("x.ne another."))
    (check-equal? diffs (list
                         (Diff-item
                          (Region (Scope (Point 0 0 0) (Point 0 1 1) #t #f 'char) '("o"))
                          (Region (Scope (Point 0 0 0) (Point 0 2 2) #t #f 'char) '("x."))))))
  (let-values ([(p lines diffs)
                (replace (Scope (Point 0 0 0) (Point 1 1 1) #t #f 'block)
                         (Point 0 4 4) '("one" "another.")
                         '("x." "y.")
                         'block)])
    (check-equal? p (Point 0 0 0))
    (check-equal? lines '("x.ne" "y.nother."))
    (check-equal? diffs
                  (list
                   (Diff-item
                    (Region (Scope (Point 0 0 0) (Point 1 1 1) #t #f 'block) '("o" "a"))
                    (Region (Scope (Point 0 0 0) (Point 1 2 2) #t #f 'block) '("x." "y.")))))))

(define (pre-paste-op scope p lines reg-manager)
  (define reg (send reg-manager get-yank-reg))
  (define inserted-lines (Reg-lines reg))
  (define mode (Reg-mode reg))
  (define deleted-lines (Scoped-lines scope lines))
  (when (not (empty-lines? deleted-lines))
    (send reg-manager set-yank-reg (make-Reg deleted-lines mode)))
  (replace scope p lines inserted-lines mode))

(module+ test
  (let ([reg-manager (new reg-manager%)])
    (let-values ([(p lines diffs)
                  (pre-paste-op (Scope (Point 0 0 0) (Point 1 1 1) #t #t 'block)
                                (Point 0 4 4) '("two" "another.") reg-manager)])
      (check-equal? p (Point 0 0 0))
      (check-equal? lines '("other."))
      (check-equal? diffs
                    (list
                     (Diff-item
                      (Region (Scope (Point 0 0 0) (Point 1 1 1) #t #t 'char) '("two" "an"))
                      (Region (Scope (Point 0 0 0) (Point 0 0 0) #t #f 'char) '())))))))

(define (post-paste-op scope p lines reg-manager)
  (define mode (Reg-mode (send reg-manager get-yank-reg)))
  (define start (Scope-start scope))
  (define updated-scope
    (cond
      [(not (empty-scope? scope)) scope]
      [else
       (define updated-start
           (cond
             [(equal? mode 'line) (Point (add1 (Point-row start)) 0 0)]
             [else (move-point (make-Motion 'right*) start lines)]))
       (struct-copy Scope scope [start updated-start] [end updated-start] [mode mode])]))
  (pre-paste-op updated-scope p lines reg-manager))

(module+ test
  (let ([reg-manager (new reg-manager%)])
    (let-values ([(p lines diffs)
                  (post-paste-op (Scope (Point 0 0 0) (Point 1 1 1) #t #t 'block)
                                 (Point 0 4 4) '("two" "another.") reg-manager)])
      (check-equal? p (Point 0 0 0))
      (check-equal? lines '("other."))
      (check-equal? diffs
                    (list
                     (Diff-item
                      (Region (Scope (Point 0 0 0) (Point 1 1 1) #t #t 'char) '("two" "an"))
                      (Region (Scope (Point 0 0 0) (Point 0 0 0) #t #f 'char) '())))))))

(define (right-shift-op scope p lines)
  (define diffs (get-right-shift-diffs scope lines))
  (define-values (new-point new-lines) (right-shift-scope scope lines))
  (values new-point new-lines diffs))

(define (left-shift-op scope p lines)
  (define diffs (get-left-shift-diffs scope lines))
  (define-values (new-point new-lines) (left-shift-scope scope lines))
  (values new-point new-lines diffs))

(module+ test
  (let-values ([(p lines diffs)
                (right-shift-op (Scope (Point 0 0 0) (Point 0 0 0) #t #f 'block)
                                (Point 0 4 4) '("two" "another."))])
    (check-equal? p (Point 0 4 4))
    (check-equal? lines '("    two" "another."))
    (check-equal? diffs
                  (list
                   (Diff-item
                    (Region (Scope (Point 0 0 0) (Point 0 0 0) #t #f 'char) '())
                    (Region (Scope (Point 0 0 0) (Point 0 4 4) #t #f 'char) '("    "))))))
  (let-values ([(p lines diffs)
                (left-shift-op (Scope (Point 0 0 0) (Point 1 0 0) #t #f 'block)
                               (Point 0 4 4) '("  two" "      another."))])
    (check-equal? p (Point 0 0 0))
    (check-equal? lines '("two" "  another."))
    (check-equal? diffs
                  (list
                   (Diff-item
                    (Region (Scope (Point 0 0 0) (Point 0 2 2) #t #f 'char) '("  "))
                    (Region (Scope (Point 0 0 0) (Point 0 0 0) #t #f 'char) '()))
                   (Diff-item
                    (Region (Scope (Point 1 0 0) (Point 1 4 4) #t #f 'char) '("    "))
                    (Region (Scope (Point 1 2 2) (Point 1 2 2) #t #f 'char) '()))))))

(define (g-proc scope p lines proc)
  (define scoped (Scoped-lines scope lines))
  (define replaced-lines (map proc scoped))
  (replace scope p lines replaced-lines (Scope-mode scope)))

(define (char-swapcase char)
  (if
   (char-upper-case? char)
   (char-downcase char)
   (char-upcase char)))

(define (string-swapcase str)
  (list->string
   (map char-swapcase (string->list str))))

(define (g~-op scope p lines)
  (g-proc scope p lines string-swapcase))

(define (gu-op scope p lines)
  (g-proc scope p lines string-downcase))

(define (gU-op scope p lines)
  (g-proc scope p lines string-upcase))

(define (replace-op scope p lines char)
  (define (char-string str)
    (make-string (string-length str) char))
  (g-proc scope p lines char-string))

(module+ test
  (let-values ([(p lines diffs)
                (g~-op (Scope (Point 0 1 1) (Point 1 2 2) #t #f 'block)
                       (Point 0 4 4) '("two" "another."))])
    (check-equal? p (Point 0 1 1))
    (check-equal? lines '("tWo" "aNother."))
    (check-equal? diffs
                  (list
                   (Diff-item
                    (Region (Scope (Point 0 1 1) (Point 1 2 2) #t #f 'block) '("w" "n"))
                    (Region (Scope (Point 0 1 1) (Point 1 2 2) #t #f 'block) '("W" "N"))))))
  (let-values ([(p lines diffs)
                (gU-op (Scope (Point 0 1 1) (Point 1 5 5) #t #f 'block)
                       (Point 0 4 4) '("two" "another."))])
    (check-equal? p (Point 0 1 1))
    (check-equal? lines '("tWO" "aNOTHer."))
    (check-equal? diffs
                  (list
                   (Diff-item
                    (Region (Scope (Point 0 1 1) (Point 1 5 5) #t #f 'block) '("wo" "noth"))
                    (Region (Scope (Point 0 1 1) (Point 1 5 5) #t #f 'block) '("WO" "NOTH"))))))
  (let-values ([(p lines diffs)
                (gu-op (Scope (Point 0 1 1) (Point 1 5 5) #t #f 'block)
                       (Point 0 4 4) '("TWO" "ANOTHER."))])
    (check-equal? p (Point 0 1 1))
    (check-equal? lines '("Two" "AnothER."))
    (check-equal? diffs
                  (list
                   (Diff-item
                    (Region (Scope (Point 0 1 1) (Point 1 5 5) #t #f 'block) '("WO" "NOTH"))
                    (Region (Scope (Point 0 1 1) (Point 1 5 5) #t #f 'block) '("wo" "noth"))))))
  (let-values ([(p lines diffs)
                (gU-op (Scope (Point 1 7 7) (Point 2 8 8) #t #t 'block)
                       (Point 1 7 7)'("so were thee" "of Atreus, " "another."))])
    (check-equal? p (Point 1 7 7))
    (check-equal? lines '("so were thee" "of AtreUS, " "another."))
    (check-equal? diffs
                  (list
                   (Diff-item
                    (Region (Scope (Point 1 7 7) (Point 2 8 8) #t #t 'block) '("us" "."))
                    (Region (Scope (Point 1 7 7) (Point 2 9 9) #t #f 'block) '("US" ".")))))))

(define (insert pp inserted-lines lines mode)
    (define-values (new-p new-lines) (insert-lines-at pp lines inserted-lines mode))
    (define insert-diffs (get-insert-diffs pp inserted-lines mode))
    (values new-p new-lines insert-diffs))

(module+ test
  (let-values ([(p lines diffs)
                (insert (Point 0 1 1)
                        '("123" "456")
                        '("two" "another.")
                        'char)])
    (check-equal? p (Point 1 3 3))
    (check-equal? lines '("t123" "456wo" "another."))
    (check-equal? diffs
                  (list
                   (Diff-item
                    (Region (Scope (Point 0 1 1) (Point 0 1 1) #t #f 'char) '())
                    (Region
                     (Scope (Point 0 1 1) (Point 1 3 3) #t #f 'char)
                     '("123" "456"))))))
  (let-values ([(p lines diffs)
                (insert (Point 0 1 1)
                        '("123" "456")
                        '("two" "another.")
                        'block)])
    (check-equal? p (Point 0 1 1))
    (check-equal? lines '("t123wo" "a456nother."))
    (check-equal? diffs
                  (list
                   (Diff-item
                    (Region (Scope (Point 0 1 1) (Point 0 1 1) #t #f 'block) '())
                    (Region (Scope (Point 0 1 1) (Point 1 4 4) #t #f 'block) '("123" "456")))))))

; todo Join 'J

(define (key-to-operator char)
  (match char
    ['y yank-op]
    ['d delete-op]
    ['x delete-op]
    ['p post-paste-op]
    ['P pre-paste-op]
    ['> right-shift-op]
    ['< left-shift-op]
    ['c 'change-op]
    [_ (error (~e 'missing-case-in-key-to-operator char))]))

(define (key-to-operator-without-prefix char [throw? #t])
  (match char
    ['y yank-op]
    ['d delete-op]
    ['x delete-op]
    ['p post-paste-op]
    ['P pre-paste-op]
    ['> right-shift-op]
    ['< left-shift-op]
    ['c 'change-op]
    ['u gu-op]
    ['U gU-op]
    ['~ g~-op]
    [_ (and throw? (error (~e 'missing-case-in-key-to-operator-without-prefix char)))]))

(define (key-to-g-op k)
  (match k
    ['g~ g~-op]
    ['gu gu-op]
    ['gU gU-op]
    [_ (error 'missing-case-in-key-to-g-op)]))

