#lang typed/racket
(require "core.rkt" "move.rkt" "wrapped-move-scope.rkt" "common-utils.rkt")

(provide (all-defined-out))

(module+ test (require typed/rackunit))

; line-insert-char! : line char index -> void
;   insert char c in the line l at index i
(: line-insert-char (-> String Char Natural String))
(define (line-insert-char s c i)
  (define n (string-length s))
  (unless (<= i n) (error 'line-insert-char "index i greater than line length, i=~a, l=~a" i s))
  (cond [(= i n) (string-append s (string c))]
        [(= i 0) (string-append (string c) s)]
        [else    (string-append (substring s 0 i) (string c) (substring s i))]))

(module+ test
  (check-equal? (line-insert-char "abc" #\d 0) "dabc")
  (check-equal? (line-insert-char "abc" #\d 1) "adbc")
  (check-equal? (line-insert-char "abc" #\d 2) "abdc")
  (check-equal? (line-insert-char "abc" #\d 3) "abcd")
  (check-exn exn:fail? (lambda () (line-insert-char "abc" #\d 4))))

; line-replace-char : string char index -> string
;   replace in the line l at index i
(: line-replace-after-char (-> String Char Natural String))
(define (line-replace-after-char s c i)
  (define n (string-length s))
  (unless (<= i n) (error 'line-replace-char "index i greater than line length, i=~a, l=~a" i s))
  (string-append (substring s 0 i) (string c) (substring s (min (+ 1 i) n))))

(module+ test
  (check-equal? (line-replace-after-char "abc" #\d 0) "dbc")
  (check-equal? (line-replace-after-char "abc" #\d 1) "adc")
  (check-equal? (line-replace-after-char "abc" #\d 2) "abd")
  (check-equal? (line-replace-after-char "abc" #\d 3) "abcd")
  (check-exn exn:fail? (lambda () (line-replace-after-char "abc" #\d 4))))

; line-delete-after-char : string index -> string
;   delete in the line l at index i
(: line-delete-after-char (-> String Natural (Option String)))
(define (line-delete-after-char s i)
  (define n (string-length s))
  (unless (<= i n) (error 'line-delete-after-char "index i greater than line length, i=~a, l=~a" i s))
  (cond [(= i 0) #f] ; or error
        [else    (string-append (substring s 0 (- i 1)) (substring s i))]))

(module+ test
  (check-equal? (line-delete-after-char "abc" 0) #f)
  (check-equal? (line-delete-after-char "abc" 1) "bc")
  (check-equal? (line-delete-after-char "abc" 2) "ac")
  (check-equal? (line-delete-after-char "abc" 3) "ab"))

(: split-line-at (-> Point (Listof String) (Listof String)))
(define (split-line-at p lines)
  (define-values (row col) (Point-row-col p))
  (define-values (l0 l123) (split-at lines row))
  (define-values (l12* l3) (split-at l123 1))
  (define l12 (first l12*))
  (define l1 (substring l12 0 col))
  (define l2 (substring l12 col))
  (append l0 (list l1 l2) l3))

(module+ test
  (check-equal? (split-line-at (Point 0 0 0) (list "abc")) '("" "abc"))
  (check-equal? (split-line-at (Point 0 1 1) (list "abc")) '("a" "bc"))
  (check-equal? (split-line-at (Point 0 2 2) (list "abc")) '("ab" "c"))
  (check-equal? (split-line-at (Point 0 3 3) (list "abc")) '("abc" "")))

(: split-line-point-lines (-> Point (Listof String) (values Point (Listof String))))
(define (split-line-point-lines p lines)
  (define updated-lines (split-line-at p lines))
  (define-values (row col) (Point-row-col p))
  (values (down-point (line-start-point row) updated-lines) updated-lines))

(module+ test
  (let-values ([(p lines) (split-line-point-lines (Point 0 0 0) (list "abc"))])
    (check-equal? p (Point 1 0 0))
    (check-equal? lines '("" "abc")))
  (let-values ([(p lines) (split-line-point-lines (Point 0 1 1) (list "abc"))])
    (check-equal? p (Point 1 0 0))
    (check-equal? lines '("a" "bc")))
  (let-values ([(p lines) (split-line-point-lines (Point 0 2 2) (list "abc"))])
    (check-equal? p (Point 1 0 0))
    (check-equal? lines '("ab" "c")))
  (let-values ([(p lines) (split-line-point-lines (Point 0 3 3) (list "abc"))])
    (check-equal? p (Point 1 0 0))
    (check-equal? lines '("abc" ""))))

(: lines-insert-char-at-point (-> Char Point (Listof String) (Listof String)))
(define (lines-insert-char-at-point c p lines)
  (define-values (row col) (Point-row-col p))
  (define l (list-ref lines row))
  (list-set lines row (line-insert-char l c col)))

(module+ test
  (check-equal? (lines-insert-char-at-point #\d (Point 0 0 0) (list "abc")) '("dabc"))
  (check-equal? (lines-insert-char-at-point #\d (Point 0 1 1) (list "abc")) '("adbc"))
  (check-equal? (lines-insert-char-at-point #\d (Point 0 2 2) (list "abc")) '("abdc"))
  (check-equal? (lines-insert-char-at-point #\d (Point 0 3 3) (list "abc")) '("abcd")))

(: lines-replace-char-after-point (-> Char Point (Listof String) (Listof String)))
(define (lines-replace-char-after-point c p lines)
  (define-values (row col) (Point-row-col p))
  (define l (list-ref lines row))
  (list-set lines row (line-replace-after-char l c col)))

(module+ test
  (check-equal? (lines-replace-char-after-point #\d (Point 0 0 0) (list "abc")) '("dbc"))
  (check-equal? (lines-replace-char-after-point #\d (Point 0 1 1) (list "abc")) '("adc"))
  (check-equal? (lines-replace-char-after-point #\d (Point 0 2 2) (list "abc")) '("abd"))
  (check-equal? (lines-replace-char-after-point #\d (Point 0 3 3) (list "abc")) '("abcd")))

(: lines-delete-char-after-point (-> Point (Listof String) Boolean (values Point (Listof String))))
(define (lines-delete-char-after-point p lines *-mode?)
  (define-values (row col) (Point-row-col p))
  (define l (list-ref lines row))
  (with-handlers ([exn:fail?
                   (λ (e) (values ((if *-mode? after-line-end-point line-end-point) row l) lines))])
    (define new-point (if (= col ((if *-mode? after-line-end-col line-end-col) l))
                          (left-point p)
                          p))
    (define new-line
      (cast
       (line-delete-after-char l (+ col 1))
       String))
    (define new-lines (list-set lines row new-line))
    (values new-point new-lines)))

(module+ test
  (let-values ([(p lines) (lines-delete-char-after-point (Point 0 0 0) (list "abc") #f)])
    (check-equal? p (Point 0 0 0))
    (check-equal? lines '("bc")))
  (let-values ([(p lines) (lines-delete-char-after-point (Point 0 1 1) (list "abc") #f)])
    (check-equal? p (Point 0 1 1))
    (check-equal? lines '("ac")))
  (let-values ([(p lines) (lines-delete-char-after-point (Point 0 2 2) (list "abc") #f)])
    (check-equal? p (Point 0 1 1))
    (check-equal? lines '("ab")))
  (let-values ([(p lines) (lines-delete-char-after-point (Point 0 3 3) (list "abc") #f)])
    (check-equal? p (Point 0 2 +inf.0))
    (check-equal? lines '("abc"))))

(: delete-scope (-> Scope (Listof String) (values Point (Listof String))))
(define (delete-scope scope lines)
  (replace-scope scope lines '()))

(module+ test
  (let-values ([(p lines) (delete-scope (Scope (Point 0 0 0) (Point 0 1 1) #f #f 'char) (list "abc"))])
    (check-equal? p (Point 0 0 0))
    (check-equal? lines '("bc")))
  (let-values ([(p lines) (delete-scope (Scope (Point 0 0 0) (Point 0 1 1) #f #t 'char) (list "abc"))])
    (check-equal? p (Point 0 0 0))
    (check-equal? lines '("bc")))
  (let-values ([(p lines) (delete-scope (Scope (Point 0 0 0) (Point 1 1 1) #f #t 'char) (list "abc" "def"))])
    (check-equal? p (Point 0 0 0))
    (check-equal? lines '("ef")))
  (let-values ([(p lines) (delete-scope (Scope (Point 0 1 1) (Point 1 1 1) #f #t 'char) (list "abc" "def"))])
    (check-equal? p (Point 0 1 1))
    (check-equal? lines '("aef")))
  (let-values ([(p lines) (delete-scope (Scope (Point 0 0 0) (Point 0 1 1) #f #f 'line) (list "abc"))])
    (check-equal? p (Point 0 0 0))
    (check-equal? lines '("abc")))
  (let-values ([(p lines) (delete-scope (Scope (Point 0 0 0) (Point 0 1 1) #f #t 'line) (list "abc"))])
    (check-equal? p (Point 0 0 0))
    (check-equal? lines '("")))
  (let-values ([(p lines) (delete-scope (Scope (Point 0 0 0) (Point 1 1 1) #f #t 'line) (list "abc"))])
    (check-equal? p (Point 0 0 0))
    (check-equal? lines '("")))
  (let-values ([(p lines) (delete-scope (Scope (Point 1 1 1) (Point 1 1 1) #f #t 'line) (list "abc" "def"))])
    (check-equal? p (Point 0 0 0))
    (check-equal? lines '("abc"))))

(: insert-lines-at (->* (Point (Listof String) (Listof String)) (Symbol) (values Point (Listof String))))
(define (insert-lines-at start lines inserted-lines [mode 'char])
  (define scope (Scope start start #f #f mode))
  (replace-scope scope lines inserted-lines))

(module+ test
  (let-values ([(lines p) (insert-lines-at (Point 0 0 0) '() '())])
    (check-equal? lines (Point 0 0 0))
    (check-equal? p '("")))
  (let-values ([(lines p) (insert-lines-at (Point 0 0 0) '() '(""))])
    (check-equal? lines (Point 0 0 0))
    (check-equal? p '("")))
  (let-values ([(lines p) (insert-lines-at (Point 0 0 0) '() '("abc"))])
    (check-equal? lines (Point 0 3 3))
    (check-equal? p '("abc")))
  (let-values ([(lines p) (insert-lines-at (Point 0 0 0) (list "") '())])
    (check-equal? lines (Point 0 0 0))
    (check-equal? p '("")))
  (let-values ([(lines p) (insert-lines-at (Point 0 0 0) (list "") (list ""))])
    (check-equal? lines (Point 0 0 0))
    (check-equal? p '("")))
  (let-values ([(lines p) (insert-lines-at (Point 0 0 0) (list "") (list "def"))])
    (check-equal? lines (Point 0 3 3))
    (check-equal? p '("def")))
  (let-values ([(lines p) (insert-lines-at (Point 0 0 0) (list " ") (list "def"))])
    (check-equal? lines (Point 0 3 3))
    (check-equal? p '("def ")))
  (let-values ([(lines p) (insert-lines-at (Point 0 0 0) (list "abc") (list "def"))])
    (check-equal? lines (Point 0 3 3))
    (check-equal? p '("defabc")))
  (let-values ([(lines p) (insert-lines-at (Point 0 1 1) (list "abc") (list "def"))])
    (check-equal? lines (Point 0 4 4))
    (check-equal? p '("adefbc")))
  (let-values ([(lines p) (insert-lines-at (Point 0 2 2) (list "abc" "123") (list "def"))])
    (check-equal? lines (Point 0 5 5))
    (check-equal? p '("abdefc" "123")))
  (let-values ([(lines p) (insert-lines-at (Point 0 3 3) (list "abc" "123") (list "def"))])
    (check-equal? lines (Point 0 6 6))
    (check-equal? p '("abcdef" "123")))
  (let-values ([(lines p) (insert-lines-at (Point 1 2 2) (list "abc" "123") (list "def"))])
    (check-equal? lines (Point 1 5 5))
    (check-equal? p '("abc" "12def3")))
  (let-values ([(lines p) (insert-lines-at (Point 1 0 0) (list "abc" "123") (list "") 'line)])
    (check-equal? lines (Point 1 0 0))
    (check-equal? p '("abc" "" "123")))
  (let-values ([(lines p) (insert-lines-at (Point 0 0 0) (list "abc" "123") (list "def") 'line)])
    (check-equal? lines (Point 0 0 0))
    (check-equal? p '("def" "abc" "123")))
  (let-values ([(lines p) (insert-lines-at (Point 1 1 1) (list "abc" "123") (list "def") 'line)])
    (check-equal? lines (Point 1 0 0))
    (check-equal? p '("abc" "def" "123")))
  (let-values ([(lines p) (insert-lines-at (Point 2 2 2) (list "abc" "123") (list "def") 'line)])
    (check-equal? lines (Point 2 0 0))
    (check-equal? p '("abc" "123" "def"))))

(: replace-line-scope (-> Scope (Listof String) (Listof String) (values Point (Listof String))))
(define (replace-line-scope scope lines inserted-lines)
  (match-define (Scope start end dir include-real-end? mode) scope)
  ; for non-line-mode, we include right end only with both dir and include-real-end? true.
  (define include-right-end? (and dir include-real-end?))
  (define l0 (take lines (Point-row start)))
  (define real-end-row (+ (Point-row end) (if include-real-end? 1 0)))
  (define l3 (if (>= (length lines) real-end-row) (drop lines real-end-row) '()))
  (define new-lines (append l0 inserted-lines l3))
  (define real-lines (if (empty? new-lines) (list "") new-lines))
  (define new-row
    (cast
     (min (sub1 (length real-lines)) (+ (Point-row start) (max 1 (length inserted-lines)) -1))
     Natural))
  (define new-point
    (line-start-point new-row))
  (values new-point real-lines))
  
(: replace-scope (-> Scope (Listof String) (Listof String) (values Point (Listof String))))
(define (replace-scope scope lines inserted-lines)
  (define mode (Scope-mode scope))
  (cond
    [(equal? mode 'line)
     (replace-line-scope scope lines inserted-lines)]
    [(equal? mode 'char)
     (replace-char-scope scope lines inserted-lines)]
    [(equal? mode 'block)
     (replace-block-scope scope lines inserted-lines)]
    [else (error 'missing-case-of-scope-mode)]))
  
(: replace-char-scope (-> Scope (Listof String) (Listof String) (values Point (Listof String))))
(define (replace-char-scope scope lines inserted-lines)
  (match-define (Scope start end dir include-real-end? mode) scope)
  ; for non-line-mode, we include right end only with both dir and include-real-end? true.
  (define include-right-end? (and dir include-real-end?))
  (define-values (l0 l1 _ l3 l4) (split-five-at lines (Point-row start) (Point-row end)))
  (define new-lines
    (cond
      [(>= (length inserted-lines) 2)
       (define-values (r1 r2 r3) (first-middle-last inserted-lines))
       (define m1 (line-merge (first-or-empty-string l1)
                              (Point-col start)
                              r1
                              0))
       (define end-col (+ (Point-col end) (if include-right-end? 1 0)))
       (define m3 (line-merge r3
                              (string-length r3)
                              (first-or-empty-string l3)
                              end-col))
       (append l0 (list m1) r2 (list m3) l4)]
      [else 
       (define l123-b (first-or-empty-string inserted-lines))
       (define m1 (line-merge (first-or-empty-string l1)
                              (Point-col start)
                              l123-b
                              0))
       (define end-col (+ (Point-col end) (if include-right-end? 1 0)))
       (define m (line-merge m1
                             (string-length m1)
                             (first-or-empty-string l3)
                             end-col))
       (append l0 (list m) l4)])
    )
  (define real-lines (if (empty? new-lines) (list "") new-lines))
  (define new-row
    (cast
     (min (sub1 (length real-lines)) (+ (Point-row start) (max 1 (length inserted-lines)) -1))
     Natural))
  (define new-point
    (after-lines-point start inserted-lines))
  (values new-point real-lines))

(: replace-block-scope (-> Scope (Listof String) (Listof String) (values Point (Listof String))))
(define (replace-block-scope scope lines inserted-lines)
  ;(displayln (~e 'replace-block-scope scope lines inserted-lines))
  (match-define (Scope start end dir include-real-end? mode) scope)
  (define row-diff (- (Point-row end) (Point-row start)))
  (define row-len (if (empty-scope? scope) (length inserted-lines) (add1 row-diff)))
  ;(displayln (~e 'start start 'end end 'row-len row-len 'inserted-lines inserted-lines))
  (unless (or (empty? inserted-lines) (= (length inserted-lines) row-len))
    (error (~e "incorrect-params" row-len inserted-lines)))
  ; for non-line-mode, we include right end only with both dir and include-real-end? true.
  (define include-right-end? (and dir include-real-end?))
  (define end-row+1 (cast (+ (Point-row start) row-len) Natural))
  (define-values (before middle after) (before-middle-after lines (Point-row start) end-row+1))
  (define col0 (Point-col start))
  (define col1 (Point-col end))
  (define col-min (min col0 col1))
  (define col-max (max col0 col1))
  (define real-col-end (if include-right-end? (add1 col-max) col-max))
  (define real-inserted-lines (if (empty? inserted-lines) (make-list row-len "") inserted-lines))
  (define new-middle
    (for/list : (Listof String)
      ([l middle]
       [ins real-inserted-lines])
      (string-append
       (substring-in-range l 0 col-min)
       ins
       (substring-in-range l real-col-end))))
  ;(displayln (~e 'middle middle 'inserted 'inserted-lines 'new-middle new-middle))
  (define new-lines (append before new-middle after))
  (define real-lines (if (empty? new-lines) (list "") new-lines))
  (values start real-lines))

(module+ test
  (let-values ([(p lines) (replace-block-scope (Scope (Point 0 1 1) (Point 0 1 1) #t #f 'block) '("two" "another.") '("123" "456"))])
    (check-equal? p (Point 0 1 1))
    (check-equal? lines '("t123wo" "a456nother."))))

(module+ test
  (let-values ([(p lines) (replace-scope (Scope (Point 0 0 0) (Point 0 0 0) #f #f 'char) (list "abc") '("1"))])
    (check-equal? p (Point 0 1 1))
    (check-equal? lines '("1abc")))
  (let-values ([(p lines) (replace-scope (Scope (Point 0 0 0) (Point 0 0 0) #f #f 'char) (list "abc") '("1" "2"))])
    (check-equal? p (Point 1 1 1))
    (check-equal? lines '("1" "2abc")))
  (let-values ([(p lines) (replace-scope (Scope (Point 0 0 0) (Point 0 0 0) #f #f 'char) (list "abc") '("1" "2" "3"))])
    (check-equal? p (Point 2 1 1))
    (check-equal? lines '("1" "2" "3abc")))
  (let-values ([(p lines) (replace-scope (Scope (Point 0 0 0) (Point 0 0 0) #f #t 'char) (list "abc") '("1" "2" "3"))])
    (check-equal? p (Point 2 1 1))
    (check-equal? lines '("1" "2" "3abc")))
  (let-values ([(p lines) (replace-scope (Scope (Point 0 1 1) (Point 0 1 1) #f #f 'char) (list "abc") '("1" "2" "3"))])
    (check-equal? p (Point 2 1 1))
    (check-equal? lines '("a1" "2" "3bc")))
  (let-values ([(p lines) (replace-scope (Scope (Point 0 1 1) (Point 0 1 1) #f #t 'char) (list "abc") '("1" "2" "3"))])
    (check-equal? p (Point 2 1 1))
    (check-equal? lines '("a1" "2" "3bc")))
  (let-values ([(p lines) (replace-scope (Scope (Point 0 0 0) (Point 0 1 1) #f #f 'char) (list "abc") '("1" "2" "3"))])
    (check-equal? p (Point 2 1 1))
    (check-equal? lines '("1" "2" "3bc")))
  (let-values ([(p lines) (replace-scope (Scope (Point 0 0 0) (Point 0 1 1) #f #t 'char) (list "abc") '("1" "2" "3"))])
    (check-equal? p (Point 2 1 1))
    (check-equal? lines '("1" "2" "3bc")))
  (let-values ([(p lines) (replace-scope (Scope (Point 0 0 0) (Point 0 1 1) #f #t 'line) (list "abc") '("1" "2" "3"))])
    (check-equal? p (Point 2 0 0))
    (check-equal? lines '("1" "2" "3")))
  (let-values ([(p lines) (replace-scope (Scope (Point 0 0 0) (Point 1 1 1) #f #t 'char) (list "abc" "def") '("1" "2" "3"))])
    (check-equal? p (Point 2 1 1))
    (check-equal? lines '("1" "2" "3ef")))
  (let-values ([(p lines) (replace-scope (Scope (Point 0 1 1) (Point 1 1 1) #f #t 'char) (list "abc" "def") '("1" "2" "3"))])
    (check-equal? p (Point 2 1 1))
    (check-equal? lines '("a1" "2" "3ef")))
  (let-values ([(p lines) (replace-scope (Scope (Point 0 0 0) (Point 1 1 1) #f #t 'line) (list "abc" "def") '("1" "2" "3"))])
    (check-equal? p (Point 2 0 0))
    (check-equal? lines '("1" "2" "3")))
  (let-values ([(p lines) (replace-scope (Scope (Point 0 1 1) (Point 1 1 1) #f #t 'line) (list "abc" "def") '("1" "2" "3"))])
    (check-equal? p (Point 2 0 0))
    (check-equal? lines '("1" "2" "3"))))

(define shift-width 4)

(: make-indention (-> String))
(define (make-indention)
  (make-string shift-width #\space))

(: right-shift-line (-> String String))
(define (right-shift-line l)
  (string-append (make-indention) l))

(module+ test
  (check-equal? (right-shift-line "abc") "    abc"))

(: left-shift-line-drop-length (-> String Natural))
(define (left-shift-line-drop-length l)
  (define ^-p (^-point 0 l))
  (min shift-width (Point-col ^-p)))

(: left-shift-line (-> String String))
(define (left-shift-line l)
  (define drop-length (left-shift-line-drop-length l))
  (substring l drop-length))

(module+ test
  (check-equal? (left-shift-line "abc") "abc")
  (check-equal? (left-shift-line "     abc") " abc"))

(: shift-scope (-> Scope (Listof String) (-> String String) (Values Point (Listof String))))
(define (shift-scope s lines shift-line-func)
  (match-define (Scope (Point row1 _ _) (Point row2 _ _) _ _ _) s)
  (cond
    [(empty? lines) (error 'empty-lines-for-right-shift)]
    [else
     (define-values (before-middle after) (split-at lines (add1 row2)))
     (define-values (before middle) (split-at before-middle row1))
     (define new-middle
       (for/list : (Listof String)
         ([l : String middle])
         (shift-line-func l)))
     (define new-lines (append before new-middle after))
     (define new-point (^-point row1 (list-ref new-lines row1)))
     (values new-point new-lines)]))

(: right-shift-scope (-> Scope (Listof String) (Values Point (Listof String))))
(define (right-shift-scope s lines)
  (shift-scope s lines right-shift-line))

(: left-shift-scope (-> Scope (Listof String) (Values Point (Listof String))))
(define (left-shift-scope s lines)
  (shift-scope s lines left-shift-line))
  
(module+ test
  (let-values ([(p lines) (right-shift-scope (Scope (Point 0 0 0) (Point 0 0 0) #t #t 'char) '("abc"))])
    (check-equal? p (Point 0 4 4))
    (check-equal? lines '("    abc")))
  (let-values ([(p lines) (right-shift-scope (Scope (Point 0 0 0) (Point 0 1 1) #t #t 'char) '("abc"))])
    (check-equal? p (Point 0 4 4))
    (check-equal? lines '("    abc")))
  (let-values ([(p lines) (right-shift-scope (Scope (Point 0 1 1) (Point 1 0 0) #t #t 'char) '("abc" "def"))])
    (check-equal? p (Point 0 4 4))
    (check-equal? lines '("    abc" "    def")))
  (let-values ([(p lines) (right-shift-scope (Scope (Point 1 1 1) (Point 2 0 0) #t #t 'char) '("abc" "def" "ghi"))])
    (check-equal? p (Point 1 4 4))
    (check-equal? lines '("abc" "    def" "    ghi")))
  (let-values ([(p lines) (left-shift-scope (Scope (Point 1 1 1) (Point 2 0 0) #t #t 'char) '("   abc" "    def" "     ghi"))])
    (check-equal? p (Point 1 0 0))
    (check-equal? lines '("   abc" "def" " ghi")))
  )