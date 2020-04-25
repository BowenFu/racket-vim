#lang typed/racket

(provide (all-defined-out))

(provide after-lines-point Scoped-lines (struct-out Point) (struct-out Scope) lines-scope)

(require "core.rkt" "move.rkt" "scope.rkt")

(require/typed "search.rkt" [search (-> Point Lines String Symbol (Listof Point))])

(require/typed "match-paren.rkt" [%-point (-> Point Lines Point)])

(module+ test (require typed/rackunit))

(struct Motion ([motion : Symbol] [char : (U Char String #f)] [count : Natural]) #:transparent)

(struct Visual-Motion ([row : Natural] [col : Natural] [mode : Symbol]) #:transparent)

(struct Mark-Motion ([point : Point]) #:transparent)

(: make-Motion (->* (Symbol) ((U Char String #f) #:count Natural) Motion))
(define (make-Motion motion [char #f] #:count [count 1])
  (Motion motion char count))

(module+ test
  (check-equal? (make-Motion 'e) (Motion 'e #f 1))
  (check-equal? (make-Motion 'f #\x) (Motion 'f #\x 1))
  (check-equal? (make-Motion 'e #:count 2) (Motion 'e #f 2))
  (check-equal? (make-Motion 'f #\x #:count 3) (Motion 'f #\x 3)))

(: search-point (-> Point Lines String Symbol Point))
(define (search-point p lines pattern direction)
  (define range (search p lines pattern direction))
  (unless range (error 'not-result))
  (first range))

(: search-scope (-> Point Lines String Symbol Scope))
(define (search-scope p lines pattern direction)
  (define new-p (search-point p lines pattern direction))
  (Scope p new-p #t #f 'char))

(: move-point (-> (U Motion Mark-Motion) Point Lines Point))
(define (move-point motions p lines)
  ;(displayln (~e 'move-point motions p lines))
  (define-values (row col) (Point-row-col p))
  (define line
    (if (empty? lines)
        ""
        (list-ref lines row)))
  (cond
    [(Mark-Motion? motions) (Mark-Motion-point motions)]
    [else
     (match-define (Motion motion char count) motions)
     (match motion
       ['left (left-point p count)]
       ['right (right-point p line count)]
       ['right* (right-point* p line count)]
       ['e (e-point p line count)]
       ['E (E-point p line count)]
       ['w (w-point p line count)]
       ['W (W-point p line count)]
       ['b (b-point p line count)]
       ['B (B-point p line count)]
       ['|0| (line-start-point row)]
       ['^ (^-point row line)]
       ['$ ($-point row lines count)]
       ['$* ($-point* row lines count)]
       ['t
        (define c (cast char Char))
        (t-point c p line count)]
       ['T
        (define c (cast char Char))
        (T-point c p line count)]
       ['f
        (define c (cast char Char))
        (f-point c p line count)]
       ['F
        (define c (cast char Char))
        (F-point c p line count)]
       ['up (up-point p lines count)]
       ['down (down-point p lines count)]
       ['up* (up-point* p lines count)]
       ['down* (down-point* p lines count)]
       ['G (G-point lines)]
       ['nope p]
       ['search-forwards (define pattern (cast char String))
                         (search-point p lines pattern 'forwards)]
       ['search-backwards (define pattern (cast char String))
                          (search-point p lines pattern 'backwards)]
       ['% (%-point p lines)]
       [_ (error 'move-point-missing-case (~a motion))])]))

(module+ test
  (check-equal? (move-point (make-Motion 'right #:count 2) (Point 0 0 0) '("abc")) (Point 0 2 2))
  (check-equal? (move-point (make-Motion 'e) (Point 0 0 0) '("abc")) (Point 0 2 2))
  (check-equal? (move-point (make-Motion 't #\x) (Point 0 0 0) '("abc")) (Point 0 0 0)))

(: get-point-scope (-> (U Motion Visual-Motion Mark-Motion) Point Lines Scope))
(define (get-point-scope motions p lines)
  (match motions
    [(Motion _ _ _) (get-point-scope-from-motion motions p lines)]
    [(Visual-Motion _ _ _) (get-point-scope-from-visual-motion motions p lines)]
    [(Mark-Motion pp)
     (define points
       (sort (list p pp) Point<?))
     (Scope (first points) (last points) #t #t 'char)]
    [_ (error "get-point-scope wrong type of motions")]))

(: get-point-scope-from-visual-motion (-> Visual-Motion Point Lines Scope))
(define (get-point-scope-from-visual-motion rel-motion p lines)
  (define-values (row col) (Point-row-col p))
  (match-define (Visual-Motion rel-row rel-col mode) rel-motion)
  (define end
    (cond
      [(equal? mode 'char)
       (define start-p (Point row 0 0))
       (define temp-p (move-point (make-Motion 'down #:count rel-row) start-p lines))
       (move-point (make-Motion 'right #:count rel-col) temp-p lines)]
      [(equal? mode 'block)
       (define new-row (cast (min (+ rel-row row) (sub1 (length lines))) Natural))
       (Point new-row (+ rel-col col) (+ rel-col col))]
      [else (error 'missing-case)]))
  (Scope p end #t #f mode))

(require/typed "match-paren.rkt"
               [a-paren-pair (-> (Pairof Char Char) Point Lines Natural (Pairof Point Point))])

(: a-paren-scope (-> (Pairof Char Char) Point Lines Natural Scope))
(define (a-paren-scope paren-pair p lines count)
  (define pair (a-paren-pair paren-pair p lines count))
  (Scope (car pair) (cdr pair) #t #t 'char))

(: i-paren-scope (-> (Pairof Char Char) Point Lines Natural Scope))
(define (i-paren-scope paren-pair p lines count)
  (define pair (a-paren-pair paren-pair p lines count))
  (define new-start
    (move-point (make-Motion 'right* #:count 1) (car pair) lines))
  (Scope new-start (cdr pair) #t #f 'char))

(: a-b-scope (-> Point Lines Natural Scope))
(define (a-b-scope p lines count)
  (a-paren-scope '(#\( . #\)) p lines count))

(: a-\[-scope (-> Point Lines Natural Scope))
(define (a-\[-scope p lines count)
  (a-paren-scope '(#\[ . #\]) p lines count))

(: a-B-scope (-> Point Lines Natural Scope))
(define (a-B-scope p lines count)
  (a-paren-scope '(#\{ . #\}) p lines count))

(: a-\<-scope (-> Point Lines Natural Scope))
(define (a-\<-scope p lines count)
  (a-paren-scope '(#\< . #\>) p lines count))

(: i-b-scope (-> Point Lines Natural Scope))
(define (i-b-scope p lines count)
  (i-paren-scope '(#\( . #\)) p lines count))

(: i-\[-scope (-> Point Lines Natural Scope))
(define (i-\[-scope p lines count)
  (i-paren-scope '(#\[ . #\]) p lines count))

(: i-B-scope (-> Point Lines Natural Scope))
(define (i-B-scope p lines count)
  (i-paren-scope '(#\{ . #\}) p lines count))

(: i-\<-scope (-> Point Lines Natural Scope))
(define (i-\<-scope p lines count)
  (i-paren-scope '(#\< . #\>) p lines count))

(: get-point-scope-from-motion (-> Motion Point Lines Scope))
(define (get-point-scope-from-motion motions p lines)
  (define-values (row col) (Point-row-col p))
  (define line
    (if (or (empty? lines) (equal? row (length lines)))
        ""
        (list-ref lines row)))
  (match-define (Motion motion char count) motions)
  (match motion
    ['nope (Scope p p #t #f 'char)]
    ['left (left-scope p count)]
    ['right (right-scope* p line count)]
    ['right* (right-scope* p line count)]
    ['e (e-scope p line count)]
    ['E (E-scope p line count)]
    ['w (w-scope p line count)]
    ['W (W-scope p line count)]
    ['iw (i-w-scope p line count)]
    ['iW (i-W-scope p line count)]
    ['aw (a-w-scope p line count)]
    ['aW (a-W-scope p line count)]
    ['ib (i-b-scope p lines count)]
    ['i\[ (i-\[-scope p lines count)]
    ['iB (i-B-scope p lines count)]
    ['i< (i-<-scope p lines count)]
    ['ab (a-b-scope p lines count)]
    ['a\[ (a-\[-scope p lines count)]
    ['aB (a-B-scope p lines count)]
    ['a< (a-<-scope p lines count)]
    ['b (b-scope p line count)]
    ['B (B-scope p line count)]
    ['|0| (line-start-scope p)]
    ['$ (line-end-scope p line)]
    ['$* (after-line-end-scope p line)]
    ['t
     (define c (cast char Char))
     (t-scope c p line count)]
    ['T
     (define c (cast char Char))
     (T-scope c p line count)]
    ['f
     (define c (cast char Char))
     (f-scope c p line count)]
    ['F
     (define c (cast char Char))
     (F-scope c p line count)]
    ['up (up-scope p lines count)]
    ['down (down-scope p lines count)]
    ['up-line-mode (up-scope-line-mode p lines count)]
    ['down-line-mode (down-scope-line-mode p lines count)]
    ['G (G-scope p lines)]
    ['line (line-scope p line)]
    ['search-forwards (define pattern (cast char String))
                      (search-scope p lines pattern 'forwards)]
    ['search-backwards (define pattern (cast char String))
                       (search-scope p lines pattern 'backwards)]
    ['%
     (define pp (%-point p lines))
     (define points
       (sort (list p pp) Point<?))
     (Scope (first points) (last points) #t #t 'char)]
    [_ (error 'get-point-scope-from-motion-missing-case (~a motion))]))

(module+ test
  (check-equal? (get-point-scope-from-motion (make-Motion 'e) (Point 0 0 0) '("abc")) (Scope (Point 0 0 0) (Point 0 2 2) #t #t 'char))
  (check-equal? (get-point-scope-from-motion (make-Motion 't #\x) (Point 0 0 0) '("abc")) (Scope (Point 0 0 0) (Point 0 0 0) #t #t 'char))
  (check-equal? (get-point-scope-from-motion (make-Motion 'ab) (Point 0 0 0) '("(abc)")) (Scope (Point 0 0 0) (Point 0 4 4) #t #t 'char)))

(define (key-to-scope k)
  (match k
    ['left       'left]
    ['right      'right]
    ['up         'up-line-mode]
    ['down       'down-line-mode]
    [#\h         'left]
    [#\l         'right]
    [#\k         'up-line-mode]
    [#\j         'down-line-mode]
    [#\0         '|0|]
    [#\^         '^]
    [#\$         '$]
    [#\e         'e]
    [#\E         'E]
    [#\b         'b]
    [#\B         'B]
    [#\w         'w]
    [#\W         'W]
    [#\G         'G]
    [#\%         '%]
    [_ #f]))

(define (key-to-motion k)
  (match k
    ['left       'left]
    ['right      'right]
    ['up         'up]
    ['down       'down]
    [#\h         'left]
    [#\l         'right]
    [#\k         'up]
    [#\j         'down]
    [#\0         '|0|]
    [#\^         '^]
    [#\$         '$]
    [#\e         'e]
    [#\E         'E]
    [#\b         'b]
    [#\B         'B]
    [#\w         'w]
    [#\W         'W]
    [#\G         'G]
    [#\%         '%]
    [_ #f]))

(define (key-to-ia-motion k i/a?)
  (cond
    [(equal? i/a? 'i)
     (match k
       [#\w 'iw]
       [#\W 'iW]
       [(or #\b #\( #\)) 'ib]
       [(or #\[ #\]) 'i\[]
       [(or #\B #\{ #\}) 'iB]
       [(or #\< #\>) 'i<]
       [_ #f])]
    [(equal? i/a? 'a)
     (match k
       [#\w 'aw]
       [#\W 'aW]
       [(or #\b #\( #\)) 'ab]
       [(or #\[ #\]) 'a\[]
       [(or #\B #\{ #\}) 'aB]
       [(or #\< #\>) 'a<]
       [_ #f])]
    [else (error (~a "incorrect i/a: " i/a?))]))

(: equal-point? (-> Point Point Boolean))
(define (equal-point? point1 point2)
  (and (equal? (Point-row point1) (Point-row point2))
       (equal? (Point-col point1) (Point-col point2))))

(: empty-scope? (-> Scope Boolean))
(define (empty-scope? scope)
  (match-define (Scope start end dir include-end? mode) scope)
  (cond
    [(equal? mode 'line) #f]
    [(and dir include-end?) #f]
    [(equal-point? start end) #t]
    [else #f]))

(module+ test
  (check-equal? (empty-scope? (Scope (Point 0 1 1) (Point 0 1 1) #t #f 'char)) #t)
  (check-equal? (empty-scope? (Scope (Point 0 1 1) (Point 0 1 1) #t #t 'char)) #f)
  (check-equal? (empty-scope? (Scope (Point 0 1 1) (Point 0 2 2) #t #t 'char)) #f))

(: equal-scope? (-> Scope Scope Boolean))
(define (equal-scope? scope1 scope2)
  (equal? scope1 scope2))


(require "common-utils.rkt")

(: Scoped-lines (-> Scope (Listof String) (Listof String)))
(define (Scoped-lines scope lines)
  (match-define (Scope start end dir include-real-end? mode) scope)
  (cond
    [(empty-scope? scope) '()]
    [(equal? mode 'line)
     (define end-row (+ (Point-row end) (if include-real-end? 1 0)))
     (drop (take lines end-row) (Point-row start))
     ]
    [(equal? mode 'block)
     (define-values (_ middle __) (before-middle-after lines (Point-row start) (add1 (Point-row end))))
     (define col0 (Point-col start))
     (define col1 (Point-col end))
     (define col-min (min col0 col1))
     (define col-max (max col0 col1))
     (define end-col (+ col-max (if (and include-real-end? dir) 1 0)))
     (for/list : (Listof String)
       ([l middle])
       (string-append
        (substring-in-range l col-min end-col)))]
    [else
     (define end-col (+ (Point-col end) (if (and include-real-end? dir) 1 0)))
     (cond
       [(equal? (Point-row start) (Point-row end))
        (define line (list-ref lines (Point-row start)))
        (list (substring-in-range line (Point-col start) end-col))]
       [else
        (define-values (l0 l1 l2 l3 l4) (split-five-at lines (Point-row start) (Point-row end)))
        (define m1 (substring (first-or-empty-string l1)
                              (Point-col start)))
        (define m3 (substring (first-or-empty-string l3) 0 end-col))
        (append (list m1) l2 (list m3))])]))

(module+ test
  (check-equal? (Scoped-lines (Scope (Point 0 1 0) (Point 0 2 0) #t #t 'char) (list "abc")) '("bc"))
  (check-equal? (Scoped-lines (Scope (Point 0 0 0) (Point 0 0 0) #f #f 'char) (list "abc")) '())
  (check-equal? (Scoped-lines (Scope (Point 0 0 0) (Point 0 0 0) #f #t 'char) (list "abc")) '())
  (check-equal? (Scoped-lines (Scope (Point 0 0 0) (Point 0 0 0) #f #f 'line) (list "abc")) '())
  (check-equal? (Scoped-lines (Scope (Point 0 0 0) (Point 0 0 0) #f #t 'line) (list "abc")) '("abc"))
  (check-equal? (Scoped-lines (Scope (Point 0 0 0) (Point 0 1 1) #f #f 'char) (list "abc")) '("a"))
  (check-equal? (Scoped-lines (Scope (Point 0 0 0) (Point 0 1 1) #f #t 'char) (list "abc")) '("a"))
  (check-equal? (Scoped-lines (Scope (Point 0 0 0) (Point 0 1 1) #f #f 'line) (list "abc")) '())
  (check-equal? (Scoped-lines (Scope (Point 0 0 0) (Point 0 1 1) #f #t 'line) (list "abc")) '("abc"))
  (check-equal? (Scoped-lines (Scope (Point 0 1 1) (Point 1 1 1) #f #f 'char) (list "abc" "def")) '("bc" "d"))
  (check-equal? (Scoped-lines (Scope (Point 0 1 1) (Point 1 1 1) #f #t 'char) (list "abc" "def")) '("bc" "d"))
  (check-equal? (Scoped-lines (Scope (Point 0 1 1) (Point 1 1 1) #f #f 'line) (list "abc" "def")) '("abc"))
  (check-equal? (Scoped-lines (Scope (Point 0 1 1) (Point 1 1 1) #f #t 'line) (list "abc" "def")) '("abc" "def"))
  (check-equal? (Scoped-lines (Scope (Point 1 1 1) (Point 2 1 1) #f #f 'char) (list "abc" "def" "ghi" "jkl")) '("ef" "g")))
