#lang racket
(provide (all-defined-out))
(require racket/gui "draw-line.rkt" "mode-utils.rkt" "core.rkt"
         "scope.rkt" "move.rkt" "wrapped-move-scope.rkt"
         "change.rkt" "params.rkt" "operators.rkt" "insert-utils.rkt" "search.rkt" "substitude.rkt")

(define (key-symbol->char k)
  (define str (symbol->string k))
  (cond
    [( = (string-length str) 1)
     (string-ref str 0)]
    [else #f])
  )

(define insert-mode%
  (class line-cursor-mode%
    (super-new)
    (init-field start-point count [diff-lst '()] [change-motions #f] [start-motions-lst '()] [pre-inserted-lines '()])
    (define org-lines #f)
    (define leftest-point start-point)
    (define/override (on-char key-symbol b mode-switcher diff-manager reg-manager macro-recorder)
      (define (cur) (Buffer-cur b))
      (define (lines) (Buffer-lines b))
      (when (and (not (equal? key-symbol '<Esc>)) (not org-lines))
        (set! org-lines (lines)))
      (define (insert-move! motion)
        (insert-escape! b leftest-point start-point org-lines start-motions-lst change-motions diff-lst diff-manager reg-manager #f)
        (define new-p (move-point (make-Motion motion) (cur) (lines)))
        (set! start-point new-p)
        (set! leftest-point new-p)
        (set! org-lines (lines))
        (set! change-motions #f)
        (set! diff-lst '())
        motion)
      (define motion
        (match key-symbol
          ['<BACKSPACE>
           (when (> (Point-col (cur)) 0)
             (define new-cur (left-point (cur)))
             (set! leftest-point (min-Point new-cur start-point))
             (set-Buffer-cur! b new-cur)
             (Buffer-delete-char! b #t))
           #f]
          ['<CR>
           (define-values (new-p new-lines) (split-line-point-lines (cur) (lines)))
           (set-Buffer-cur! b new-p)
           (set-Buffer-lines! b new-lines)
           #f]
          ['<Left>       (insert-move! 'left)] ; out of insert mode and reenter
          ['<Right>      (insert-move! 'right*)]
          ['<Up>         (insert-move! 'up*)]
          ['<Down>       (insert-move! 'down*)]
          ['<M-v>
           (define inserted-str (send the-clipboard get-clipboard-string (current-milliseconds)))
           (define inserted-lines (string-split inserted-str "\n"))
           (call-with-values
            (thunk
             (insert (cur) inserted-lines (lines) 'char))
            (update-Buffer-and-diffs! b diff-manager))
           #f]
          ['<Esc>
           ;(displayln (~e 'start-motions-lst start-motions-lst))
           (insert-escape! b leftest-point start-point org-lines start-motions-lst change-motions diff-lst diff-manager reg-manager count)
           (send mode-switcher enter-mode! (new normal-mode%))
           'left]
          [(? key-symbol->char key-symbol)
           (define k (key-symbol->char key-symbol))
           (define updated-lines (lines-insert-char-at-point k (cur) (lines)))
           (set-Buffer-lines! b updated-lines)
           'right*]
          [_           #f]))
      (when motion (set-Buffer-cur! b (move-point (make-Motion motion) (cur) (lines))))
      )))

(define normal-mode%
  (class block-cursor-mode%
    (super-new)
    (init-field [delegated-mode #f])
    (define count #f)
    (define/override (on-char key-symbol b mode-switcher diff-manager reg-manager macro-recorder)
      (define (p) (Buffer-cur b))
      (define (lines) (Buffer-lines b))
      (define (row) (Point-row (p)))
      (define (line) (list-ref (lines) (row)))
      (define (line-end) (line-end-point (row) (line)))
      (cond
        [((conjoin key-symbol->char
                   (lambda(k-s)
                     (define k (key-symbol->char k-s))
                     (and (char-numeric? k)
                          (or count (not (equal? k #\0)))))) key-symbol)
         (set! count (update-count (key-symbol->char key-symbol) count))]
        [else (define motion
                (match key-symbol
                  [(or 't 'T 'f 'F '\' '\`)
                   (send mode-switcher enter-mode! (new tfm-mode% [tfm-motion key-symbol] [operator #f] [count count] [last-mode (or delegated-mode this)])) #f]
                  ['c         (send mode-switcher enter-mode! (new op-mode% [operator 'change-op] [prefix-count count]))
                              (set! count #f)
                              #f]
                  ['m         (send mode-switcher enter-mode! (new mark-mode% [last-mode (or delegated-mode this)])) #f]
                  [(or 'i 'a 'I 'A 'C 's 'S 'o 'O) (define start-motion-lst (insert-key-to-start-motion-lst key-symbol))
                                                   (define scope-motion (insert-key-to-scope-motion key-symbol))
                                                   (define start-motions-lst (map make-Motion start-motion-lst))
                                                   (define scope-motions (make-Motion scope-motion))
                                                   (define pre-inserted-lines (if (member key-symbol (list 'o 'O)) '("" "") '()))
                                                   (change! start-motions-lst scope-motions (p) b pre-inserted-lines mode-switcher count)
                                                   'nope]
                  [(or 'x 'X)(define motions (make-Motion
                                              (if (equal? key-symbol 'x)
                                                  'right*
                                                  'left)
                                              #:count (or count 1)))
                             (send reg-manager set-last-cmd (make-Command delete-op motions))
                             (call-with-values
                              (thunk
                               (repeat (send reg-manager get-last-cmd) (p) (lines) reg-manager))
                              (update-Buffer-and-diffs! b diff-manager))
                             (set! count #f)
                             #f]
                  [(or 'p 'P) (define motions (make-Motion 'nope))
                              (define op (key-to-operator key-symbol))
                              (send reg-manager set-last-cmd (make-Command op motions))
                              (call-with-values
                               (thunk
                                (repeat (send reg-manager get-last-cmd) (p) (lines) reg-manager #:count (or count 1)))
                               (update-Buffer-and-diffs! b diff-manager))
                              (set! count #f)
                              #f]
                  [(or 'y 'd '> '<)(send mode-switcher enter-mode! (new op-mode% [operator (key-to-operator key-symbol)] [prefix-count count]))
                                   (set! count #f)
                                   #f]
                  ['<C-v>
                   (send mode-switcher enter-mode! (new visual-block-mode% [start-point (p)])) #f]
                  ['v         (send mode-switcher enter-mode! (new visual-char-mode% [start-point (p)])) #f]
                  ['V         (send mode-switcher enter-mode! (new visual-line-mode% [start-point (p)])) #f]
                  ['\.         (call-with-values
                                (thunk
                                 (repeat (send reg-manager get-last-cmd) (p) (lines) reg-manager))
                                (update-Buffer-and-diffs! b diff-manager))
                               #f]
                  ['\;         (send reg-manager get-last-motions)]
                  ['\,         (match-define (Motion mo char count) (send reg-manager get-last-motions))
                               (make-Motion (reverse-tf mo) char #:count count)]
                  ['u         (define-values (new-p new-lines) (send diff-manager undo-last (lines)))
                              (when new-p
                                (set-Buffer-lines! b new-lines)
                                (set-Buffer-cur! b new-p))
                              #f]
                  ['<C-r>
                   (define-values (new-p new-lines) (send diff-manager redo-next (lines)))
                   (when new-p
                     (set-Buffer-lines! b new-lines)
                     (set-Buffer-cur! b new-p))
                   #f]
                  ['r         (send mode-switcher enter-mode! (new replace-r-mode%)) #f]
                  ['R         (send mode-switcher enter-mode! (new replace-R-mode% [start-point (p)]))
                              'right]
                  ['g         (send mode-switcher enter-mode! (new g-op-mode% [prefix-count count]))
                              #f]
                  ['G         (define row (exact-round (sub1 (min (length (Buffer-lines b)) (or count +inf.0)))))
                              (set-Buffer-cur! b (Point row 0 0))
                              (send mode-switcher enter-mode! (new normal-mode%))
                              #f] ; should be merged to key-to-motion
                  [(or '/ '?)(send mode-switcher enter-mode! (new command-line-mode% [count count] [prefix key-symbol] [last-mode (or delegated-mode this)]))
                             #f]
                  [':         (send mode-switcher enter-mode! (new command-line-mode% [count count] [prefix ':] [last-mode (or delegated-mode this)]))
                              #f]
                  ['n (define single-motions (send reg-manager get-last-search-motions))
                      (struct-copy Motion single-motions [count count])]
                  ['N (match-define (Motion mo char count) (send reg-manager get-last-search-motions))
                      (define (reverse-search-motion mo)
                        (match mo
                          ['search-forwards 'search-backwards]
                          ['search-backwards 'search-forwards]))
                      (make-Motion (reverse-search-motion mo) char #:count count)]
                  [(or '* '\#)
                   (define search-words (Scoped-lines (get-point-scope (make-Motion 'iw) (p) (lines)) (lines)))
                   (define pattern (string-append "\\b" (first search-words) "\\b"))
                   (define motion (if (equal? key-symbol '*) 'search-forwards 'search-backwards))
                   (define search-motions (make-Motion motion pattern #:count 1))
                   (send reg-manager set-last-search-motions! search-motions)
                   (make-Motion motion pattern #:count count)]
                  ['q
                   (cond
                     [(send macro-recorder recording?)
                      (send macro-recorder stop-record-macro-to-reg! reg-manager)]
                     [else
                      (send mode-switcher enter-mode! (new record-start-mode% [last-mode (or delegated-mode this)]))])
                   #f]
                  ['@ (send mode-switcher enter-mode! (new execute-macro-mode% [last-mode (or delegated-mode this)] [count count]))
                      #f]
                  [_           (key-to-motion key-symbol)]))
              (cond
                [motion
                 (define motions (if (symbol? motion) (make-Motion motion #:count count) motion))
                 (move! motions (p) b)
                 (set! count #f)]
                [(Point<? (line-end) (p)) (set-Buffer-cur! b (line-end))])
              (set! count #f)]))))

(define (visual-mode-mixin %)
  (class %
    (super-new)
    (abstract get-mode)
    [init-field start-point]
    (define count #f)
    (define sub-normal-mode (new normal-mode% [delegated-mode this]))
    (define/override (get-scope b)
      (sort (list (Buffer-cur b) start-point) Point<?))
    (define (make-scope b reg-manager)
      (define p-pp (get-scope b))
      (send reg-manager set-mark! #\< (first p-pp))
      (send reg-manager set-mark! #\> (last p-pp))
      (apply Scope (append p-pp (list #t #t (get-mode)))))
    (define/override (on-char key-symbol b mode-switcher diff-manager reg-manager macro-recorder)
      (match key-symbol
        ['<Esc>     (make-scope b reg-manager)
                    (send mode-switcher enter-mode! (new normal-mode%))]
        ['<C-v>
         (send mode-switcher enter-mode!
               (if (equal? (get-mode) 'block)
                   (new normal-mode%)
                   (new visual-block-mode% [start-point start-point])))]
        ['v         (send mode-switcher enter-mode!
                          (if (equal? (get-mode) 'char)
                              (new normal-mode%)
                              (new visual-char-mode% [start-point start-point])))]
        ['V         (send mode-switcher enter-mode!
                          (if (equal? (get-mode) 'line)
                              (new normal-mode%)
                              (new visual-line-mode% [start-point start-point])))]
        ['i         (send mode-switcher enter-mode! (new op-ia-mode% [i/a? 'i] [operator visual-op] [count count]))] ; should only work when starting a visual mode / or from visual line.
        ['a         (send mode-switcher enter-mode! (new op-ia-mode% [i/a? 'a] [operator visual-op] [count count]))] ; should only work when starting a visual mode / or from visual line.
        ['o         (define old-start start-point)
                    (set! start-point (Buffer-cur b))
                    (set-Buffer-cur! b old-start)]
        ['g         (define old-scope (make-scope b reg-manager))
                    (send mode-switcher enter-mode! (new visual-g-op-mode% [visual-scope old-scope]))]
        ['r         (define old-scope (make-scope b reg-manager))
                    (send mode-switcher enter-mode! (new replace-r-mode% [visual-scope old-scope]))]
        ['c
         (define old-scope (make-scope b reg-manager))
         (define motions (scope-to-motion old-scope))
         (define start-motions-lst '())
         (change! start-motions-lst motions (Buffer-cur b) b '() mode-switcher count)]
        [(? (lambda (k) (key-to-operator-without-prefix k #f)))
         (define old-scope (make-scope b reg-manager))
         (define operator (key-to-operator-without-prefix key-symbol))
         (define motions (scope-to-motion old-scope))
         ;(displayln (~v 'motions motions))
         (operate! operator motions (Scope-start old-scope) b mode-switcher diff-manager reg-manager)]
        [(or 'I 'A) #:when (equal? (get-mode) 'block)
                    (define p (Buffer-cur b))
                    (define rows (sort (map Point-row (list p start-point)) <))
                    (define cols (sort (map Point-col (list p start-point)) <))
                    (define col (first cols))
                    (define fake-scope (Scope (Point (first rows) col col) (Point (last rows) col col) #t #f 'block))
                    (define scope-motions (scope-to-motion fake-scope))
                    (define insert-point (Point (first rows) col col))
                    (define start-motions (make-Motion (if (equal? key-symbol 'I) 'nope 'right) #:count (- (last cols) col -1)))
                    (change! (list start-motions) scope-motions insert-point b '() mode-switcher count)
                    #f]
        [(? (conjoin key-symbol->char
                     (lambda(k-s)
                       (define k (key-symbol->char k-s))
                       (and (char-numeric? k)
                            (or count (not (equal? k #\0)))))))
         (set! count (update-count (key-symbol->char key-symbol) count))
         #f]
        [_           (send sub-normal-mode on-char key-symbol b mode-switcher diff-manager reg-manager macro-recorder)]))))

(define visual-char-mode%
  (class (visual-mode-mixin visual-mode-base%)
    (super-new)
    (define/override (get-mode)
      'char)))

(define visual-line-mode%
  (class (visual-mode-mixin visual-line-mode-base%)
    (super-new)
    (define/override (get-mode)
      'line)))

(define visual-block-mode%
  (class (visual-mode-mixin visual-block-mode-base%)
    (super-new)
    (define/override (get-mode)
      'block)))

(define replace-r-mode%
  (class block-cursor-mode%
    (super-new)
    (init-field [visual-scope #f])
    
    (define scope visual-scope)
    (define/override (on-char key-symbol b mode-switcher diff-manager reg-manager macro-recorder)
      (define (p) (Buffer-cur b))
      (define (lines) (Buffer-lines b))
      (define result (match key-symbol
                       ['<CR>
                        (define s (get-point-scope (make-Motion 'right) (p) (lines)))
                        (call-with-values
                         (thunk
                          (replace s (p) (lines) '("" "") 'char))
                         (update-Buffer-and-diffs! b diff-manager))]
                       [(? key-symbol->char key-symbol)
                        (define real-scope (or scope (get-point-scope (make-Motion 'right) (p) (lines))))
                        (define motions (scope-to-motion real-scope))
                        (operate! 'replace-op motions (Scope-start real-scope) b
                                  mode-switcher diff-manager reg-manager #:op-params (key-symbol->char key-symbol))]
                       [_ #f]))
      (and result (send mode-switcher enter-mode! (new normal-mode%))))))

(define replace-R-mode%
  (class block-cursor-mode%
    (super-new)
    [init-field start-point]
    (define org-line #f)
    (define/override (on-char key-symbol b mode-switcher diff-manager reg-manager macro-recorder)
      (define (cur) (Buffer-cur b))
      (define (lines) (Buffer-lines b))
      (define (row) (Point-row (cur)))
      (define (line) (list-ref (lines) (row)))
      (define (left) (left-point (cur)))
      (define (right) (right-point (cur) (line)))
      (define (up) (up-point (cur) (lines)))
      (define (down) (down-point (cur) (lines)))
      
      (match key-symbol
        ['<Left>       (set-Buffer-cur! b (left))]
        ['<Right>      (set-Buffer-cur! b (right))]
        ['<Up>         (set-Buffer-cur! b (up))]
        ['<Down>       (set-Buffer-cur! b (down))]
        ['<CR>
         (Buffer-delete-char! b #t)
         (define-values (new-p new-lines) (split-line-point-lines (cur) (lines)))
         (set-Buffer-cur! b new-p)
         (set-Buffer-lines! b new-lines)
         (set! org-line (line))
         #f]
        ['<BACKSPACE>
         (set-Buffer-cur! b (left))
         (when (Point<? start-point (cur))
           (define c (string-ref org-line (Point-col (cur))))
           (define updated-lines (lines-replace-char-after-point c (cur) (Buffer-lines b)))
           (set-Buffer-lines! b updated-lines))]
        [(? key-symbol->char key-symbol)
         (define updated-lines (lines-replace-char-after-point (key-symbol->char key-symbol) (cur) (Buffer-lines b)))
         (set-Buffer-lines! b updated-lines)
         (set-Buffer-cur! b (right))]
        ['<Esc>
         (send mode-switcher enter-mode! (new normal-mode%))
         (set-Buffer-cur! b (left))
         (set! org-line #f)
         #f]
        [_ (void)])
      (when (and (not (equal? key-symbol '<Esc>)) (not org-line)) (set! org-line (line)))
      )))

(define op-mode%
  (class block-cursor-mode%
    (super-new)
    (init-field operator [prefix-count #f])
    (define infix-count #f)
    (define/override (on-char key-symbol b mode-switcher diff-manager reg-manager macro-recorder)
      (define p (Buffer-cur b))
      (define lines (Buffer-lines b))
      (define row (Point-row p))
      (define l (list-ref lines row))
      (define (line-start) (line-start-scope p l))
      (define (line-end) (line-end-scope p l))
      (define count
        (cond
          [(not (or prefix-count infix-count)) #f]
          [(* (or prefix-count 1) (or infix-count 1))]))
      (match key-symbol
        [(or 't 'T 'f 'F '\` '\')
         (send mode-switcher enter-mode! (new tfm-mode% [tfm-motion key-symbol] [operator operator] [count count] [last-mode (new normal-mode%)]))]
        ['i         (send mode-switcher enter-mode! (new op-ia-mode% [i/a? 'i] [operator operator] [count count]))]
        ['a         (send mode-switcher enter-mode! (new op-ia-mode% [i/a? 'a] [operator operator] [count count]))]
        [(or '/ '?)(send mode-switcher enter-mode! (new command-line-mode% [count count] [prefix key-symbol] [operator operator] [last-mode (new normal-mode%)]))]
        [(or 'shift 'release)    (void)]
        ['<Esc>     (send mode-switcher enter-mode! (new normal-mode%))]
        [(? (conjoin key-symbol->char
                     (lambda(k-s)
                       (define k (key-symbol->char k-s))
                       (and (char-numeric? k)
                            (or infix-count (not (equal? k #\0)))))))
         (set! infix-count (update-count (key-symbol->char key-symbol) infix-count))]
        [_
         (define motions
           (match key-symbol
             [(? (lambda (key) (equal? operator (key-to-operator-without-prefix key #f))))
              (make-Motion 'down-line-mode #:count (sub1 (or count 1)))] ; down-line-mode include end
             ['n (define single-motions (send reg-manager get-last-search-motions))
                 (struct-copy Motion single-motions [count count])]
             ['N (match-define (Motion mo char count) (send reg-manager get-last-search-motions))
                 (define (reverse-search-motion mo)
                   (match mo
                     ['search-forwards 'search-backwards]
                     ['search-backwards 'search-forwards]))
                 (make-Motion (reverse-search-motion mo) char #:count count)]
             [(or '* '\#)
              (define search-words (Scoped-lines (get-point-scope (make-Motion 'iw) p lines) lines))
              (define pattern (string-append "\\b" (first search-words) "\\b"))
              (define motion (if (equal? key-symbol '*) 'search-forwards 'search-backwards))
              (define search-motions (make-Motion motion pattern #:count 1))
              (send reg-manager set-last-search-motions! search-motions)
              (make-Motion motion pattern #:count count)]
             [_           (cond
                            [(key-to-scope key-symbol)
                             (make-Motion (key-to-scope key-symbol) #:count count)]
                            [else (error 'missing-case (~a key-symbol))])]))
         (operate! operator motions p b mode-switcher diff-manager reg-manager)]))))

(define g-op-mode%
  (class block-cursor-mode%
    (super-new)
    (init-field [prefix-count #f])
    (define/override (on-char key-symbol b mode-switcher diff-manager reg-manager macro-recorder)
      (define op-key
        (match key-symbol
          ['~ 'g~]
          ['u 'gu]
          ['U 'gU]
          ['g (define row (sub1 (min (length (Buffer-lines b)) (or prefix-count 1))))
              (set-Buffer-cur! b (Point row 0 0))
              (send mode-switcher enter-mode! (new normal-mode%)) #f]
          ['<Esc> (send mode-switcher enter-mode! (new normal-mode%)) #f]
          [_          (error 'missing-case (~a key-symbol))]))
      (when op-key (define op (key-to-g-op op-key))
        (send mode-switcher enter-mode! (new op-mode% [operator op] [prefix-count prefix-count]))
        ))))

(define visual-g-op-mode%
  (class block-cursor-mode%
    (super-new)
    (init-field visual-scope)
    
    (define scope visual-scope)
    (define/override (on-char key-symbol b mode-switcher diff-manager reg-manager macro-recorder)
      (define op-key
        (match key-symbol
          ['~ 'g~]
          ['u 'gu]
          ['U 'gU]
          ['g (set-Buffer-cur! b (Point 0 0 0)) #f]
          ['<Esc> (send mode-switcher enter-mode! (new normal-mode%)) #f]
          ['release #f]
          ['shift #f]
          [_          (error 'missing-case (~a key-symbol))]))
      (cond
        [op-key (define operator (key-to-g-op op-key))
                (define p (Buffer-cur b))
                (define lines (Buffer-lines b))
                (define-values (new-point new-lines diffs) (operator scope p lines))
                (define motions (scope-to-motion scope))
                (unless (equal? key-symbol 'y) (send reg-manager set-last-cmd (make-Command operator motions)))
                (set-Buffer-cur! b new-point)
                (set-Buffer-lines! b new-lines)
                (send diff-manager push-diffs! diffs)
                (send mode-switcher enter-mode! (new normal-mode%))]
        ))))

(define tfm-mode%
  (class block-cursor-mode%
    (init-field tfm-motion operator last-mode [count 1])
    (super-new)
    (define/override (on-char key-symbol b mode-switcher diff-manager reg-manager macro-recorder)
      (define p (Buffer-cur b))
      (define lines (Buffer-lines b))
      (cond
        [(equal? operator #f)
         (match key-symbol
           [(? key-symbol->char key-symbol)
            (define k (key-symbol->char key-symbol))
            (define motions
              (cond
                [(equal? tfm-motion '\`) (Mark-Motion (send reg-manager get-mark k))]
                [(equal? tfm-motion '\')
                 (define m (send reg-manager get-mark k))
                 (define line-start (move-point (make-Motion '\^) m lines))
                 (Mark-Motion line-start)]
                [else (make-Motion tfm-motion k #:count count)]))
            (send reg-manager set-last-motions motions)
            (set-Buffer-cur! b (move-point motions p lines))
            (send mode-switcher enter-mode! last-mode)]
           [_ (void)])]
        [else
         (define k
           (match key-symbol
             [(? key-symbol->char key-symbol) (key-symbol->char key-symbol)]
             [_ #f]))
         (when k
           (define motions
             (cond
               [(equal? tfm-motion '\`) (Mark-Motion (send reg-manager get-mark k))]
               [(equal? tfm-motion '\')
                (define m (send reg-manager get-mark k))
                (define line-start (move-point (make-Motion '\^) m lines))
                (Mark-Motion line-start)]
               [else (make-Motion tfm-motion k #:count count)]))
           (operate! operator motions p b mode-switcher diff-manager reg-manager)
           (set! operator #f))]))))

(define op-ia-mode%
  (class block-cursor-mode%
    (init-field i/a? operator count)
    (super-new)
    (define/override (on-char key-symbol b mode-switcher diff-manager reg-manager macro-recorder)
      (define m (Buffer-cur b))
      (define motion (key-to-ia-motion key-symbol i/a?))
      (cond
        [motion
         (operate! operator (make-Motion motion #:count count) m b mode-switcher diff-manager reg-manager)]
        [(equal? key-symbol '<Esc>)
         (send mode-switcher enter-mode! (new normal-mode%))]))))

(define command-line-mode%
  (class block-cursor-mode%
    (init-field prefix count last-mode [operator #f])
    (define command "")
    (super-new)
    (define/override (get-status-line)
      (string-append (symbol->string prefix) command))
    (define/override (on-char key-symbol b mode-switcher diff-manager reg-manager macro-recorder)
      (define m (Buffer-cur b))
      (match key-symbol
        ['<Esc>
         (send mode-switcher enter-mode! (new normal-mode%))]
        ['<CR>
         (match prefix
           [(or '/ '?)
            (define motion
              (match prefix
                ['/ 'search-forwards]
                ['? 'search-backwards]
                [_ 'missing-case]))
            (when (non-empty-string? command)
              (define motions (make-Motion motion command #:count 1))
              (send reg-manager set-last-search-motions! motions))
            (define latest-motions (send reg-manager get-last-search-motions))
            (define motions (make-Motion motion (Motion-char latest-motions) #:count count))
            (cond
              [operator (operate! operator motions (Buffer-cur b) b mode-switcher diff-manager reg-manager)]
              [else
               (set-Buffer-cur! b (move-point motions (Buffer-cur b) (Buffer-lines b)))
               (send mode-switcher enter-mode! last-mode)])]
           [': (execute-command! command b mode-switcher diff-manager)]
           [else (error 'missing)])]
        ['<BACKSPACE>
         (set! command (substring command 0 (sub1 (string-length command))))]
        [(? key-symbol->char key-symbol)
         (set! command (string-append command (string (key-symbol->char key-symbol))))]
        [_ (void)]))))

(define (substitude-command-all! range src dst b diff-manager)
  (define row (Point-row (Buffer-cur b)))
  (define lines (Buffer-lines b))
  (match-define (list start-line end-line) (range->start-end-line range row lines))
  (define-values (new-lines diffs) (substitude-within-range src dst start-line end-line lines 'all))
  (set-Buffer-lines! b new-lines)
  (send diff-manager push-diffs! diffs))

(define (substitude-command-first-of-line! range src dst b diff-manager)
  (define lines (Buffer-lines b))
  (define row (Point-row (Buffer-cur b)))
  (match-define (list start-line end-line) (range->start-end-line range row lines))
  (define-values (new-lines diffs) (substitude-within-range src dst start-line end-line lines 'first))
  (set-Buffer-lines! b new-lines)
  (send diff-manager push-diffs! diffs))

(define (substitude-with-asking! range src dst b mode-switcher)
  (define next-range (search (Point 0 0 0) (Buffer-lines b) src 'forwards 1))
  (cond
    [next-range
     (set-Buffer-cur! b (first next-range))
     (send mode-switcher enter-mode! (new substitude-command-mode%
                                          [src-pattern src]
                                          [dst-pattern dst]
                                          [range-end-p (left-point (second next-range))]))]
    [else (send mode-switcher enter-mode! (new normal-mode%))]))

(define (range-line->line range-line row lines)
  (match range-line
    [(or "" ".") row]
    ["$" (sub1 (length lines))]
    [(? string->number range-line) (sub1 (string->number range-line))]
    [_ (error 'missing-range-line-case)]))

(define (parse-comma-range range row lines)
  (define reg-result (regexp-match #rx"^(.*),(.*)$" range))
  (cond
    [(not reg-result) #f]
    [else
     (match-define (list _ start-line end-line) reg-result)
     (map (lambda (l) (range-line->line l row lines)) (list start-line end-line))]))

(define (range->start-end-line range row lines)
  (cond
    [(parse-comma-range range row lines)]
    [else
     (match range
       ["%" (list 0 (sub1 (length lines)))]
       ["" (list row row)]
       [_ (error 'missing-range-case)])]))

(define (execute-command! command b mode-switcher diff-manager)
  (define reg-result (regexp-match #rx"^(.*)s(ubstitude)?/([^/]*)/([^/]*)(/g?c?)?$" command))
  (cond
    [(not reg-result) (error 'not-matched)]
    [else
     (match-define (list _ range __ src dst flag) reg-result)
     (define to-normal
       (match flag
         [#f (substitude-command-first-of-line! range src dst b diff-manager)]
         ["/g" (substitude-command-all! range src dst b diff-manager)]
         ["/gc" (substitude-with-asking! range src dst b mode-switcher) #f]
         ;["/c" (substitude-first-with-asking! range src dst b mode-switcher) #f]
         [else (error 'missing-case)]
         ))
     (when to-normal (send mode-switcher enter-mode! (new normal-mode%)))]))

(define substitude-command-mode%
  (class visual-mode-base%
    (super-new)
    (init-field src-pattern dst-pattern range-end-p)
    (define sub-normal-mode (new normal-mode%))
    (define/override (get-status-line)
      (string-append "%s/" src-pattern "/" dst-pattern "/gc"))
    (define/override (get-scope b)
      (sort (list (Buffer-cur b) (or range-end-p (Buffer-cur b))) Point<?))
    (define/override (on-char key-symbol b mode-switcher diff-manager reg-manager macro-recorder)
      (define p (Buffer-cur b))
      (define lines (Buffer-lines b))
      (match key-symbol
        ['<Esc>
         (send mode-switcher enter-mode! sub-normal-mode)]
        [(or 'y 'Y 'n 'N)
         (when (or (equal? key-symbol 'y) (equal? key-symbol 'Y))
           (define-values (new-lines diffs) (substitude-once src-pattern dst-pattern p lines))
           (set-Buffer-lines! b new-lines)
           (send diff-manager push-diffs! diffs))
         (define next-range (search p lines src-pattern 'forwards 1))
         (cond
           [(and next-range (not (Point<? (first next-range) p)))
            (set-Buffer-cur! b (first next-range))
            (set! range-end-p (left-point (second next-range)))]
           [else (set! range-end-p #f)
                 (send mode-switcher enter-mode! sub-normal-mode)])]
        [(or 'a 'A) (define-values (new-lines diffs) (substitude-from-point src-pattern dst-pattern p lines 'all))
                    (set-Buffer-lines! b new-lines)
                    (send diff-manager push-diffs! diffs)]
        [(or 'release 'shift) (void)]
        [_ (set! range-end-p #f)
           (send sub-normal-mode on-char key-symbol b mode-switcher diff-manager reg-manager macro-recorder)]))))

(define mark-mode%
  (class block-cursor-mode%
    (super-new)
    (init-field last-mode)
    (define/override (on-char key-symbol b mode-switcher diff-manager reg-manager macro-recorder)
      (define p (Buffer-cur b))
      (match key-symbol
        ['<Esc>
         (send mode-switcher enter-mode! last-mode)]
        [(? key-symbol->char key-symbol)
         (send reg-manager set-mark! (key-symbol->char key-symbol) p)
         (send mode-switcher enter-mode! last-mode)]
        [(or 'release 'shift)
         (void)]
        ))))

(define record-start-mode%
  (class block-cursor-mode%
    (super-new)
    (init-field last-mode)
    (define/override (on-char key-symbol b mode-switcher diff-manager reg-manager macro-recorder)
      (match key-symbol
        ['<Esc>
         (send mode-switcher enter-mode! last-mode)]
        [(? key-symbol->char key-symbol)
         (send mode-switcher enter-mode! last-mode)
         (send macro-recorder start-record-macro-to-reg! (key-symbol->char key-symbol))]
        [_ (error 'missing-case-in-record-start-mode%)]
        ))))

(define execute-macro-mode%
  (class block-cursor-mode%
    (super-new)
    (init-field last-mode count)
    (define/override (on-char key-symbol b mode-switcher diff-manager reg-manager macro-recorder)
      (match key-symbol
        ['<Esc>
         (send mode-switcher enter-mode! last-mode)]
        [(? key-symbol->char key-symbol)
         (send mode-switcher enter-mode! last-mode)
         (send macro-recorder execute-macro-from-reg (key-symbol->char key-symbol) reg-manager diff-manager count)]
        [_ (error 'missing-case-in-execute-macro-mode%)]
        ))))

(define (change start-motions-lst scope-motions p lines pre-inserted-lines mode-switcher count)
  (define scope (get-point-scope scope-motions p lines))
  (define-values (new-point new-lines diffs)
    (replace scope p lines pre-inserted-lines (Scope-mode scope)))
  ;(displayln (~e 'change p (Scope-start scope) new-point))
  (send mode-switcher enter-mode! (new insert-mode%
                                       [start-point new-point]
                                       [diff-lst diffs]
                                       [start-motions-lst start-motions-lst]
                                       [change-motions scope-motions]
                                       [count count]))
  (values new-point new-lines '()))

(define (change! start-motions-lst scope-motions p b pre-inserted-lines mode-switcher count)
  (define lines (Buffer-lines b))
  (define new-p 
    (for/fold ([new-p p])
              ([motion start-motions-lst])
      (move-point motion new-p lines)))
  (define-values (new-point new-lines diffs)
    (change start-motions-lst scope-motions new-p lines pre-inserted-lines mode-switcher count))
  (set-Buffer-cur! b new-point)
  (set-Buffer-lines! b new-lines))

(define (visual-op scope b mode-switcher)
  (match-define (Scope start end _ _ _) scope)
  (send mode-switcher enter-mode! (new visual-char-mode% [start-point start]))
  (values end (Buffer-lines b) '()))

(define (operate! operator motions p b mode-switcher diff-manager reg-manager #:op-params [op-params '()])
  (cond
    [(equal? operator 'change-op) ; only use in op-mode, not visual-mode. visual-mode change op accept count params to repeate inserted lines.
     (define start-motions-lst '())
     (change! start-motions-lst motions p  b '() mode-switcher 1)]
    [(equal? operator visual-op)
     (define lines (Buffer-lines b))
     (define scope (get-point-scope motions p lines))
     (call-with-values
      (thunk
       (visual-op scope b mode-switcher))
      (update-Buffer-and-diffs! b diff-manager))]
    [else
     (define lines (Buffer-lines b))
     (call-with-values
      (thunk
       (execute operator motions p lines reg-manager #:op-params op-params))
      (update-Buffer-and-diffs! b diff-manager))
     (unless (equal? operator yank-op) (send reg-manager set-last-cmd (make-Command operator motions #:op-params op-params)))
     (send mode-switcher enter-mode! (new normal-mode%))]))
