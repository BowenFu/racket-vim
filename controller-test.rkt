#lang racket

(require rackunit
         (only-in racket/gui/base key-event% key-code-symbol?)
         "controller.rkt" "core.rkt")

(define (key-or-event ke)
  (cond
    [(is-a? ke key-event%) ke]
    [((disjoin char? key-code-symbol?) ke) (new key-event% [key-code ke])]
    [else (error 'missing-case)]))

(define (execute-key-events b lst)
  (define controller (new controller% [buffer b]))
  (for ([k lst])
    (define event (key-or-event k))
    (send controller on-char event)))

(define sample-lines
  '("Sing, O goddess, the anger"
    "of Achilles son"
    "of Peleus, that brought"))
  
(let ([ b (new-Buffer sample-lines)])
  (execute-key-events b (list #\A #\1 #\2))
  (check-equal? (Buffer-lines b)
                '("Sing, O goddess, the anger12"
                  "of Achilles son"
                  "of Peleus, that brought")))
  
(let ([ b (new-Buffer sample-lines)])
  (execute-key-events b (list (new key-event% [key-code #\v] [control-down #f])
                              #\A
                              #\1
                              'escape))
  (check-equal? (Buffer-lines b)
                '("Sing, O goddess, the anger1"
                  "of Achilles son"
                  "of Peleus, that brought")))
(let ([ b (new-Buffer sample-lines)])
  (execute-key-events b (list (new key-event% [key-code #\v] [control-down #t])
                              #\A
                              'release ; necessary for set org-lines
                              #\1
                              'escape))
  (check-equal? (Buffer-lines b)
                '("S1ing, O goddess, the anger"
                  "of Achilles son"
                  "of Peleus, that brought")))
(let ([ b (new-Buffer sample-lines)])
  (execute-key-events b (list #\>
                              #\>
                              #\d
                              #\d
                              #\p))
  (check-equal? (Buffer-lines b)
                '("of Achilles son"
                  "    Sing, O goddess, the anger"
                  "of Peleus, that brought")))
(let ([ b (new-Buffer sample-lines)])
  (execute-key-events b (list #\c
                              #\i
                              #\w
                              #\1
                              'escape))
  (check-equal? (Buffer-lines b)
                '("1, O goddess, the anger"
                  "of Achilles son"
                  "of Peleus, that brought")))
(let ([ b (new-Buffer sample-lines)])
  (execute-key-events b (list #\d
                              #\2
                              #\f
                              #\,))
  (check-equal? (Buffer-lines b)
                '(" the anger"
                  "of Achilles son"
                  "of Peleus, that brought")))
(let ([ b (new-Buffer sample-lines)])
  (execute-key-events b (list #\d
                              #\2
                              #\t
                              #\,))
  (check-equal? (Buffer-lines b)
                '(", the anger"
                  "of Achilles son"
                  "of Peleus, that brought")))
(let ([ b (new-Buffer sample-lines)])
  (execute-key-events b (list #\d
                              #\2
                              #\a
                              #\W))
  (check-equal? (Buffer-lines b)
                '("goddess, the anger"
                  "of Achilles son"
                  "of Peleus, that brought")))
(let ([ b (new-Buffer sample-lines)])
  (execute-key-events b (list #\g
                              'escape
                              #\2
                              #\$
                              #\d
                              #\T
                              #\space
                              #\d
                              #\F
                              #\space
                              #\d
                              #\i
                              #\w))
  (check-equal? (Buffer-lines b)
                '("Sing, O goddess, the anger"
                  "of "
                  "of Peleus, that brought")))
(let ([ b (new-Buffer sample-lines)])
  (execute-key-events b (list (new key-event% [key-code #\v] [control-down #t])
                              #\A
                              'release
                              #\1
                              'left
                              'down
                              'right
                              'up
                              'left
                              #\2
                              'escape))
  (check-equal? (Buffer-lines b)
                '("S21ing, O goddess, the anger"
                  "of Achilles son"
                  "of Peleus, that brought"))
  (check-equal? (Buffer-cur b) (Point 0 1 1)))
(let ([ b (new-Buffer sample-lines)])
  (execute-key-events b (list #\v
                              #\i
                              #\w
                              'escape
                              #\v
                              #\a
                              #\W
                              #\d))
  (check-equal? (Buffer-lines b)
                '("O goddess, the anger"
                  "of Achilles son"
                  "of Peleus, that brought"))
  (check-equal? (Buffer-cur b) (Point 0 0 0)))
(let ([ b (new-Buffer sample-lines)])
  (execute-key-events b (list #\v
                              #\a
                              'escape
                              #\g
                              #\g
                              #\f
                              #\,))
  (check-equal? (Buffer-cur b) (Point 0 4 4)))
(let ([ b (new-Buffer sample-lines)])
  (execute-key-events b (list   #\f
                                #\O
                                #\R
                                'right
                                'down
                                'left
                                'up
                                #\1
                                #\2
                                #\4
                                #\backspace
                                #\3
                                #\return
                                #\4
                                #\5
                                #\6
                                'escape))
  (check-equal? (Buffer-lines b)
                '("Sing, O123"
                  "456s, the anger"
                  "of Achilles son"
                  "of Peleus, that brought"))
  (check-equal? (Buffer-cur b) (Point 1 2 2)))
(let ([ b (new-Buffer sample-lines)])
  (execute-key-events b (list   #\g
                                #\U
                                #\U))
  (check-equal? (Buffer-lines b)
                '("SING, O GODDESS, THE ANGER"
                  "of Achilles son"
                  "of Peleus, that brought")))
(let ([ b (new-Buffer sample-lines)])
  (execute-key-events b (list   #\g
                                #\U
                                #\U
                                #\u))
  (check-equal? (Buffer-cur b) (Point 0 0 0))
  (check-equal? (Buffer-lines b)
                sample-lines)
  (check-equal? (Buffer-cur b) (Point 0 0 0)))
  
(let ([ b (new-Buffer sample-lines)])
  (execute-key-events b (list   #\g
                                #\U
                                #\U
                                #\u
                                (new key-event% [key-code #\r] [control-down #t])))
  (check-equal? (Buffer-lines b)
                '("SING, O GODDESS, THE ANGER"
                  "of Achilles son"
                  "of Peleus, that brought"))
  (check-equal? (Buffer-cur b) (Point 0 0 0)))

(let ([ b (new-Buffer sample-lines)])
  (execute-key-events b (list     #\f
                                  #\,
                                  (new key-event% [key-code #\v] [control-down #t])
                                  #\j
                                  #\e
                                  #\V
                                  #\v
                                  #\g
                                  #\~))
  (check-equal? (Buffer-lines b)
                '("Sing, o GODDESS, THE ANGER"
                  "OF aCHILLES son"
                  "of Peleus, that brought"))
  (check-equal? (Buffer-cur b) (Point 1 11 11)))
(let ([ b (new-Buffer sample-lines)])
  (execute-key-events b (list       #\f
                                    #\O
                                    #\v
                                    #\j
                                    #\r
                                    #\-))
  (check-equal? (Buffer-lines b)
                '("Sing, --------------------"
                  "-------lles son"
                  "of Peleus, that brought"))
  (check-equal? (Buffer-cur b) (Point 1 7 7)))
(let ([ b (new-Buffer sample-lines)])
  (execute-key-events b (list       #\f
                                    #\O
                                    #\v
                                    #\j
                                    #\r
                                    #\-
                                    #\w
                                    #\.))
  (check-equal? (Buffer-lines b)
                '("Sing, --------------------"
                  "-------lles ---"
                  "-------us, that brought"))
  (check-equal? (Buffer-cur b) (Point 2 7 7)))
(let ([ b (new-Buffer sample-lines)])
  (execute-key-events b (list       #\G
                                    #\A
                                    #\backspace
                                    'escape
                                    #\2
                                    #\g
                                    #\g
                                    #\x))
  (check-equal? (Buffer-lines b)
                '("Sing, O goddess, the anger" "f Achilles son" "of Peleus, that brough"))
  (check-equal? (Buffer-cur b) (Point 1 0 0)))
(let ([ b (new-Buffer sample-lines)])
  (execute-key-events b (list       #\f
                                    #\,
                                    #\F
                                    #\g
                                    #\T
                                    #\S
                                    #\t
                                    #\O
                                    #\;
                                    #\x
                                    #\,
                                    #\.))
  (check-equal? (Buffer-lines b)
                '("Sing, goddess, the anger"
                  "of Achilles son"
                  "of Peleus, that brought"))
  (check-equal? (Buffer-cur b) (Point 0 5 5)))
(let ([ b (new-Buffer sample-lines)])
  (execute-key-events b (list       #\t
                                    #\O
                                    #\r
                                    #\return
                                    #\y
                                    #\y
                                    #\P))
  (check-equal? (Buffer-lines b)
                '("Sing,"
                  "O goddess, the anger"
                  "O goddess, the anger"
                  "of Achilles son"
                  "of Peleus, that brought"))
  (check-equal? (Buffer-cur b) (Point 1 0 0)))
(let ([ b (new-Buffer sample-lines)])
  (execute-key-events b (list       #\t
                                    #\O
                                    #\V
                                    (new key-event% [key-code #\v] [control-down #t])
                                    #\v
                                    'down
                                    #\o
                                    'right
                                    #\d))
  (check-equal? (Buffer-lines b)
                '("Sing, O goddess, the anger"
                  "of Aceus, that brought"))
  (check-equal? (Buffer-cur b) (Point 1 5 5)))
(let ([ b (new-Buffer sample-lines)])
  (execute-key-events b (list       #\t
                                    #\O
                                    #\i
                                    #\backspace
                                    'down
                                    #\return
                                    #\o
                                    'escape))
  (check-equal? (Buffer-lines b)
                '("Sing O goddess, the anger"
                  "of A"
                  "ochilles son"
                  "of Peleus, that brought"))
  (check-equal? (Buffer-cur b) (Point 2 0 0)))
(require (only-in racket/gui/base the-clipboard))
(let ([ b (new-Buffer sample-lines)])
  (send the-clipboard set-clipboard-string "123\n456" (current-milliseconds))
  (execute-key-events b (list       #\2
                                    #\$
                                    #\I
                                    (new key-event% [key-code #\v] [meta-down #t])
                                    'escape))
  (check-equal? (Buffer-lines b)
                '("Sing, O goddess, the anger"
                  "123"
                  "456of Achilles son"
                  "of Peleus, that brought"))
  (check-equal? (Buffer-cur b) (Point 2 2 2)))
(let ([ b (new-Buffer sample-lines)])
  (execute-key-events b (list       #\2
                                    #\j
                                    #\w
                                    (new key-event% [key-code #\v] [control-down #t])
                                    #\e
                                    #\k
                                    #\A
                                    'release
                                    #\:
                                    'escape))
  (check-equal? (Buffer-lines b)
                '("Sing, O goddess, the anger"
                  "of Achill:es son"
                  "of Peleus:, that brought"))
  (check-equal? (Buffer-cur b) (Point 1 9 9)))
(let ([ b (new-Buffer sample-lines)])
  (execute-key-events b (list       #\$
                                    #\x
                                    #\x))
  (check-equal? (Buffer-lines b)
                '("Sing, O goddess, the ang"
                  "of Achilles son"
                  "of Peleus, that brought"))
  (check-equal? (Buffer-cur b) (Point 0 23 +inf.0)))
(let ([ b (new-Buffer sample-lines)])
  (execute-key-events b (list       #\o
                                    'release
                                    #\1
                                    'escape
                                    #\u))
  (check-equal? (Buffer-lines b)
                '("Sing, O goddess, the anger"
                  "of Achilles son"
                  "of Peleus, that brought"))
  (check-equal? (Buffer-cur b) (Point 0 25 +inf.0)))
(let ([ b (new-Buffer sample-lines)])
  (execute-key-events b (list       #\o
                                    'release
                                    #\1
                                    'escape
                                    #\x))
  (check-equal? (Buffer-lines b)
                '("Sing, O goddess, the anger" ; to be fixed, "1" should be ""
                  ""
                  "of Achilles son" "of Peleus, that brought"))
  (check-equal? (Buffer-cur b) (Point 1 0 0)))
(let ([ b (new-Buffer sample-lines)])
  (execute-key-events b (list       #\f
                                    #\,
                                    #\r
                                    #\return
                                    #\u))
  (check-equal? (Buffer-lines b)
                '("Sing, O goddess, the anger"
                  "of Achilles son"
                  "of Peleus, that brought"))
  (check-equal? (Buffer-cur b) (Point 0 5 5)))
(let ([ b (new-Buffer sample-lines)])
  (execute-key-events b (list       #\/
                                    #\,
                                    #\return
                                    #\n
                                    #\n
                                    #\n
                                    #\N))
  (check-equal? (Buffer-lines b)
                '("Sing, O goddess, the anger"
                  "of Achilles son"
                  "of Peleus, that brought"))
  (check-equal? (Buffer-cur b) (Point 2 9 9)))

(let ([ b (new-Buffer sample-lines)])
  (execute-key-events b (list       #\?
                                    #\,
                                    #\return
                                    #\n
                                    #\n
                                    #\n
                                    #\N))
  (check-equal? (Buffer-lines b)
                '("Sing, O goddess, the anger"
                  "of Achilles son"
                  "of Peleus, that brought"))
  (check-equal? (Buffer-cur b) (Point 0 4 4)))

(let ([ b (new-Buffer sample-lines)])
  (execute-key-events b (list       #\G
                                    #\:
                                    #\%
                                    #\s
                                    #\/
                                    #\/
                                    #\backspace
                                    #\o
                                    #\f
                                    #\/
                                    #\a
                                    #\n
                                    #\d
                                    #\/
                                    #\g
                                    #\c
                                    #\return
                                    #\n
                                    #\y
                                    #\j))
  (check-equal? (Buffer-lines b)
                '("Sing, O goddess, the anger"
                  "of Achilles son"
                  "and Peleus, that brought"))
  (check-equal? (Buffer-cur b) (Point 2 0 0)))

(let ([ b (new-Buffer sample-lines)])
  (execute-key-events b (list       #\G
                                    #\:
                                    #\%
                                    #\s
                                    #\/
                                    #\o
                                    #\f
                                    #\/
                                    #\a
                                    #\n
                                    #\d
                                    #\/
                                    #\g
                                    #\c
                                    #\return
                                    #\n
                                    #\y
                                    #\j))
  (check-equal? (Buffer-lines b)
                '("Sing, O goddess, the anger"
                  "of Achilles son"
                  "and Peleus, that brought"))
  (check-equal? (Buffer-cur b) (Point 2 0 0)))
(let ([ b (new-Buffer sample-lines)])
  (execute-key-events b (list       #\G
                                    #\:
                                    #\%
                                    #\s
                                    #\/
                                    #\o
                                    #\f
                                    #\/
                                    #\a
                                    #\n
                                    #\d
                                    #\/
                                    #\g
                                    #\c
                                    #\return
                                    #\n
                                    #\a
                                    #\j))
  (check-equal? (Buffer-lines b)
                '("Sing, O goddess, the anger"
                  "of Achilles son"
                  "and Peleus, that brought"))
  (check-equal? (Buffer-cur b) (Point 2 0 0)))
(let ([ b (new-Buffer sample-lines)])
  (execute-key-events b (list       #\G
                                    #\:
                                    #\%
                                    #\s
                                    #\/
                                    #\o
                                    #\f
                                    #\/
                                    #\a
                                    #\n
                                    #\d
                                    #\/
                                    #\g
                                    #\c
                                    #\return
                                    #\a
                                    #\j))
  (check-equal? (Buffer-lines b)
                '("Sing, O goddess, the anger"
                  "and Achilles son"
                  "and Peleus, that brought"))
  (check-equal? (Buffer-cur b) (Point 2 0 0)))
(let ([ b (new-Buffer sample-lines)])
  (execute-key-events b (list #\:
                              #\%
                              #\s
                              #\/
                              #\o
                              #\/
                              #\x
                              #\return))
  (check-equal? (Buffer-lines b)
                '("Sing, O gxddess, the anger"
                  "xf Achilles son"
                  "xf Peleus, that brought"))
  (check-equal? (Buffer-cur b) (Point 0 0 0)))
(let ([ b (new-Buffer sample-lines)])
  (execute-key-events b (list #\:
                              #\%
                              #\s
                              #\/
                              #\o
                              #\/
                              #\x
                              #\/
                              #\g
                              #\return))
  (check-equal? (Buffer-lines b)
                '("Sing, O gxddess, the anger"
                  "xf Achilles sxn"
                  "xf Peleus, that brxught"))
  (check-equal? (Buffer-cur b) (Point 0 0 0)))
(let ([ b (new-Buffer sample-lines)])
  (execute-key-events b (list #\:
                              #\%
                              #\s
                              #\/
                              #\o
                              #\/
                              #\x
                              #\/
                              #\g
                              #\return
                              #\u))
  (check-equal? (Buffer-lines b)
                '("Sing, O goddess, the anger"
                  "of Achilles son"
                  "of Peleus, that brought"))
  (check-equal? (Buffer-cur b) (Point 0 0 0)))
(let ([ b (new-Buffer sample-lines)])
  (execute-key-events b (list #\:
                              #\%
                              #\s
                              #\/
                              #\o
                              #\/
                              #\x
                              #\/
                              #\g
                              #\c
                              #\return
                              #\y
                              #\y
                              #\Y
                              #\u))
  (check-equal? (Buffer-lines b)
                '("Sing, O gxddess, the anger"
                  "xf Achilles son"
                  "of Peleus, that brought"))
  (check-equal? (Buffer-cur b) (Point 1 0 0)))
(let ([ b (new-Buffer sample-lines)])
  (execute-key-events b (list #\:
                              #\%
                              #\s
                              #\/
                              #\o
                              #\/
                              #\x
                              #\return
                              #\u))
  (check-equal? (Buffer-lines b)
                '("Sing, O goddess, the anger"
                  "of Achilles son"
                  "of Peleus, that brought"))
  (check-equal? (Buffer-cur b) (Point 0 0 0)))

(let ([ b (new-Buffer '("Sing, O goddess, the anger"
                        "of Achilles son"
                        "of Peleus, that brought"
                        ", the anger"
                        "of Achilles son"))])
  (execute-key-events b (list #\:
                              #\%
                              #\s
                              #\/
                              #\o
                              #\/
                              #\x
                              #\return))
  (check-equal? (Buffer-lines b)
                '("Sing, O gxddess, the anger"
                  "xf Achilles son"
                  "xf Peleus, that brought"
                  ", the anger"
                  "xf Achilles son"))
  (check-equal? (Buffer-cur b) (Point 0 0 0)))

(let ([ b (new-Buffer sample-lines)])
  (execute-key-events b (list 
                         #\d
                         #\l))
  (check-equal? (Buffer-lines b)
                '("ing, O goddess, the anger"
                  "of Achilles son"
                  "of Peleus, that brought"))
  (check-equal? (Buffer-cur b) (Point 0 0 0)))