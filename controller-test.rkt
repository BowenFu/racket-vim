#lang racket

(require rackunit
         (only-in racket/gui/base key-event% key-code-symbol?)
         "controller.rkt" "core.rkt")

(define (execute-key-symbols b lst)
  (define controller (new controller% [buffer b]))
  (for ([k-s lst])
    (send controller on-char k-s)))

(define sample-lines
  '("Sing, O goddess, the anger"
    "of Achilles son"
    "of Peleus, that brought"))
  
(let ([ b (new-Buffer sample-lines)])
  (execute-key-symbols b (list 'A '\1 '\2))
  (check-equal? (Buffer-lines b)
                '("Sing, O goddess, the anger12"
                  "of Achilles son"
                  "of Peleus, that brought")))
  
(let ([ b (new-Buffer sample-lines)])
  (execute-key-symbols b (list 'v
                              'A
                              'release
                              '\1
                              '<Esc>))
  (check-equal? (Buffer-lines b)
                '("Sing, O goddess, the anger1"
                  "of Achilles son"
                  "of Peleus, that brought")))
(let ([ b (new-Buffer sample-lines)])
  (execute-key-symbols b (list '<C-v>
                              'A
                              'release ; necessary for set org-lines
                              '\1
                              '<Esc>))
  (check-equal? (Buffer-lines b)
                '("S1ing, O goddess, the anger"
                  "of Achilles son"
                  "of Peleus, that brought")))
(let ([ b (new-Buffer sample-lines)])
  (execute-key-symbols b (list '>
                              '>
                              'd
                              'd
                              'p))
  (check-equal? (Buffer-lines b)
                '("of Achilles son"
                  "    Sing, O goddess, the anger"
                  "of Peleus, that brought")))
(let ([ b (new-Buffer sample-lines)])
  (execute-key-symbols b (list 'c
                              'i
                              'w
                              'release
                              '\1
                              '<Esc>))
  (check-equal? (Buffer-lines b)
                '("1, O goddess, the anger"
                  "of Achilles son"
                  "of Peleus, that brought")))
(let ([ b (new-Buffer sample-lines)])
  (execute-key-symbols b (list 'd
                              '\2
                              'f
                              '\,))
  (check-equal? (Buffer-lines b)
                '(" the anger"
                  "of Achilles son"
                  "of Peleus, that brought")))
(let ([ b (new-Buffer sample-lines)])
  (execute-key-symbols b (list 'd
                              '\2
                              't
                              '\,))
  (check-equal? (Buffer-lines b)
                '(", the anger"
                  "of Achilles son"
                  "of Peleus, that brought")))
(let ([ b (new-Buffer sample-lines)])
  (execute-key-symbols b (list 'd
                              '\2
                              'a
                              'W))
  (check-equal? (Buffer-lines b)
                '("goddess, the anger"
                  "of Achilles son"
                  "of Peleus, that brought")))
(let ([ b (new-Buffer sample-lines)])
  (execute-key-symbols b (list 'g
                              '<Esc>
                              '\2
                              '$
                              'd
                              'T
                              '| |
                              'd
                              'F
                              '| |
                              'd
                              'i
                              'w))
  (check-equal? (Buffer-lines b)
                '("Sing, O goddess, the anger"
                  "of "
                  "of Peleus, that brought")))
(let ([ b (new-Buffer sample-lines)])
  (execute-key-symbols b (list '<C-v>
                              'A
                              'release
                              '\1
                              '<Left>
                              '<Down>
                              '<Right>
                              '<Up>
                              '<Left>
                              '\2
                              '<Esc>))
  (check-equal? (Buffer-lines b)
                '("S21ing, O goddess, the anger"
                  "of Achilles son"
                  "of Peleus, that brought"))
  (check-equal? (Buffer-cur b) (Point 0 1 1)))
(let ([ b (new-Buffer sample-lines)])
  (execute-key-symbols b (list 'v
                              'i
                              'w
                              '<Esc>
                              'v
                              'a
                              'W
                              'd))
  (check-equal? (Buffer-lines b)
                '("O goddess, the anger"
                  "of Achilles son"
                  "of Peleus, that brought"))
  (check-equal? (Buffer-cur b) (Point 0 0 0)))
(let ([ b (new-Buffer sample-lines)])
  (execute-key-symbols b (list 'v
                              'a
                              '<Esc>
                              'g
                              'g
                              'f
                              '\,))
  (check-equal? (Buffer-cur b) (Point 0 4 4)))
(let ([ b (new-Buffer sample-lines)])
  (execute-key-symbols b (list   'f
                                'O
                                'R
                                '<Right>
                                '<Down>
                                '<Left>
                                '<Up>
                                '\1
                                '\2
                                '\4
                                '<BACKSPACE>
                                '\3
                                '<CR>
                                '\4
                                '\5
                                '\6
                                '<Esc>))
  (check-equal? (Buffer-lines b)
                '("Sing, O123"
                  "456s, the anger"
                  "of Achilles son"
                  "of Peleus, that brought"))
  (check-equal? (Buffer-cur b) (Point 1 2 2)))
(let ([ b (new-Buffer sample-lines)])
  (execute-key-symbols b (list   'g
                                'U
                                'U))
  (check-equal? (Buffer-lines b)
                '("SING, O GODDESS, THE ANGER"
                  "of Achilles son"
                  "of Peleus, that brought")))
(let ([ b (new-Buffer sample-lines)])
  (execute-key-symbols b (list   'g
                                'U
                                'U
                                'u))
  (check-equal? (Buffer-cur b) (Point 0 0 0))
  (check-equal? (Buffer-lines b)
                sample-lines)
  (check-equal? (Buffer-cur b) (Point 0 0 0)))
  
(let ([ b (new-Buffer sample-lines)])
  (execute-key-symbols b (list   'g
                                'U
                                'U
                                'u
                                '<C-r>))
  (check-equal? (Buffer-lines b)
                '("SING, O GODDESS, THE ANGER"
                  "of Achilles son"
                  "of Peleus, that brought"))
  (check-equal? (Buffer-cur b) (Point 0 0 0)))

(let ([ b (new-Buffer sample-lines)])
  (execute-key-symbols b (list     'f
                                  '\,
                                  '<C-v>
                                  'j
                                  'e
                                  'V
                                  'v
                                  'g
                                  '~))
  (check-equal? (Buffer-lines b)
                '("Sing, o GODDESS, THE ANGER"
                  "OF aCHILLES son"
                  "of Peleus, that brought"))
  (check-equal? (Buffer-cur b) (Point 1 11 11)))
(let ([ b (new-Buffer sample-lines)])
  (execute-key-symbols b (list       'f
                                    'O
                                    'v
                                    'j
                                    'r
                                    '-))
  (check-equal? (Buffer-lines b)
                '("Sing, --------------------"
                  "-------lles son"
                  "of Peleus, that brought"))
  (check-equal? (Buffer-cur b) (Point 1 7 7)))
(let ([ b (new-Buffer sample-lines)])
  (execute-key-symbols b (list       'f
                                    'O
                                    'v
                                    'j
                                    'r
                                    '-
                                    'w
                                    '\.))
  (check-equal? (Buffer-lines b)
                '("Sing, --------------------"
                  "-------lles ---"
                  "-------us, that brought"))
  (check-equal? (Buffer-cur b) (Point 2 7 7)))

(let ([ b (new-Buffer sample-lines)])
  (execute-key-symbols b (list       'G
                                    'A
                                    '<BACKSPACE>
                                    '<Esc>
                                    '\2
                                    'g
                                    'g
                                    'x))
  (check-equal? (Buffer-lines b)
                '("Sing, O goddess, the anger" "f Achilles son" "of Peleus, that brough"))
  (check-equal? (Buffer-cur b) (Point 1 0 0)))

(let ([ b (new-Buffer sample-lines)])
  (execute-key-symbols b (list       'f
                                    '\,
                                    'F
                                    'g
                                    'T
                                    'S
                                    't
                                    'O
                                    '\;
                                    'x
                                    '\,
                                    '\.))
  (check-equal? (Buffer-lines b)
                '("Sing, goddess, the anger"
                  "of Achilles son"
                  "of Peleus, that brought"))
  (check-equal? (Buffer-cur b) (Point 0 5 5)))

(let ([ b (new-Buffer sample-lines)])
  (execute-key-symbols b (list       't
                                    'O
                                    'r
                                    '<CR>
                                    'y
                                    'y
                                    'P))
  (check-equal? (Buffer-lines b)
                '("Sing,"
                  "O goddess, the anger"
                  "O goddess, the anger"
                  "of Achilles son"
                  "of Peleus, that brought"))
  (check-equal? (Buffer-cur b) (Point 1 0 0)))

(let ([ b (new-Buffer sample-lines)])
  (execute-key-symbols b (list       't
                                    'O
                                    'V
                                    '<C-v>
                                    'v
                                    '<Down>
                                    'o
                                    '<Right>
                                    'd))
  (check-equal? (Buffer-lines b)
                '("Sing, illes son"
                  "of Peleus, that brought"))
  (check-equal? (Buffer-cur b) (Point 0 6 6)))

(let ([ b (new-Buffer sample-lines)])
  (execute-key-symbols b (list       't
                                    'O
                                    'i
                                    '<BACKSPACE>
                                    '<Down>
                                    '<CR>
                                    'o
                                    '<Esc>))
  (check-equal? (Buffer-lines b)
                '("Sing O goddess, the anger"
                  "of A"
                  "ochilles son"
                  "of Peleus, that brought"))
  (check-equal? (Buffer-cur b) (Point 2 0 0)))

(require (only-in racket/gui/base the-clipboard))
(let ([ b (new-Buffer sample-lines)])
  (send the-clipboard set-clipboard-string "123\n456" (current-milliseconds))
  (execute-key-symbols b (list       '\2
                                    '$
                                    'I
                                    '<M-v>
                                    '<Esc>))
  (check-equal? (Buffer-lines b)
                '("Sing, O goddess, the anger"
                  "123"
                  "456of Achilles son"
                  "of Peleus, that brought"))
  (check-equal? (Buffer-cur b) (Point 2 2 2)))

(let ([ b (new-Buffer sample-lines)])
  (execute-key-symbols b (list       '\2
                                    'j
                                    'w
                                    '<C-v>
                                    'e
                                    'k
                                    'A
                                    ':
                                    '<Esc>))
  (check-equal? (Buffer-lines b)
                '("Sing, O goddess, the anger"
                  "of Achill:es son"
                  "of Peleus:, that brought"))
  (check-equal? (Buffer-cur b) (Point 1 9 9)))
