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
  (execute-key-symbols b (list       '$
                                    'x
                                    'x))
  (check-equal? (Buffer-lines b)
                '("Sing, O goddess, the ang"
                  "of Achilles son"
                  "of Peleus, that brought"))
  (check-equal? (Buffer-cur b) (Point 0 23 +inf.0)))
(let ([ b (new-Buffer sample-lines)])
  (execute-key-symbols b (list       'o
                                    '\1
                                    '<Esc>
                                    'u))
  (check-equal? (Buffer-lines b)
                '("Sing, O goddess, the anger"
                  "of Achilles son"
                  "of Peleus, that brought"))
  (check-equal? (Buffer-cur b) (Point 0 25 +inf.0)))
(let ([ b (new-Buffer sample-lines)])
  (execute-key-symbols b (list       'o
                                    '\1
                                    '<Esc>
                                    'x))
  (check-equal? (Buffer-lines b)
                '("Sing, O goddess, the anger"
                  ""
                  "of Achilles son" "of Peleus, that brought"))
  (check-equal? (Buffer-cur b) (Point 1 0 0)))

(let ([ b (new-Buffer sample-lines)])
  (execute-key-symbols b (list       'f
                                    '\,
                                    'r
                                    '<CR>
                                    'u))
  (check-equal? (Buffer-lines b)
                '("Sing, O goddess, the anger"
                  "of Achilles son"
                  "of Peleus, that brought"))
  (check-equal? (Buffer-cur b) (Point 0 5 5)))

(let ([ b (new-Buffer sample-lines)])
  (execute-key-symbols b (list       '/
                                    '\,
                                    '<CR>
                                    'n
                                    'n
                                    'n
                                    'N))
  (check-equal? (Buffer-lines b)
                '("Sing, O goddess, the anger"
                  "of Achilles son"
                  "of Peleus, that brought"))
  (check-equal? (Buffer-cur b) (Point 2 9 9)))

(let ([ b (new-Buffer sample-lines)])
  (execute-key-symbols b (list       '?
                                    '\,
                                    '<CR>
                                    'n
                                    'n
                                    'n
                                    'N))
  (check-equal? (Buffer-lines b)
                '("Sing, O goddess, the anger"
                  "of Achilles son"
                  "of Peleus, that brought"))
  (check-equal? (Buffer-cur b) (Point 0 4 4)))

(let ([ b (new-Buffer sample-lines)])
  (execute-key-symbols b (list       'G
                                    ':
                                    '%
                                    's
                                    '/
                                    '/
                                    '<BACKSPACE>
                                    'o
                                    'f
                                    '/
                                    'a
                                    'n
                                    'd
                                    '/
                                    'g
                                    'c
                                    '<CR>
                                    'n
                                    'y
                                    'j))
  (check-equal? (Buffer-lines b)
                '("Sing, O goddess, the anger"
                  "of Achilles son"
                  "and Peleus, that brought"))
  (check-equal? (Buffer-cur b) (Point 2 0 0)))

(let ([ b (new-Buffer sample-lines)])
  (execute-key-symbols b (list       'G
                                    ':
                                    '%
                                    's
                                    '/
                                    'o
                                    'f
                                    '/
                                    'a
                                    'n
                                    'd
                                    '/
                                    'g
                                    'c
                                    '<CR>
                                    'n
                                    'y
                                    'j))
  (check-equal? (Buffer-lines b)
                '("Sing, O goddess, the anger"
                  "of Achilles son"
                  "and Peleus, that brought"))
  (check-equal? (Buffer-cur b) (Point 2 0 0)))
(let ([ b (new-Buffer sample-lines)])
  (execute-key-symbols b (list       'G
                                    ':
                                    '%
                                    's
                                    '/
                                    'o
                                    'f
                                    '/
                                    'a
                                    'n
                                    'd
                                    '/
                                    'g
                                    'c
                                    '<CR>
                                    'n
                                    'a
                                    'j))
  (check-equal? (Buffer-lines b)
                '("Sing, O goddess, the anger"
                  "of Achilles son"
                  "and Peleus, that brought"))
  (check-equal? (Buffer-cur b) (Point 2 0 0)))
(let ([ b (new-Buffer sample-lines)])
  (execute-key-symbols b (list       'G
                                    ':
                                    '%
                                    's
                                    '/
                                    'o
                                    'f
                                    '/
                                    'a
                                    'n
                                    'd
                                    '/
                                    'g
                                    'c
                                    '<CR>
                                    'a
                                    'j))
  (check-equal? (Buffer-lines b)
                '("Sing, O goddess, the anger"
                  "and Achilles son"
                  "and Peleus, that brought"))
  (check-equal? (Buffer-cur b) (Point 2 0 0)))
(let ([ b (new-Buffer sample-lines)])
  (execute-key-symbols b (list ':
                              '%
                              's
                              '/
                              'o
                              '/
                              'x
                              '<CR>))
  (check-equal? (Buffer-lines b)
                '("Sing, O gxddess, the anger"
                  "xf Achilles son"
                  "xf Peleus, that brought"))
  (check-equal? (Buffer-cur b) (Point 0 0 0)))
(let ([ b (new-Buffer sample-lines)])
  (execute-key-symbols b (list ':
                              '%
                              's
                              '/
                              'o
                              '/
                              'x
                              '/
                              'g
                              '<CR>))
  (check-equal? (Buffer-lines b)
                '("Sing, O gxddess, the anger"
                  "xf Achilles sxn"
                  "xf Peleus, that brxught"))
  (check-equal? (Buffer-cur b) (Point 0 0 0)))
(let ([ b (new-Buffer sample-lines)])
  (execute-key-symbols b (list ':
                              '%
                              's
                              '/
                              'o
                              '/
                              'x
                              '/
                              'g
                              '<CR>
                              'u))
  (check-equal? (Buffer-lines b)
                '("Sing, O goddess, the anger"
                  "of Achilles son"
                  "of Peleus, that brought"))
  (check-equal? (Buffer-cur b) (Point 0 0 0)))
(let ([ b (new-Buffer sample-lines)])
  (execute-key-symbols b (list ':
                              '%
                              's
                              '/
                              'o
                              '/
                              'x
                              '/
                              'g
                              'c
                              '<CR>
                              'y
                              'y
                              'Y
                              'u))
  (check-equal? (Buffer-lines b)
                '("Sing, O gxddess, the anger"
                  "xf Achilles son"
                  "of Peleus, that brought"))
  (check-equal? (Buffer-cur b) (Point 1 0 0)))
(let ([ b (new-Buffer sample-lines)])
  (execute-key-symbols b (list ':
                              '%
                              's
                              '/
                              'o
                              '/
                              'x
                              '<CR>
                              'u))
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
  (execute-key-symbols b (list ':
                              '%
                              's
                              '/
                              'o
                              '/
                              'x
                              '<CR>))
  (check-equal? (Buffer-lines b)
                '("Sing, O gxddess, the anger"
                  "xf Achilles son"
                  "xf Peleus, that brought"
                  ", the anger"
                  "xf Achilles son"))
  (check-equal? (Buffer-cur b) (Point 0 0 0)))

(let ([ b (new-Buffer sample-lines)])
  (execute-key-symbols b (list 
                         'd
                         'l))
  (check-equal? (Buffer-lines b)
                '("ing, O goddess, the anger"
                  "of Achilles son"
                  "of Peleus, that brought"))
  (check-equal? (Buffer-cur b) (Point 0 0 0)))
