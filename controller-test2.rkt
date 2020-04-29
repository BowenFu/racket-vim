#lang racket

(require rackunit
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
  (execute-key-symbols b (list 'v
                               '/
                               'f
                               '<CR>
                               'd))
  (check-equal? (Buffer-lines b)
                '(" Achilles son"
                  "of Peleus, that brought")))

(let ([ b (new-Buffer sample-lines)])
  (execute-key-symbols b (list 'v
                               't
                               'O
                               '<CR>
                               'd))
  (check-equal? (Buffer-lines b)
                '("O goddess, the anger"
                  "of Achilles son"
                  "of Peleus, that brought")))

(let ([ b (new-Buffer sample-lines)])
  (execute-key-symbols b (list 'c
                               '/
                               'P
                               '<CR>
                               '\1
                               '\2
                               '<Esc>))
  (check-equal? (Buffer-lines b)
                '("12Peleus, that brought")))

(let ([ b (new-Buffer sample-lines)])
  (execute-key-symbols b (list 'f
                               'O
                               'm
                               'm
                               'j
                               'g
                               'U
                               '\`
                               'm
                               ))
  (check-equal? (Buffer-lines b)
                '("Sing, O GODDESS, THE ANGER"
                  "OF ACHIlles son"
                  "of Peleus, that brought")))

(let ([ b (new-Buffer sample-lines)])
  (execute-key-symbols b (list 'f
                               'O
                               'i
                               '\(
                               '<Esc>
                               'j
                               'b
                               'i
                               '\)
                               '<Esc>
                               'd
                               '%))
  (check-equal? (Buffer-lines b)
                '("Sing, Achilles son"
                  "of Peleus, that brought")))

(let ([ b (new-Buffer sample-lines)])
  (execute-key-symbols b (list 'A
                               '<
                               '<Down>
                               '>
                               '<Esc>
                               'd
                               'a
                               '<
                               ))
  (check-equal? (Buffer-lines b)
                '("Sing, O goddess, the anger"
                  "of Peleus, that brought")))

(let ([ b (new-Buffer sample-lines)])
  (execute-key-symbols b (list 'f
                               '\,
                               'a
                               '\{
                               '\{
                               '<Esc>
                               'j
                               'A
                               '\}
                               '\}
                               '<Esc>
                               'k
                               'd
                               '\2
                               'a
                               '\}
                               ))
  (check-equal? (Buffer-lines b)
                '("Sing,"
                  "of Peleus, that brought")))

(let ([ b (new-Buffer sample-lines)])
  (execute-key-symbols b (list 'j
                               'f
                               'shift
                               'A
                               'J
                               'm
                               'm
                               'e
                               'j
                               'd
                               '\'
                               'm))
  (check-equal? (Buffer-lines b)
                '("Sing, O goddess, the anger"
                  "that brought")))

(let ([ b (new-Buffer sample-lines)])
  (execute-key-symbols b (list '\1
                               '\|
                               'd
                               '\2
                               '\|))
  (check-equal? (Buffer-lines b)
                '("ing, O goddess, the anger"
                  "of Achilles son"
                  "of Peleus, that brought")))

(let ([ b (new-Buffer sample-lines)])
  (execute-key-symbols b (list '?
                               'o
                               'f
                               '<CR>
                               '\#
                               'd
                               '*))
  (check-equal? (Buffer-lines b)
                '("Sing, O goddess, the anger"
                  "of Peleus, that brought")))

(let ([ b (new-Buffer sample-lines)])
  (execute-key-symbols b (list '\2
                               '/
                               'o
                               'f
                               '<CR>
                               'd
                               '\2
                               'shift
                               '*))
  (check-equal? (Buffer-lines b)
                '("Sing, O goddess, the anger"
                  "of Achilles son"
                  "of Peleus, that brought")))

(let ([ b (new-Buffer sample-lines)])
  (execute-key-symbols b (list '\9
                               '\0
                               '%
                               'd
                               '\1
                               '\0
                               '\0
                               '%))
  (check-equal? (Buffer-lines b)
                '("Sing, O goddess, the anger"
                  "of Achilles son")))

(let ([ b (new-Buffer sample-lines)])
  (execute-key-symbols b (list '/
                               'o
                               '<CR>
                               'd
                               '\3
                               '?
                               '<CR>))
  (check-equal? (Buffer-lines b)
                '("Sing, O gon"
                  "of Peleus, that brought")))

(let ([ b (new-Buffer sample-lines)])
  (execute-key-symbols b (list 'j
                               'f
                               'A
                               'v
                               'e
                               '<Esc>
                               'j
                               '\`
                               '<
                               'd
                               '\'
                               '>))
  (check-equal? (Buffer-lines b)
                '("Sing, O goddess, the anger"
                  "chilles son"
                  "of Peleus, that brought")))

(let ([ b (new-Buffer sample-lines)])
  (execute-key-symbols b (list '<C-v>
                               'j
                               '\5
                               'A
                               '\1
                               '<Esc>))
  (check-equal? (Buffer-lines b)
                '("S11111ing, O goddess, the anger"
                  "o11111f Achilles son"
                  "of Peleus, that brought")))

(let ([ b (new-Buffer sample-lines)])
  (execute-key-symbols b (list '\2
                               'l
                               '\5
                               'X))
  (check-equal? (Buffer-lines b)
                '("ng, O goddess, the anger"
                  "of Achilles son"
                  "of Peleus, that brought")))

(let ([ b (new-Buffer sample-lines)])
  (execute-key-symbols b (list 
                          'q
                          'q
                          'd
                          'w
                          'q
                          'j
                          '@
                          'q
                          'j
                          '@
                          '@))
  (check-equal? (Buffer-lines b)
                '(", O goddess, the anger"
                  "Achilles son" "Peleus, that brought")))

(let ([ b (new-Buffer sample-lines)])
  (execute-key-symbols b (list 'q
                               'q
                               'x
                               'q
                               'u
                               '|2|
                               '@
                               'q
                               'u
                               'j
                               '@
                               'q
                               'u
                               'h
                               '|2|
                               '@
                               'q))
  (check-equal? (Buffer-lines b)
                '("Sing, O goddess, the anger"
                  "ofchilles son"
                  "of Peleus, that brought")))

(let ([ b (new-Buffer sample-lines)])
  (execute-key-symbols b (list '/
                               'o
                               'f
                               '/
                               '+
                               '|2|
                               '<CR>
                               'x))
  (check-equal? (Buffer-lines b)
                '("Sing, O goddess, the anger"
                  "of Achilles son"
                  "f Peleus, that brought")))

(let ([ b (new-Buffer sample-lines)])
  (execute-key-symbols b (list '?
                               'b
                               '?
                               '-
                               '|1|
                               '<CR>
                               'x))
  (check-equal? (Buffer-lines b)
                '("Sing, O goddess, the anger"
                  "f Achilles son"
                  "of Peleus, that brought")))
