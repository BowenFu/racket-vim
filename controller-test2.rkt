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
  (execute-key-events b (list #\v
                              #\/
                              #\f
                              #\return
                              #\d))
  (check-equal? (Buffer-lines b)
                '(" Achilles son"
                  "of Peleus, that brought")))

(let ([ b (new-Buffer sample-lines)])
  (execute-key-events b (list #\v
                              #\t
                              #\O
                              #\return
                              #\d))
  (check-equal? (Buffer-lines b)
                '("O goddess, the anger"
                  "of Achilles son"
                  "of Peleus, that brought")))

(let ([ b (new-Buffer sample-lines)])
  (execute-key-events b (list #\c
                              #\/
                              #\P
                              'release
                              #\return
                              'release
                              #\1
                              #\2
                              'escape))
  (check-equal? (Buffer-lines b)
                '("12Peleus, that brought")))

(let ([ b (new-Buffer sample-lines)])
  (execute-key-events b (list #\f
                              #\O
                              #\m
                              #\m
                              #\j
                              #\g
                              #\U
                              #\`
                              #\m
                              ))
  (check-equal? (Buffer-lines b)
                '("Sing, O GODDESS, THE ANGER"
                  "OF ACHIlles son"
                  "of Peleus, that brought")))

(let ([ b (new-Buffer sample-lines)])
  (execute-key-events b (list #\f
                              #\O
                              #\i
                              'release
                              #\(
                              'escape
                              #\j
                              #\b
                              #\i
                              'release
                              #\)
                              'escape
                              #\d
                              #\%))
  (check-equal? (Buffer-lines b)
                '("Sing, Achilles son"
                  "of Peleus, that brought")))

(let ([ b (new-Buffer sample-lines)])
  (execute-key-events b (list #\A
                              'release
                              #\<
                              'down
                              #\>
                              'escape
                              #\d
                              #\a
                              #\<
                              ))
  (check-equal? (Buffer-lines b)
                '("Sing, O goddess, the anger"
                  "of Peleus, that brought")))

(let ([ b (new-Buffer sample-lines)])
  (execute-key-events b (list #\f
                              #\,
                              #\a
                              'release
                              #\{
                              #\{
                              'escape
                              #\j
                              #\A
                              'release
                              #\}
                              #\}
                              'escape
                              #\k
                              #\d
                              #\2
                              #\a
                              #\}
                              ))
  (check-equal? (Buffer-lines b)
                '("Sing,"
                  "of Peleus, that brought")))

(let ([ b (new-Buffer sample-lines)])
  (execute-key-events b (list #\j
                              #\f
                              'shift
                              #\A
                              #\J
                              #\m
                              #\m
                              #\e
                              #\j
                              #\d
                              #\'
                              #\m))
  (check-equal? (Buffer-lines b)
                '("Sing, O goddess, the anger"
                  "that brought")))

(let ([ b (new-Buffer sample-lines)])
  (execute-key-events b (list #\1
                              #\|
                              #\d
                              #\2
                              #\|))
  (check-equal? (Buffer-lines b)
                '("ing, O goddess, the anger"
                  "of Achilles son"
                  "of Peleus, that brought")))

(let ([ b (new-Buffer sample-lines)])
  (execute-key-events b (list #\?
                              #\o
                              #\f
                              #\return
                              #\#
                              #\d
                              #\*))
  (check-equal? (Buffer-lines b)
                '("Sing, O goddess, the anger"
                  "of Peleus, that brought")))

(let ([ b (new-Buffer sample-lines)])
  (execute-key-events b (list #\2
                              #\/
                              #\o
                              #\f
                              #\return
                              #\d
                              #\2
                              'shift
                              #\*))
  (check-equal? (Buffer-lines b)
                '("Sing, O goddess, the anger"
                  "of Achilles son"
                  "of Peleus, that brought")))

(let ([ b (new-Buffer sample-lines)])
  (execute-key-events b (list #\9
                              #\0
                              #\%
                              #\d
                              #\1
                              #\0
                              #\0
                              #\%))
  (check-equal? (Buffer-lines b)
                '("Sing, O goddess, the anger"
                  "of Achilles son")))

(let ([ b (new-Buffer sample-lines)])
  (execute-key-events b (list #\/
                              #\o
                              #\return
                              #\d
                              #\3
                              #\?
                              #\return))
  (check-equal? (Buffer-lines b)
                '("Sing, O gon"
                  "of Peleus, that brought")))

(let ([ b (new-Buffer sample-lines)])
  (execute-key-events b (list #\j
                              #\f
                              #\A
                              #\v
                              #\e
                              'escape
                              #\j
                              #\`
                              #\<
                              #\d
                              #\'
                              #\>))
  (check-equal? (Buffer-lines b)
                '("Sing, O goddess, the anger"
                  "chilles son"
                  "of Peleus, that brought")))

(let ([ b (new-Buffer sample-lines)])
  (execute-key-events b (list (new key-event% [key-code #\v] [control-down #t])
                              #\j
                              #\5
                              #\A
                              #\1
                              'escape))
  (check-equal? (Buffer-lines b)
                '("S111111ing, O goddess, the anger"
    "o11111f Achilles son"
    "of Peleus, that brought")))
