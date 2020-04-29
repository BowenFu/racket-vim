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

(displayln "Test Substitude")

(let ([ b (new-Buffer sample-lines)])
  (execute-key-symbols b (list      ':
                                    's
                                    '/
                                    'i
                                    '/
                                    'o
                                    '/
                                    'g
                                    '<CR>))
  (check-equal? (Buffer-lines b)
                '("Song, O goddess, the anger"
                  "of Achilles son"
                  "of Peleus, that brought"))
  (check-equal? (Buffer-cur b) (Point 0 0 0)))

(let ([ b (new-Buffer sample-lines)])
  (execute-key-symbols b (list      ':
                                    '\1
                                    '\,
                                    '\2
                                    's
                                    '/
                                    'i
                                    '/
                                    'o
                                    '/
                                    'g
                                    '<CR>))
  (check-equal? (Buffer-lines b)
                '("Song, O goddess, the anger"
                  "of Acholles son"
                  "of Peleus, that brought"))
  (check-equal? (Buffer-cur b) (Point 0 0 0)))