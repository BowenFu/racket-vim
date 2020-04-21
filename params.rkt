#lang racket
(provide (all-defined-out))

(struct Command (op start-motions-lst scope-motions inserted-lines) #:transparent)

(define (make-Command op scope-motions #:op-params [op-params '()] #:start-motions-lst [start-motions-lst '()])
  (Command op start-motions-lst scope-motions op-params))

