#lang racket

(provide substitude-from-point substitude-within-range substitude-once)

(require "core.rkt" "common-utils.rkt" "wrapped-move-scope.rkt" "diff.rkt" "diff-utils.rkt")

(define (substitude-line-from-col src dst line col mode)
  (define substitude-func
    (match mode
      ['first regexp-replace]
      ['all regexp-replace*]
      [_ (error 'missing-case)]))
  (string-append
   (substring line 0 col)
   (substitude-func src (substring line col) dst)))

(define (diff-for-each-line row new-line lines)
  (define start (Point row 0 0))
  (define scope (Scope start start #t #t 'line))
  (get-replace-diffs scope start (list new-line) 'line lines))

(define (substitude-from-point src dst p lines mode)
  (define-values (row col) (Point-row-col p))
  (define-values (_ this after) (before-this-after lines row))
  (define new-this (substitude-line-from-col src dst this col mode))
  (define this-diff-lst
    (cond
      [(equal? this new-this) '()]
      [else (diff-for-each-line row new-this lines)]))
  (define new-diff-lst
    (for/fold ([diff-lst this-diff-lst])
              ([l after]
               [r (in-naturals (add1 row))])
      (define new-l (substitude-line-from-col src dst l 0 mode))
      (cond
        [(equal? l new-l) diff-lst]
        [else (define new-diffs (diff-for-each-line r new-l lines))
              (append new-diffs diff-lst)])))
  (define-values (__ new-lines) (redo-diffs new-diff-lst lines))
  (values new-lines new-diff-lst))

; end-line inclusive.
(define (substitude-within-range src dst start-line end-line lines mode)
  (define ranged-lines (drop (take lines (add1 end-line)) start-line))
  (define diffs
    (for/fold ([diff-lst '()])
              ([l ranged-lines]
               [r (in-naturals start-line)])
      (define new-l (substitude-line-from-col src dst l 0 mode))
      (cond
        [(equal? l new-l) diff-lst]
        [else (define new-diffs (diff-for-each-line r new-l lines))
              (append new-diffs diff-lst)])))
  (define-values (__ new-lines) (redo-diffs diffs lines))
  (values new-lines diffs))

(define (substitude-once src dst p lines)
  (define-values (row col) (Point-row-col p))
  (define old-line (list-ref lines row))
  (define new-line (substitude-line-from-col src dst old-line col 'first))
  (define diff-lst
    (cond
      [(equal? old-line new-line) '()]
      [else (diff-for-each-line row new-line lines)]))
  (define-values (__ new-lines) (redo-diffs diff-lst lines))
  (values new-lines diff-lst))
