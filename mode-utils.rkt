#lang racket

(provide (all-defined-out))

(require "core.rkt" "wrapped-move-scope.rkt" "scope.rkt" "change.rkt" "operators.rkt" "params.rkt")

(define (repeat c p lines reg-manager #:count [count 1])
  (match-define (Command op start-motions-lst scope-motions inserted-lines) c)
  (define new-p 
                  (for/fold ([new-p p])
                            ([motion start-motions-lst])
                    (move-point motion new-p lines)))
  (execute op scope-motions new-p lines reg-manager #:count count #:op-params inserted-lines))

(define (execute op motions p lines reg-manager #:count [count 1] #:op-params [op-params '()])
  (cond
    [motions
     (match op
       [(or (== delete-op) (== yank-op) (== pre-paste-op) (== post-paste-op))
        (for/fold
         ([pp p]
          [ls lines]
          [diffs '()])
         ([i count])
          (define scope (get-point-scope motions pp ls))
          (define-values (new-point new-lines new-diffs) (op scope p lines reg-manager))
          (values new-point new-lines (append new-diffs diffs)))]
       [(? procedure?)
        (for/fold
         ([pp p]
          [ls lines]
          [diffs '()])
         ([i count])
          (define scope (get-point-scope motions pp ls))
          (define-values (new-point new-lines new-diffs) (op scope p lines))
          (values new-point new-lines (append new-diffs diffs)))]
       ['change
        (define inserted-lines op-params)
        (define scope (get-point-scope motions p lines))
        ;(displayln (~e 'change scope rel-motion 'p p))
        (replace scope p lines inserted-lines (Scope-mode scope))]
       ['replace-op
        (define char op-params)
        (define scope (get-point-scope motions p lines))
        ;(displayln (~e 'replace-op 'scope scope))
        (replace-op scope p lines char)]
       [_ (error 'missing-execute-case (~a op))])]
    [else (values p lines '())]
    ))

(define (update-Buffer-and-diffs! b diff-manager)
  (lambda (new-p new-ls [new-diffs '()])
    (set-Buffer-cur! b new-p)
    (set-Buffer-lines! b new-ls)
    (send diff-manager push-diffs! new-diffs)))

(define (Buffer-delete-char! b *-mode?)
  (define p (Buffer-cur b))
  (define-values (new-point new-lines) (lines-delete-char-after-point p (Buffer-lines b) *-mode?))
  (set-Buffer-cur! b new-point)
  (set-Buffer-lines! b new-lines))

(define (update-count k count)
  (define digit (- (char->integer k) (char->integer #\0)))
  (cond
    [count
     (+ (* 10 count) digit)]
    [else digit]))

(define (scope-to-motion scope)
  (match-define (Scope start end dir include-real-end? mode) scope)
  (define rel-row (- (Point-row end) (Point-row start)))
  (define include-end (and dir include-real-end?))
  (define additional-count (if include-end 1 0))
  (cond
    [(equal? mode 'block)
     (define rel-col (+ (abs (- (Point-col end) (Point-col start))) additional-count))
     (Visual-Motion rel-row rel-col 'block)]
    [(equal? mode 'char)
     (cond
       [(equal? rel-row 0)
        (define count (- (Point-col end) (Point-col start)))
        (make-Motion 'right #:count (+ count additional-count))]
       [else
        (Visual-Motion rel-row (+ (Point-col end) additional-count) 'char)])]
    [(equal? mode 'line)
     (make-Motion 'down-line-mode #:count rel-row)]
    [else (error (~e 'not-implemented-in-scope-to-motion scope))]))

(define (move! motions p b)
  ;(displayln (list 'motions motions))
  (define lines (Buffer-lines b))
  (define new-point (move-point motions p lines))
  (set-Buffer-cur! b new-point))
