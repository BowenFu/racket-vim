#lang racket

(require "diff.rkt"
         "wrapped-move-scope.rkt")

(module+ test
  (require rackunit))

(provide diff-manager%)
  
; (: empty-diff? (-> Diff-item Boolean))
(define (empty-diff? diff)
  (match-define (Diff-item old-region new-region) diff)
  (match-define (Region old-scope old-lines) old-region)
  (match-define (Region new-scope new-lines) new-region)
  (define empty-lines? (andmap empty? (list old-lines new-lines)))
  (cond
    [empty-lines?
     (andmap empty-scope? (list old-scope new-scope))]
    [(equal-scope? old-scope new-scope) (equal? old-lines new-lines)]
    [else #f]))

(module+ test
  (let ([scope1 (Scope (Point 0 1 1) (Point 0 1 1) #t #f 'char)]
        [scope2 (Scope (Point 0 1 1) (Point 0 1 1) #t #t 'char)]
        [scope3 (Scope (Point 0 2 2) (Point 0 2 2) #t #f 'char)])
    (define region1-0 (Region scope1 '()))
    (define region1-1 (Region scope1 '("1")))
    (define region2-0 (Region scope2 '()))
    (define region2-1 (Region scope2 '("2")))
    (define region3-0 (Region scope3 '()))
    (check-equal? (empty-diff? (Diff-item region1-0 region1-0)) #t)
    (check-equal? (empty-diff? (Diff-item region1-0 region3-0)) #t)
    (check-equal? (empty-diff? (Diff-item region1-0 region1-1)) #f)
    (check-equal? (empty-diff? (Diff-item region2-0 region2-0)) #f)
    (check-equal? (empty-diff? (Diff-item region2-0 region2-1)) #f)
    (check-equal? (empty-diff? (Diff-item region2-1 region2-1)) #t)))

(define diff-manager%
  (class object%
    (super-new)
    (define diff-index 0)
    ;(new (old))
    (define diff-stack '())
    
    (define/public (push-diffs! diff-lst)
      (when (not (andmap empty-diff? diff-lst))
        (set! diff-stack (cons diff-lst (drop diff-stack diff-index)))
        (set! diff-index 0)))

    ;(: undo-last (-> (Listof String) (Values (Option Point) (Option (Listof String)))))
    (define/public (undo-last lines)
      (cond
        [(< diff-index (length diff-stack))
         (set! diff-index (add1 diff-index))
         (undo-diffs (list-ref diff-stack (sub1 diff-index)) lines)]
        [else
         (values #f #f)]))

    ;(: redo-next (-> (Listof String) (Values (Option Point) (Option (Listof String)))))
    (define/public (redo-next lines)
      (cond
        [(> diff-index 0)
         (set! diff-index
                (sub1 diff-index))
         (redo-diffs (list-ref diff-stack diff-index) lines)]
        [else
         (values #f lines)]))

    (define/public (diff-stack-len-index)
      (- (length diff-stack) diff-index))

    (define/public (combine-since! len)
      (unless (= diff-index 0) (error 'diff-index-not-0))
      (define-values (after before) (split-at-right diff-stack len))
      (set! diff-stack (cons (flatten after) before)))
    ))