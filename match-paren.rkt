#lang racket

(provide %-point)

(require "core.rkt" "common-utils.rkt")

(module+ test (require rackunit))

(define (get-paren-list char)
  (match char
    [(or #\( #\)) (list #\( #\))]
    [(or #\{ #\}) (list #\{ #\})]
    [(or #\[ #\]) (list #\[ #\])]
    [_ (error 'missing-case (~v char))]))

(define (line-match-forwards paren-list line counter)
  (for/fold ([l-counter counter]
             [col- 0])
            ([col (in-naturals)]
             [c line]
             #:when (memv c paren-list)
             #:break (= l-counter 0)) ; todo start from p
    (define local-counter
      (cond
        [(equal? c (first paren-list))
         (add1 l-counter)]
        [else (sub1 l-counter)]))
    (values local-counter col)))

(define (string-reverse x)
  (list->string (reverse (string->list x))))

(define (line-match-backwards paren-list line counter)
  (for/fold ([l-counter counter]
             [col- (sub1 (string-length line))])
            ([col (in-range (sub1 (string-length line)) -1 -1)]
             [c (string-reverse line)]
             #:when (memv c paren-list)
             #:break (= l-counter 0)) ; todo start from p
    (define local-counter
      (cond
        [(equal? c (first paren-list))
         (add1 l-counter)]
        [else (sub1 l-counter)]))
    (values local-counter col)))

(define (%-right-point p lines)
  (define-values (p-row p-col) (Point-row-col p))
  (define-values (_ this after) (before-this-after lines p-row))
  (define current-char (string-ref this p-col))
  (define paren-list (get-paren-list current-char))
  (unless (equal? current-char (first paren-list)) (error 'incorrect-paren (~v current-char)))
  (define-values (init-counter init-col) (line-match-forwards paren-list (substring this (add1 p-col)) 1))
  (define init-col+ (+ init-col p-col 1))
  (define-values (c pp)
    (for/fold ([counter init-counter]
               [point (Point p-row init-col+ init-col+)])
              ([row (in-naturals (add1 p-row))]
               [line after]
             #:break (= counter 0))
      (define-values (l-counter l-col) (line-match-forwards paren-list line counter))
      (values l-counter (Point row l-col l-col))
      ))
  (unless (equal? c 0) (error 'no-matched-parens (~v c)))
  pp)

(module+ test
  (check-equal? (%-right-point (Point 0 1 1) '("((-[)])")) (Point 0 4 4))
  (check-equal? (%-right-point (Point 0 3 3) '("((-["
                                               ")])")) (Point 1 1 1))
  (check-equal? (%-right-point (Point 0 0 0) '("(123" "3))")) (Point 1 1 1))
  )

(define (%-left-point p lines)
  (define-values (p-row p-col) (Point-row-col p))
  (define-values (before this _) (before-this-after lines p-row))
  (define current-char (string-ref this p-col))
  (define paren-list (get-paren-list current-char))
  (unless (equal? current-char (second paren-list)) (error 'incorrect-paren (~v current-char)))
  (define-values (init-counter init-col) (line-match-backwards paren-list (substring this 0 p-col) -1))
  (define-values (c pp)
    (for/fold ([counter init-counter]
               [point (Point p-row init-col init-col)])
              ([row (in-range (sub1 p-row) -1 -1)]
               [line (reverse before)]
             #:break (= counter 0))
      (define-values (l-counter l-col) (line-match-backwards paren-list line counter))
      (values l-counter (Point row l-col l-col))
      ))
  (unless (equal? c 0) (error 'no-matched-parens (~v c)))
  pp)

(module+ test
  (check-equal? (%-left-point (Point 0 4 4) '("((-[)])")) (Point 0 1 1))
  (check-equal? (%-left-point (Point 1 1 1) '("((-["
                                               ")])")) (Point 0 3 3))
  (check-equal? (%-left-point (Point 1 1 1) '("(123" "3))")) (Point 0 0 0))
  )

(define (is-left-paren? char)
  (equal? char (first (get-paren-list char))))

(define (%-point p lines)
  (define-values (p-row p-col) (Point-row-col p))
  (define-values (before this after) (before-this-after lines p-row))
  (define left-paren? (is-left-paren? (string-ref this p-col)))
  (if left-paren?
      (%-right-point p lines)
      (%-left-point p lines)))

(module+ test
  (let ([lines '("((-"
                 "[)])")])
  (check-equal? (%-point (%-point (Point 0 1 1) lines) lines) (Point 0 1 1)))
  )