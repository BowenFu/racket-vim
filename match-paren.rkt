#lang racket

(provide %-point a-paren-pair)

(require "core.rkt" "common-utils.rkt" "Scope.rkt")

(module+ test (require rackunit))

(define (get-paren-pair char)
  (match char
    [(or #\( #\)) (cons #\( #\))]
    [(or #\{ #\}) (cons #\{ #\})]
    [(or #\[ #\]) (cons #\[ #\])]
    [_ (error 'missing-case (~v char))]))

(define (line-match-forwards paren-pair line counter)
  (for/fold ([l-counter counter]
             [col- 0])
            ([col (in-naturals)]
             [c line]
             #:when (or (equal? c (car paren-pair)) (equal? c (cdr paren-pair)))
             #:break (= l-counter 0)) ; todo start from p
    (define local-counter
      (cond
        [(equal? c (car paren-pair))
         (add1 l-counter)]
        [else (sub1 l-counter)]))
    (values local-counter col)))

(define (string-reverse x)
  (list->string (reverse (string->list x))))

(define (line-match-backwards paren-pair line counter)
  (for/fold ([l-counter counter]
             [col- (sub1 (string-length line))])
            ([col (in-range (sub1 (string-length line)) -1 -1)]
             [c (string-reverse line)]
             #:when (or (equal? c (car paren-pair)) (equal? c (cdr paren-pair)))
             #:break (= l-counter 0)) ; todo start from p
    (define local-counter
      (cond
        [(equal? c (car paren-pair))
         (add1 l-counter)]
        [else (sub1 l-counter)]))
    (values local-counter col)))

(define (%-right-point p lines [check? #t] [paren-lst #f])
  (define-values (p-row p-col) (Point-row-col p))
  (define-values (_ this after) (before-this-after lines p-row))
  (define current-char (string-ref this p-col))
  (define paren-pair (or paren-lst (get-paren-pair current-char)))
  (unless (or (not check?) (equal? current-char (car paren-pair))) (error 'incorrect-paren (~v current-char)))
  (define-values (init-counter init-col) (line-match-forwards paren-pair (substring this (add1 p-col)) 1))
  (define init-col+ (+ init-col p-col 1))
  (define-values (c pp)
    (for/fold ([counter init-counter]
               [point (Point p-row init-col+ init-col+)])
              ([row (in-naturals (add1 p-row))]
               [line after]
               #:break (= counter 0))
      (define-values (l-counter l-col) (line-match-forwards paren-pair line counter))
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

(define (%-left-point p lines [check? #t] [paren-lst #f])
  (define-values (p-row p-col) (Point-row-col p))
  (define-values (before this _) (before-this-after lines p-row))
  (define current-char (string-ref this p-col))
  (define paren-pair (or paren-lst (get-paren-pair current-char)))
  (unless (or (not check?) (equal? current-char (cdr paren-pair))) (error 'incorrect-paren (~v current-char)))
  (define-values (init-counter init-col) (line-match-backwards paren-pair (substring this 0 p-col) -1))
  (define-values (c pp)
    (for/fold ([counter init-counter]
               [point (Point p-row init-col init-col)])
              ([row (in-range (sub1 p-row) -1 -1)]
               [line (reverse before)]
               #:break (= counter 0))
      (define-values (l-counter l-col) (line-match-backwards paren-pair line counter))
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

(define (is-left-paren? char [paren-pair #f])
  (equal? char (car (or paren-pair (get-paren-pair char)))))

(define (is-right-paren? char [paren-pair #f])
  (equal? char (cdr (or paren-pair (get-paren-pair char)))))

(define (%-point p lines)
  (define-values (p-row p-col) (Point-row-col p))
  (define-values (before this after) (before-this-after lines p-row))
  (define char (string-ref this p-col))
  (cond
    [(is-left-paren? char)
     (%-right-point p lines)]
    [(is-right-paren? char)
     (%-left-point p lines)]
    [else (error 'not-on-a-paren)]))

(module+ test
  (let ([lines '("((-"
                 "[)])")])
    (check-equal? (%-point (%-point (Point 0 1 1) lines) lines) (Point 0 1 1))))

(define (a-paren-pair paren-pair p lines count) ;todo fix count
  (define-values (p-row p-col) (Point-row-col p))
  (define this (list-ref lines p-row))
  (define char (string-ref this p-col))
  (define left-p
    (cond
      [(is-left-paren? char paren-pair) p]
      [else (%-left-point p lines #f paren-pair)]))
  (define right-p
    (cond
      [(is-right-paren? char paren-pair) p]
      [else (%-right-point p lines #f paren-pair)]))
  (cons left-p right-p))
  
(module+ test
  (let ([lines '("((-"
                 "[)])")])
    (check-equal? (a-paren-pair '(#\( . #\)) (Point 0 1 1) lines 1)
                  (cons (Point 0 1 1) (Point 1 1 1)))
    (check-equal? (a-paren-pair '(#\[ . #\]) (Point 1 1 1) lines 1)
                  (cons (Point 1 0 0) (Point 1 2 2)))
    )
  )