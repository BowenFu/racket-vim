#lang racket

(require "core.rkt")

(provide search)

(define (search p lines pattern direction)
  (define search-func
    (match direction
      ['forwards search-forwards]
      ['backwards search-backwards]
      [_ (error 'missing-case)]))
  (search-func p lines pattern))

(define (search-string-forwards row str pattern [col-inc 0])
  (define pair (regexp-match-positions pattern str))
  (cond
    [(not pair) #f]
    [else
     (define p (first pair))
     (define col0 (+ (car p) col-inc))
     (define col1 (+ (cdr p) col-inc))
     (list (Point row col0 col0) (Point row col1 col1))]))

(define (search-string-backwards row str pattern [col-inc 0])
  (define pair (regexp-match-positions* pattern str))
  (cond
    [(empty? pair) #f]
    [else
     (define p (last pair))
     (define col0 (+ (car p) col-inc))
     (define col1 (+ (cdr p) col-inc))
     (list (Point row col0 col0) (Point row col1 col1))]))

;(require racket/trace)
;(trace search-string-backwards)

(define (search-forwards p lines pattern)
  (match-define (Point row col _) p)
  (define line (list-ref lines row))
  (define (search-the-line)
    (define rest-str (substring line (add1 col)))
    (search-string-forwards row rest-str pattern (add1 col)))
  (cond
    [(search-the-line)]
    [(for/or ([l (drop lines (add1 row))]
              [new-row (in-naturals (add1 row))])
       (search-string-forwards new-row l pattern))]
    [else
     (define begin-to-point
       (append (take lines row) (list (substring line 0 col))))
     (for/or ([l begin-to-point]
              [new-row (in-naturals)])
       (search-string-forwards new-row l pattern))]
    ))

(define (search-backwards p lines pattern)
  (match-define (Point row col _) p)
  (define line (list-ref lines row))
  (define (search-the-line)
    (define rest-str (substring line (add1 col)))
    (search-string-backwards row rest-str pattern (add1 col)))
  (cond
    [(let ([begin-to-point
             (append (take lines row) (list (substring line 0 col)))])
       (for/or ([l (reverse begin-to-point)]
                [new-row (in-range (sub1 (length begin-to-point)) -1 -1)])
         (search-string-backwards new-row l pattern)))]
    [(for/or ([l (reverse (drop lines (add1 row)))]
              [new-row (in-range (sub1 (length lines)) (add1 row) -1)])
       (search-string-backwards new-row l pattern))]
    [else (search-the-line)]
    ))