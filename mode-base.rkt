#lang racket
(provide (all-defined-out))

(define mode%
  (class object%
    (super-new)
    (abstract on-char draw-points)
    (define/public (get-status-line)
      "")))
