#lang racket

(provide macro-recorder%)

(define macro-recorder%
  (class object%
    (super-new)
    [init-field controller]
    (define macro-reg #f)
    (define macro-content '())
    (define last-execute-reg #f)
    (define/public (start-record-macro-to-reg! reg)
      (set! last-execute-reg #f)
      (set! macro-reg reg))
    (define/public (stop-record-macro-to-reg! reg-manager)
      (define fixed-content (reverse (drop macro-content 1)))
      (send reg-manager set-named-reg! macro-reg fixed-content)
      (set! macro-reg #f)
      (set! macro-content '()))
    (define/public (recording?)
      macro-reg)
    (define/public (execute-macro-from-reg macro-reg reg-manager diff-manager count)
      (cond
        [(equal? macro-reg #\@)
         (unless last-execute-reg (error 'no-previously-used-register))]
        [else
         (set! last-execute-reg macro-reg)])
      (define content (send reg-manager get-named-reg last-execute-reg))
      (define org-diff-len (send diff-manager diff-stack-len-index))
      (for ([i (or count 1)])
        (for ([k content])
          (send controller on-char k)))
      (send diff-manager combine-since! org-diff-len))
    (define/public (record! key-symbol)
      (when macro-reg (set! macro-content (cons key-symbol macro-content))))
    ))