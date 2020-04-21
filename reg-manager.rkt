#lang racket

(module+ test
  (require rackunit))

(provide (struct-out Reg) reg-manager% make-Reg)

(struct Reg (lines mode) #:transparent)

(define (make-Reg lines [mode 'char])
  (Reg lines mode))

(define reg-manager%
  (class object%
    (super-new)
    (define yank-reg (make-Reg '()))

    (define last-command #f)

    (define last-motions #f)
    
    (define/public (get-reg name)
      (match name
        ['yank yank-reg]
        ['last-cmd last-command]
        ['last-motions last-motions]
        [_ (error 'missing-case-in-get-reg)]))

    (define/public (set-reg name reg)
      (match name
        ['yank (set! yank-reg reg)]
        ['last-cmd (set! last-command reg)]
        ['last-motions (set! last-motions reg)]
        [_ (error 'missing-case-in-set-reg)]))

    (define/public (get-yank-reg)
      (get-reg 'yank))
    
    (define/public (set-yank-reg reg)
      (set-reg 'yank reg))

    (define/public (get-last-cmd)
      (get-reg 'last-cmd))
    
    (define/public (set-last-cmd reg)
      (set-reg 'last-cmd reg))
    
    (define/public (get-last-motions)
      (get-reg 'last-motions))
    
    (define/public (set-last-motions reg)
      (set-reg 'last-motions reg))))
