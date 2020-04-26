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

    (define last-search-motions #f)

    (define named-regs (make-hash))

    ; marks separate from regs
    (define local-marks (make-vector 26))
    (define global-marks (make-vector 26))
    (define special-marks (make-hash))
    
    (define/public (get-reg name)
      (match name
        ['yank yank-reg]
        ['last-cmd last-command]
        ['last-motions last-motions]
        ['last-search-motions last-search-motions]
        [_ (error 'missing-case-in-get-reg)]))

    (define/public (set-reg name reg)
      (match name
        ['yank (set! yank-reg reg)]
        ['last-cmd (set! last-command reg)]
        ['last-motions (set! last-motions reg)]
        ['last-search-motions (set! last-search-motions reg)]
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
      (set-reg 'last-motions reg))
    
    (define/public (get-last-search-motions)
      (get-reg 'last-search-motions))
    
    (define/public (set-last-search-motions! reg)
      (set-reg 'last-search-motions reg))
    
    (define/public (get-mark char)
      (cond
        [(char-alphabetic? char)
         (cond
           [(char-lower-case? char)
            (define index (- (char->integer char) (char->integer #\a)))
            (vector-ref local-marks index)]
           [(char-upper-case? char)
            (define index (- (char->integer char) (char->integer #\A)))
            (vector-ref global-marks index)])]
        [else (hash-ref special-marks char)]))
    
    (define/public (set-mark! char point)
      (cond
        [(char-alphabetic? char)
         (cond
        [(char-lower-case? char)
         (define index (- (char->integer char) (char->integer #\a)))
         (vector-set! local-marks index point)]
        [(char-upper-case? char)
         (define index (- (char->integer char) (char->integer #\A)))
         (vector-set! global-marks index point)])]
        [else (hash-set! special-marks char point)]))
    
    (define/public (get-named-reg char)
        (hash-ref named-regs char))
    
    (define/public (set-named-reg! char reg)
        (hash-set! named-regs char reg))
    ))
