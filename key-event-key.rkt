#lang racket

(require racket/gui/base)

(provide key-event->key-symbol ignored-key-event?)

(define (ignored-key-event? event)
  (define k (send event get-key-code))
  (match k
    [(or 'release 'control 'shift 'rcontrol 'rshift)
         #t]
    [_ #f]))

(define (key-event->key-symbol event)
  (define k (send event get-key-code))
  (define base-key 
    (match k
      [#\return "CR"]
      [#\backspace "BACKSPACE"]
      [(? char? k) (string k)]
      ['escape "Esc"]
      ['left "Left"]
      ['right "Right"]
      ['up "Up"]
      ['down "Down"]
      [_ (error 'missing (~v k))]))
  (define complex-key
    (for/fold ([com-key base-key])
              ([func-S (list (cons (send event get-meta-down) "M")
                             ;(cons (send event get-shift-down) "S")
                             (cons (send event get-control-down) "C")
                             (cons (send event get-alt-down) "A"))])
      (cond
        [(car func-S) (string-append (cdr func-S) "-" com-key)]
        [else com-key])))
  (define final-string
    (cond
    [(> (string-length complex-key) 1) (string-append "<" complex-key ">")]
    [else complex-key]))
  (string->symbol final-string))