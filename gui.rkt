#lang racket
(module+ test (require rackunit))

(require racket/gui "buffer.rkt")

(define (new-editor-frame b)
  (define min-width  800)
  (define min-height 800)
  (define text-color (make-object color% "black"))
  (define font-size  16)
  (define font-family 'modern) ; fixed width
  (define fixed-font (make-object font% font-size font-family))
  (define frame (new frame% [label "Editor"]))
  (define msg   (new message% [parent frame] [label "No news"]))
  (send msg min-width min-width)
  (define subeditor-canvas%
    (class canvas%
      (define/override (on-char event)
        (send msg set-label "subeditor-canvas key event")
        (define k (send event get-key-code))
        (match k
          [(? char? k) (buffer-insert-char! b k)
                       (buffer-move-point! b 1)]
          ['left       (buffer-move-point! b -1)]
          ['right      (buffer-move-point! b  1)]
          [_           (void)])
        (send canvas on-paint))
      (define/override (on-paint)
        (define dc (send canvas get-dc))
        ; (send msg set-label "on-paint")
        (send dc clear)
        (send dc set-text-foreground text-color)
        (send dc set-font fixed-font)
        ; draw line
        (for/fold ([y 0]) ([l (text-lines (buffer-text b))])          
          (send dc draw-text (line-string l) 0 y)
          (+ y font-size 1))
        ; draw points
        (define-values (font-width font-height _ __) (send dc get-text-extent "M"))
        (for ([p (buffer-points b)])
          (define-values (r c) (mark-row+column p b))
          (define x (* c font-width))
          (define y (* r font-height))
          (send dc draw-line x y x (+ y font-height))))
      (super-new)))
  (define canvas (new subeditor-canvas% [parent frame]))
  (send canvas min-client-width  400)
  (send canvas min-client-height 400)
  (send frame show #t))

(module+ test
  (require (submod "buffer.rkt" test))
  (new-editor-frame illead-buffer)
)