#lang racket

(require racket/gui "core.rkt" "controller.rkt" "key-event-key.rkt")

(define (new-editor-frame controller)
  (define min-width  800)
  (define min-height 800)
  (define text-color (make-object color% "black"))
  (define font-size  16)
  (define font-family 'modern) ; fixed width
  (define fixed-font (make-object font% font-size font-family))
  (define frame (new frame% [label "Editor"]))
  (define subeditor-canvas%
    (class canvas%
      (define/override (on-char event)
        (cond
          [(ignored-key-event? event) (void)]
          [else
           (define key-symbol (key-event->key-symbol event))
           (displayln (~e key-symbol))
           (send controller on-char key-symbol)
           (define b (send controller get-buffer))
           (send status-line set-label (string-append (~v (Buffer-cur b)) " " (send controller get-status-line)))
           (send canvas on-paint)]))
      (define/override (on-paint)
        (define b (send controller get-buffer))
        (render-buffer b canvas 16 controller))
      (define start-row 0)
      (define end-row #f)
      (define (render-buffer b canvas font-size controller)
        (define dc (send canvas get-dc))
        (send dc clear)
        (send dc suspend-flush)
        (send dc set-text-foreground text-color)
        (send dc set-text-background "white")
        (send dc set-font fixed-font)
        ;; Dimensions
        (define-values (width height) (send canvas get-client-size))
        (define fs font-size)
        (define ls (+ fs 1)) ; linesize -- 1 pixel for spacing
        ;; Placement of point relative to lines on screen
        (define num-lines-on-screen   (max 0 (quotient height ls)))
        (define-values (row col)      (Point-row-col (Buffer-cur b)))
        ;(displayln (list 'before: 'row row 'start-row start-row 'end-row end-row 'n num-lines-on-screen))
        (define n num-lines-on-screen)
        (when (not end-row)
          (set! end-row (+ start-row n -1)))
        (when (<= (length (Buffer-lines b)) num-lines-on-screen)
          (set! start-row 0)
          (set! end-row (sub1 (length (Buffer-lines b)))))
          
        (when (< row start-row)
          (define new-start-row row)
          (define new-end-row (+ new-start-row n -1))
          (set! start-row new-start-row)
          (set! end-row new-end-row)
          ;(displayln (list 'new-start-and-end start-row end-row))
          )

        (when (> row end-row)
          (define new-end-row row)
          (define new-start-row (- new-end-row n -1))
          (set! start-row new-start-row)
          (set! end-row   new-end-row)
          ;(displayln (list 'new-start-and-end start-row end-row))
          )

        ; draw-string : string real real -> real
        ;   draw string t at (x,y), return point to draw next string
        (define (draw-string t x y)
          (send dc draw-text t x y)
          (+ y ls))
        ; draw text
        (define after-end-row (add1 end-row))
        (define window-lines
          (drop (take (Buffer-lines b) after-end-row) start-row))
        ;(displayln (~e 'window-lines window-lines))
        (define ymin 0 #;(send canvas get-y))
        (define xmin 0 #;(send canvas get-x))
        (for/fold ([y ymin])
                  ([l window-lines])
          (draw-string l xmin y))
        ;(displayln (~v 'start-row start-row 'end-row end-row))
        (send controller draw-points dc start-row)
        ; resume flush
        (send dc resume-flush))
      (super-new)))
  (define canvas (new subeditor-canvas% [parent frame]))
  (send canvas min-client-width  400)
  (send canvas min-client-height 100)
  (define status-line (new message% [parent frame] [label "No news"]))
  (send status-line min-width min-width)
  (send frame show #t))

(module+ test
  (require (submod "core.rkt" test))
  (define b (new-Buffer '("Sing, O goddess, the anger"
                          "of Achilles son"
                          "of Peleus, that brought"
                          ", the anger"
                          "of Achilles son"
                          "x")))
  (define controller (new controller% [buffer b]))
  (new-editor-frame controller)
  )
