#lang racket
(provide (all-defined-out))

(require "core.rkt" "mode-base.rkt")

;;; 
;;; LINES
;;;

; The size of a line is the same as the font size plus one.
(define (line-size)
  (define multiplier
    ; see https://docs.racket-lang.org/gui/windowing-overview.html#%28part._display-resolution%29
    (case (system-type)
      [(macosx)        1]
      [(unix window)   96/72]
      [else            1]))
  (define font-size 16) ; 
  (inexact->exact (floor (+ 1 (* multiplier font-size)))))

(define line-cursor-mode%
  (class mode%
    (super-new)
    (define/override (draw-points dc b start-row)
      (define-values (font-width font-height _ __) (send dc get-text-extent "M"))
      (define p (Buffer-cur b))
      (define-values (r c) (Point-row-col p))
      (define x (* c font-width))
      (define line-height (line-size))
      (define y (* (- r start-row) line-height))
      (send dc draw-line x y x (+ y line-height)))))

(define block-cursor-mode%
  (class mode%
    (super-new)
    (define/override (draw-points dc b start-row)
      (send dc set-text-mode 'solid)
      (send dc set-text-background "black")
      (send dc set-text-foreground "white")
      (define-values (font-width font-height _ __) (send dc get-text-extent "M"))
      (define p (Buffer-cur b))
      ;(displayln (list 'draw-points 'p p))
      (define-values (r c) (Point-row-col p))
      (define x (* c font-width))
      (define line-height (line-size))
      (define y (* (- r start-row) line-height))
      (define l (list-ref (Buffer-lines b) r))
      (define Point-char (line-ref-char l c))
      (send dc draw-text (string Point-char) x y))))

(define visual-mode-base%
  (class mode%
    (super-new)
    (abstract get-scope)
    (define/override (draw-points dc b start-row)
      (send dc set-text-mode 'solid)
      (send dc set-text-background "black")
      (send dc set-text-foreground "white")
      (define-values (font-width font-height _ __) (send dc get-text-extent "M"))
      (let ([p (Buffer-cur b)])
        (define-values (start end) (apply values (get-scope b)))
        (define-values (r1 c1) (Point-row-col start))
        (define line-height (line-size))
        (define x (* c1 font-width))
        (define y (* (- r1 start-row) line-height))
        (define-values (r2 c2) (Point-row-col end))
        (cond
          [(equal? r2 r1) 
           (define str (Buffer-substring-at b r2 c1 (+ c2 1)))
           (send dc draw-text str x y)]
          [else
           (define lines (Buffer-lines b))
           (define l1 (list-ref lines (Point-row start)))
           (define l2 (list-ref lines (Point-row end)))
           (define str1 (Buffer-substring-at b r1 c1 (string-length l1)))
           (define str2 (Buffer-substring-at b r2 0 (+ c2 1)))
           (send dc draw-text str1 x y)
           (define x2 0)
           (define y2 (* (- r2 start-row) line-height))
           (send dc draw-text str2 x2 y2)
           (for ([ri (in-range (+ r1 1) r2)])
             (define xi 0)
             (define yi (* (- ri start-row) line-height))
             (define stri (list-ref lines ri))
             (send dc draw-text stri xi yi))])
        ))))

(define visual-line-mode-base%
  (class mode%
    (super-new)
    (abstract get-scope)
    (define/override (draw-points dc b start-row)
      (send dc set-text-mode 'solid)
      (send dc set-text-background "black")
      (send dc set-text-foreground "white")
      (define-values (font-width font-height _ __) (send dc get-text-extent "M"))
      (let ([p (Buffer-cur b)])
        (define-values (start end) (apply values (get-scope b)))
        (define-values (r1 _) (Point-row-col start))
        (define line-height (line-size))
        (define x 0)
        (define y (* (- r1 start-row) line-height))
        (define-values (r2 __) (Point-row-col end))
        (define lines (Buffer-lines b))
        (for ([ri (in-range r1 (+ r2 1))])
          (define xi 0)
          (define yi (* (- ri start-row) line-height))
          (define stri (list-ref lines ri))
          (send dc draw-text stri xi yi))
        ))))

(define visual-block-mode-base%
  (class mode%
    (super-new)
    (abstract get-scope)
    (define/override (draw-points dc b start-row)
      (send dc set-text-mode 'solid)
      (send dc set-text-background "black")
      (send dc set-text-foreground "white")
      (define-values (font-width font-height _ __) (send dc get-text-extent "M"))
      (let ([p (Buffer-cur b)])
        (define-values (start end) (apply values (get-scope b)))
        (define-values (r1 c1) (Point-row-col start))
        (define line-height (line-size))
        (define-values (r2 c2) (Point-row-col end))
        (define lines (Buffer-lines b))
        (define c-min (min c1 c2))
        (define c-max (max c1 c2))
        (define x (* c-min font-width))
        (for ([ri (in-range r1 (+ r2 1))])
          (define yi (* (- ri start-row) line-height))
          (define line (list-ref lines ri))
          (when (>= (string-length line) c-min)
            (define end-c (min (add1 c-max) (string-length line)))
            (define stri (substring line c-min end-c))
            (send dc draw-text stri x yi)))
        ))))
