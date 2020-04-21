#lang racket

(provide (all-defined-out))

(require "mode.rkt" "core.rkt" "diff-manager.rkt" "reg-manager.rkt")

(define mode-switcher%
  (class object%
    (super-new)
    (define current-mode (new normal-mode%))
    (define/public (get-current-mode)
      current-mode)
    (define/public (enter-mode! mode)
      (set! current-mode mode))))

(define controller%
  (class object%
    (super-new)
    [init-field buffer]
    (define mode-switcher (new mode-switcher%))
    (define diff-manager (new diff-manager%))
    (define reg-manager (new reg-manager%))
    (define/public (get-buffer)
      buffer)
    (define/public (on-char event)
      (define current-mode (send mode-switcher get-current-mode))
      (send current-mode on-char event buffer mode-switcher diff-manager reg-manager))
    (define/public (draw-points dc start-row)
      (define current-mode (send mode-switcher get-current-mode))
      (send current-mode draw-points dc buffer start-row))
    (define/public (get-status-line)
      (define current-mode (send mode-switcher get-current-mode))
      (send current-mode get-status-line))))

