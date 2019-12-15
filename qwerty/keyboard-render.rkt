#lang racket

(require 2htdp/image 2htdp/universe lang/posn "utilities.rkt")

(provide generate-keyboard image->bitmap save-image)

(define (generate-keyboard keyboard width height #:foreground (fg "black") #:background (bg "white"))
  (let* ((w (floor (/ width 10)))
         (h (floor (/ height 3)))
         (size (floor (/ (+ w h) 12)))
         (normal-key (rectangle w h "outline" fg))
         (keyboard-background (rectangle width height "solid" bg))
         (kbd (format-keyboard keyboard)))
    (letrec ((column (λ (lst result)
                     (cond
                       ((empty? lst) result)
                       (else (column (cdr lst) (above result (row (car lst) empty-image)))))))
             (row (λ (lst result)
                    (cond
                      ((empty? lst) result)
                      (else (row (cdr lst) (beside result (key (car lst))))))))
             (key (λ (char)
                    (overlay (text (string char) size fg) normal-key))))
      (overlay (column kbd empty-image) keyboard-background))))

(define (image->bitmap image width height empty-bitmap)
  (let ((dc (send empty-bitmap make-dc)))
    (send dc clear)
    (send image draw dc 0 0 0 0 width height 0 0 #f)
    empty-bitmap))