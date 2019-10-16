#lang racket

(require 2htdp/image 2htdp/universe)

(provide animate-knight-tour)

(define TILE-SIZE 30)
(define WHITE-TILE (square TILE-SIZE "solid" "Light Goldenrod"))
(define BLACK-TILE (square TILE-SIZE "solid" "Medium Brown"))
(define PASSED-TILE (square TILE-SIZE "solid" (color 0 255 0 120)))
(define SQUARE-TILE (above (beside WHITE-TILE BLACK-TILE) (beside BLACK-TILE WHITE-TILE)))
(define KNIGHT (text "♘" TILE-SIZE "black"))

(define (create-board n)
  (if (odd? n) (error "Odd number")
  (letrec ((rec (λ (n result)
                  (cond
                    ((zero? n) result)
                    (else (rec (sub1 n) (beside SQUARE-TILE result)))))))
    (let ((line (rec (/ n 2) empty-image)))
      (letrec ((outer-rec (λ (n result)
                           (cond
                             ((zero? n) result)
                             (else (outer-rec (sub1 n) (above line result)))))))
  (outer-rec (/ n 2) empty-image))))))

(define (add-passed-tile l-xy board #:img (img PASSED-TILE))
  (underlay/xy board
               (* TILE-SIZE (first l-xy))
               (* TILE-SIZE (second l-xy))
               img))

(define (create-board-list tour)
  (letrec ((rec (λ (pos lst actual-board result)
                  (cond
                    ((empty? lst) (reverse (cons (add-passed-tile pos actual-board) result)))
                    (else (let ((board (add-passed-tile pos actual-board)))
                            (rec (car lst) (cdr lst) board (cons board result))))))))
    (let ((board (create-board 8)))
      (rec (car tour) (cdr tour) board '()))))

;'((0 0) (1 1) (2 2) (3 3) (4 4) (5 5) (6 6) (7 7) (7 6) (7 5) (7 4))

(define (board-list-with-knight tour)
  (map (λ (pos board) (add-passed-tile pos board #:img KNIGHT)) tour (create-board-list tour)))

(define (animate-knight-tour tour as)
  (run-movie as (board-list-with-knight tour)))