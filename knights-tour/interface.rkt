#lang racket

(require racket/gui plot "genetic-algorithm.rkt" "utilities.rkt" "chess-animation.rkt")

(define WIDTH 600)
(define HEIGHT 200)

(define main-window (new frame% [label "GENETIC ALGORITHM"]))
                         ;[style (list 'no-resize-border)]))

(define main-panel (new horizontal-panel% [parent main-window]))

(define setup-panel (new vertical-panel%
                         [parent main-panel]
                         [style (list 'border)]
                         [alignment '(left top)]
                         [stretchable-width #f]))

(define performance-panel (new vertical-panel%
                               [parent main-panel]
                               [style (list 'border)]
                               [min-width WIDTH]
                               [min-height WIDTH]
                               [alignment '(center center)]))

(define performance-canvas (new canvas%
                                [parent performance-panel]))

(define animation (new text-field%
                       [label "ANIMATION SPEED"]
                       [parent setup-panel]
                       [init-value "1"]))

(define selection (new radio-box%
                       [label "SELECTION"]
                       [parent setup-panel]
                       [choices (list "TOURNAMENT" "ROULETTE")]))

(define tournament-k (new text-field%
                          [label "K"]
                          [parent setup-panel]
                          [init-value "4"]))

(define elitism (new text-field%
                     [label "ELITISM"]
                     [parent setup-panel]
                     [init-value "2"]))

(define pc (new text-field%
                [label "PC"]
                [parent setup-panel]
                [init-value "0.9"]))

(define pm (new text-field%
                [label "PM"]
                [parent setup-panel]
                [init-value "0.005"]))

(define pa (new text-field%
                [label "PA"]
                [parent setup-panel]
                [init-value "0.5"]))

(define number-individuals (new text-field%
                                [label "NUMBER OF INDIVIDUALS"]
                                [parent setup-panel]
                                [init-value "50"]))

(define start (new button%
                   [label "START"]
                   [parent setup-panel]
                   [stretchable-width #t]
                   [stretchable-height #t]
                   [callback (λ (b e)
                               (let ((as (get-value animation))
                                     (sl (if (= 0 (send selection get-selection)) 'tournament  'roulette))
                                     (tk (get-value tournament-k))
                                     (el (get-value elitism))
                                     (pc (get-value pc))
                                     (pm (get-value pm))
                                     (pa (get-value pa))
                                     (nind (get-value number-individuals)))
                                 (let-values ([(best best-for-generation med-for-generation) (genetic-algorithm #:selection sl
                                                                                                                #:tournament-k tk
                                                                                                                #:elitism el
                                                                                                                #:pc pc
                                                                                                                #:pm pm
                                                                                                                #:pa pa
                                                                                                                #:number-of-individuals nind)])
                                   (plot-performance best-for-generation med-for-generation)
                                   (animate-knight-tour best as))))]))

(define (get-value widget)
  (eval (read (open-input-string (send widget get-value)))))

(send main-window show #t)

(define (plot-performance y-by-gen med-by-gen)
  (plot/dc (list (lines y-by-gen #:color 'red #:label "Best")
                 (lines med-by-gen #:color 'blue #:label "Average"))
           (send performance-canvas get-dc)
           0 0
           (send performance-panel get-width)
           (send performance-panel get-height)
           #:title "PERFORMANCE"
           #:x-label "GENERATION"
           #:y-label "FITNESS"))