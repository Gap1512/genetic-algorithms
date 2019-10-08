#lang racket

(require racket/gui plot "genetic-algorithm.rkt" "utilities.rkt")

(define WIDTH 600)
(define HEIGHT 200)

(define main-window (new frame% [label "GENETIC ALGORITHM"]
                         [style (list 'no-resize-border)]
                         [width WIDTH]
                         [height WIDTH]))

(define main-panel (new horizontal-panel% [parent main-window]))

(define setup-panel (new vertical-panel%
                         [parent main-panel]
                         [style (list 'border)]
                         [alignment '(left top)]
                         [stretchable-width #f]))

(define results-panel (new vertical-panel%
                           [parent main-panel]))

(define lines-panel (new vertical-panel%
                         [parent results-panel]
                         [style (list 'border)]
                         [alignment '(center center)]))

(define performance-panel (new vertical-panel%
                               [parent results-panel]
                               [style (list 'border)]
                               [min-height HEIGHT]
                               [stretchable-height #f]
                               [alignment '(center center)]))

(define lines-canvas (new canvas%
                          [parent lines-panel]))

(define performance-canvas (new canvas%
                                [parent performance-panel]))

(define number-cities (new text-field%
                                [label "NUMBER OF CITIES"]
                                [parent setup-panel]
                                [init-value "6"]))

(define animation (new text-field%
                       [label "ANIMATION SPEED"]
                       [parent setup-panel]
                       [init-value "0.03"]))

(define alpha (new text-field%
                       [label "ALPHA"]
                       [parent setup-panel]
                       [init-value "0.5"]))

(define selection (new radio-box%
                       [label "SELECTION"]
                       [parent setup-panel]
                       [choices (list "ROULETTE" "TOURNAMENT")]))

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
                [init-value "0.6"]))

(define pm (new text-field%
                [label "PM"]
                [parent setup-panel]
                [init-value "0.01"]))

(define last-generation (new text-field%
                             [label "LAST GENERATION"]
                             [parent setup-panel]
                             [init-value "70"]))

(define number-individuals (new text-field%
                                [label "NUMBER OF INDIVIDUALS"]
                                [parent setup-panel]
                                [init-value "50"]))

(define pt (new text-field%
                     [label "TOTAL DISTANCE"]
                     [parent setup-panel]
                     [enabled #f]))

(define start (new button%
                   [label "START"]
                   [parent setup-panel]
                   [stretchable-width #t]
                   [stretchable-height #t]
                   [callback (λ (b e)
                               (let ((nc (get-value number-cities))
                                     (as (get-value animation))
                                     (al (get-value alpha))
                                     (sl (if (= 0 (send selection get-selection)) 'roulette 'tournament))
                                     (tk (get-value tournament-k))
                                     (el (get-value elitism))
                                     (pc (get-value pc))
                                     (pm (get-value pm))
                                     (lg (get-value last-generation))
                                     (nind (get-value number-individuals)))
                                 (let-values ([(populations best best-for-generation med-for-generation) (time (genetic-algorithm #:selection sl
                                                                                                                            #:tournament-k tk
                                                                                                                            #:elitism el
                                                                                                                            #:resolution nc
                                                                                                                            #:pc pc
                                                                                                                            #:pm pm
                                                                                                                            #:last-generation lg
                                                                                                                            #:number-of-individuals nind))])
                                   (send pt set-value (real->decimal-string (first best) 5))
                                   (plot-performance best-for-generation med-for-generation)
                                   (plot-lines populations as al 'red)
                                   (plot-result (list-tail best 1) 'blue))))]))

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

(define (plot-lines populations as al color)
  (let ((aux (λ (population)
               (plot/dc (map (λ (x) (lines x #:color color #:alpha al)) population)
                        (send lines-canvas get-dc)
                        0 0
                        (send lines-panel get-width)
                        (send lines-panel get-height)
                        #:title "CITIES"
                        #:x-label "X"
                        #:y-label "Y")
               (sleep/yield as))))
  (map aux populations)))

(define (plot-result path color)
  (plot/dc (lines path #:color color)
           (send lines-canvas get-dc)
           0 0
           (send lines-panel get-width)
           (send lines-panel get-height)
           #:title "CITIES"
           #:x-label "X"
           #:y-label "Y"))