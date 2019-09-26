#lang racket

(require racket/gui plot "shared.rkt" "genetic-algorithm.rkt")

(define WIDTH 1400)

(define main-window (new frame% [label "GENETIC ALGORITHM"]
                         [style (list 'no-resize-border)]
                         [width WIDTH]))

(define main-panel (new horizontal-panel% [parent main-window]))

(define setup-panel (new vertical-panel%
                         [parent main-panel]
                         [alignment '(left top)]
                         [stretchable-width #f]))

(define canvas-panel (new horizontal-panel%
                          [parent main-panel]
                          [alignment (list 'center 'center)]))
(define function-canvas (new canvas% [parent canvas-panel]))
(define performance-canvas (new canvas% [parent canvas-panel]))

(define fn (new combo-field%
                [label "FUNCTION"]
                [parent setup-panel]
                [choices (list "(λ (x) (+ (* x (sin (expt x 4))) (cos (* x x))))"
                               "(λ (x) (- (+ 480 (* -1 (abs (* x (sin (sqrt (abs x)))))))))"
                               "(λ (x y) (+ 21.5 (* x (sin (* 4 pi x))) (* y (sin (* 20 pi y)))))")]
                [init-value "(λ (x y) (+ 21.5 (* x (sin (* 4 pi x))) (* y (sin (* 20 pi y)))))"]))

(define fn-type (new check-box%
                     [label "3D"]
                     [parent setup-panel]
                     [value #t]))

(define threshold (new text-field%
                       [label "THRESHOLD"]
                       [parent setup-panel]
                       [init-value "500"]))

(define animation (new text-field%
                       [label "ANIMATION SPEED"]
                       [parent setup-panel]
                       [init-value "0.03"]))

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

(define resolution (new text-field%
                        [label "RESOLUTION"]
                        [parent setup-panel]
                        [init-value "10"]))

(define x-min (new text-field%
                   [label "X MIN"]
                   [parent setup-panel]
                   [init-value "-3.1"]))

(define x-max (new text-field%
                   [label "X MAX"]
                   [parent setup-panel]
                   [init-value "12.1"]))

(define y-min (new text-field%
                   [label "Y MIN*"]
                   [parent setup-panel]
                   [init-value "4.1"]))

(define y-max (new text-field%
                   [label "Y MAX*"]
                   [parent setup-panel]
                   [init-value "5.8"]))

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

(define x-entry (new text-field%
                     [label "X"]
                     [parent setup-panel]
                     [enabled #f]))

(define y-entry (new text-field%
                     [label "Y*"]
                     [parent setup-panel]
                     [enabled #f]))

(define fx-entry (new text-field%
                      [label "F(X Y*)"]
                      [parent setup-panel]
                      [enabled #f]))

(define start (new button%
                   [label "START"]
                   [parent setup-panel]
                   [stretchable-width #t]
                   [stretchable-height #t]
                   [callback (λ (b e)
                               (let ((fn (get-value fn))
                                     (fnt (send fn-type get-value));
                                     (th (get-value threshold))
                                     (as (get-value animation))
                                     (sl (if (= 0 (send selection get-selection)) 'roulette 'tournament))
                                     (tk (get-value tournament-k))
                                     (res (get-value resolution))
                                     (el (get-value elitism))
                                     (x-min (get-value x-min))
                                     (x-max (get-value x-max))
                                     (y-min (get-value y-min));
                                     (y-max (get-value y-max));
                                     (pc (get-value pc))
                                     (pm (get-value pm))
                                     (lg (get-value last-generation))
                                     (nind (get-value number-individuals)))
                                 (let-values ([(points best best-for-generation med-for-generation) (genetic-algorithm #:function fn
                                                                                                                       #:3d fnt
                                                                                                                       #:threshold th
                                                                                                                       #:selection sl
                                                                                                                       #:tournament-k tk
                                                                                                                       #:resolution res
                                                                                                                       #:elitism el
                                                                                                                       #:x-min x-min
                                                                                                                       #:x-max x-max
                                                                                                                       #:y-min y-min
                                                                                                                       #:y-max y-max
                                                                                                                       #:pc pc
                                                                                                                       #:pm pm
                                                                                                                       #:last-generation lg
                                                                                                                       #:number-of-individuals nind)])
                                   (plot-performance best-for-generation med-for-generation)
                                   (send x-entry set-value (real->decimal-string (first best) 5))
                                   (send y-entry set-value (if fnt (real->decimal-string (second best) 5) ""))
                                   (send fx-entry set-value (real->decimal-string (if fnt (third best) (second best)) 5))
                                   (plot-function-and-points fn fnt points x-min x-max y-min y-max as))))]))

(define (get-value widget)
  (eval (read (open-input-string (send widget get-value)))))

(send main-window show #t)
(plot-background-alpha 0)

(define (plot-function-and-points fn 3d list-of-points x-min x-max y-min y-max sleep-time)
  (if 3d
      (let* ((z-max (apply max (map (λ (points) (apply max (map third points))) list-of-points)))
             (z-min (apply min (map (λ (points) (apply min (map third points))) list-of-points)))
             (plot-background (plot3d-bitmap (surface3d fn x-min x-max y-min y-max)
                                             #:title "MAXIMIZATION"
                                             #:x-label "X"
                                             #:y-label "Y"
                                             #:z-label "F(X, Y)"
                                             #:x-min x-min
                                             #:x-max x-max
                                             #:y-min y-min
                                             #:y-max y-max
                                             #:z-min z-min
                                             #:z-max z-max
                                             #:width (- (/ (send canvas-panel get-width) 2) 40)
                                             #:height (- (send canvas-panel get-height) 40))))
        (for-each
         (λ (population)
           (define dc (send function-canvas get-dc))
           (send dc clear)
           (send dc draw-bitmap plot-background 0 0)
           (plot3d/dc (points3d population
                                #:x-min x-min
                                #:x-max x-max
                                #:y-min y-min
                                #:y-max y-max
                                #:z-min z-min
                                #:z-max z-max
                                #:color 'red
                                #:alpha 0.5)
                      dc
                      0 0
                      (- (/ (send canvas-panel get-width) 2) 40)
                      (- (send canvas-panel get-height) 40)
                      #:title "MAXIMIZATION"
                      #:x-label "X"
                      #:y-label "Y"
                      #:z-label "F(X, Y)")
           (sleep/yield sleep-time))
         list-of-points))
      (let* ((fx-max (apply max (map (λ (points) (apply max (map second points))) list-of-points)))
             (fx-min (apply min (map (λ (points) (apply min (map second points))) list-of-points)))
             (plot-background (plot-bitmap (function fn x-min x-max)
                                             #:title "MAXIMIZATION"
                                             #:x-label "X"
                                             #:y-label "F(X)"
                                             #:x-min x-min
                                             #:x-max x-max
                                             #:y-min fx-max
                                             #:y-max fx-min
                                             #:width (- (/ (send canvas-panel get-width) 2) 40)
                                             #:height (- (send canvas-panel get-height) 40))))
        (for-each
         (λ (population)
           (define dc (send function-canvas get-dc))
           (send dc clear)
           (send dc draw-bitmap plot-background 0 0)
           (plot/dc (points population
                             #:x-min x-min
                             #:x-max x-max
                             #:y-min fx-max
                             #:y-max fx-min
                             #:color 'red
                             #:alpha 0.5)
                    dc
                    0 0
                    (- (/ (send canvas-panel get-width) 2) 40)
                    (- (send canvas-panel get-height) 40)
                    #:title "MAXIMIZATION"
                    #:x-label "X"
                    #:y-label "F(X)")
           (sleep/yield sleep-time))
         list-of-points))))

(define (plot-performance y-by-gen med-by-gen)
  (define dc (send performance-canvas get-dc))
  (send dc clear)
  (plot/dc (list (lines y-by-gen #:color 'red #:label "Best")
                 (lines med-by-gen #:color 'blue #:label "Average"))
           (send performance-canvas get-dc)
           0 0
           (- (/ (send canvas-panel get-width) 2) 40)
           (- (send canvas-panel get-height) 40)
           #:title "PERFORMANCE"
           #:x-label "GENERATION"
           #:y-label "Y"))