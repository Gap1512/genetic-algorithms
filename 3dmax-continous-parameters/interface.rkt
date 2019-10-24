#lang racket

(require racket/gui plot "shared.rkt" "genetic-algorithm.rkt")

(define WIDTH 1400)

(define main-window (new frame% [label "GENETIC ALGORITHM"]
                         ;[style (list 'no-resize-border)]
                         [width WIDTH]))

(define main-panel (new horizontal-panel% [parent main-window]))

(define setup-panel (new vertical-panel%
                         [parent main-panel]
                         [alignment '(left top)]
                         [stretchable-width #f]))

(define canvas-panel (new horizontal-panel% [parent main-panel]))
(define performance-canvas (new canvas% [parent canvas-panel]))
(define function-canvas (new canvas% [parent canvas-panel]))

(define fn (new combo-field%
                [label "FUNCTION"]
                [parent setup-panel]
                [choices (list "(λ (x) (+ (* x (sin (expt x 4))) (cos (* x x))))"
                               "(λ (x) (- (+ 480 (* -1 (abs (* x (sin (sqrt (abs x)))))))))"
                               "(λ (x y) (+ (* x (sin (* 4 x))) (* 1.1 (sin (* 2 y)))))"
                               "(λ (x y) (+ 21.5 (* x (sin (* 4 pi x))) (* y (sin (* 20 pi y)))))")]
                [init-value "(λ (x y) (+ (* x (sin (* 4 x))) (* 1.1 (sin (* 2 y)))))"]))

(define fn-type (new text-field%
                     [label "NUMBER OF VARIABLES"]
                     [parent setup-panel]
                     [init-value "2"]))

(define mode (new radio-box%
                  [label "MODE"]
                  [parent setup-panel]
                  [choices (list "MAXIMIZATION" "MINIMIZATION")]))

(define continous-p (new check-box%
                         [label "CONTINOUS PARAMETERS"]
                         [parent setup-panel]))

(define threshold (new text-field%
                       [label "THRESHOLD"]
                       [parent setup-panel]
                       [init-value "500"]))

(define selection (new radio-box%
                       [label "SELECTION"]
                       [parent setup-panel]
                       [choices (list "TOURNAMENT" "ROULETTE")]))

(define tournament-k (new text-field%
                          [label "TOURNAMENT K"]
                          [parent setup-panel]
                          [init-value "4"]))

(define elitism (new text-field%
                     [label "ELITISM"]
                     [parent setup-panel]
                     [init-value "20"]))

(define resolution (new text-field%
                        [label "STRING SIZE FOR EACH VARIABLE IN TRADITIONAL MODE"]
                        [parent setup-panel]
                        [init-value "10"]))

(define range (new text-field%
                   [label "RANGE"]
                   [parent setup-panel]
                   [init-value "'((0 10) (0 10))"]))

(define crossover-mode (new radio-box%
                            [label "CROSSOVER"]
                            [parent setup-panel]
                            [choices (list "TRADITIONAL" "RADCLIFF" "WRIGHT")]))

(define pc (new text-field%
                [label "PC"]
                [parent setup-panel]
                [init-value "0.6"]))

(define mutation-mode (new radio-box%
                            [label "MUTATION"]
                            [parent setup-panel]
                            [choices (list "TRADITIONAL" "CONTINOUS")]))

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
                                [init-value "500"]))

(define returning-values (new text-field%
                              [label "(X, Y, ...)"]
                              [style (list 'multiple)]
                              [parent setup-panel]))
                              ;[enabled #f]))

(define returning-fx (new text-field%
                          [label "F(X, Y, ...)"]
                          [parent setup-panel]
                          [enabled #f]))

(define start (new button%
                   [label "START"]
                   [parent setup-panel]
                   [stretchable-width #t]
                   [stretchable-height #t]
                   [callback (λ (b e)
                               (let ((fn (get-value fn))
                                     (fnt (get-value fn-type))
                                     (md (list-ref (list 'max 'min) (send mode get-selection)))
                                     (cnt? (send continous-p get-value))
                                     (th (get-value threshold))
                                     (sl (list-ref (list 'tournament 'roulette) (send selection get-selection)))
                                     (tk (get-value tournament-k))
                                     (res (get-value resolution))
                                     (el (get-value elitism))
                                     (r (get-value range))
                                     (c-m (list-ref (list 'traditional 'radcliff 'wright) (send crossover-mode get-selection)))
                                     (pc (get-value pc))
                                     (m-m (list-ref (list 'traditional 'continous) (send mutation-mode get-selection)))
                                     (pm (get-value pm))
                                     (lg (get-value last-generation))
                                     (nind (get-value number-individuals)))
                                 (let-values ([(best best-for-generation med-for-generation) (time (genetic-algorithm #:function fn
                                                                                                                #:number-of-variables fnt
                                                                                                                #:mode md
                                                                                                                #:continous? cnt?
                                                                                                                #:threshold th
                                                                                                                #:selection sl
                                                                                                                #:tournament-k tk
                                                                                                                #:elitism el
                                                                                                                #:resolution res
                                                                                                                #:range r
                                                                                                                #:c-mode c-m
                                                                                                                #:pc pc
                                                                                                                #:m-mode m-m
                                                                                                                #:pm pm
                                                                                                                #:last-generation lg
                                                                                                                #:number-of-individuals nind))])
                                   (plot-performance best-for-generation med-for-generation (> fnt 2))
                                   (send returning-values set-value (slist->string (take best fnt)))
                                   (send returning-fx set-value (real->decimal-string (last best) 5))
                                   (case fnt
                                     ('1 (plot-2d-function-with-point fn best (caar r) (cadar r)))
                                     ('2 (plot-3d-function-with-point fn best (first r) (second r)))))))]))

(define (get-value widget)
  (eval (read (open-input-string (send widget get-value)))))

(send main-window show #t)
(plot-background-alpha 0)

(define (slist->string lst)
  (string-append "(" (string-join (map (λ (x) (real->decimal-string x 5)) lst) ", ") ")"))

(define (plot-3d-function-with-point fn point x-range y-range)
  (define dc (send function-canvas get-dc))
  (send dc clear)
  (plot3d/dc (list
              (surface3d fn (first x-range) (second x-range) (first y-range) (second y-range))
              (points3d (list point) #:color 'red))
             dc
             0 0
             (/ (send canvas-panel get-width) 2)
             (send canvas-panel get-height)
             #:title "OPTIMIZATION"
             #:x-label "X"
             #:y-label "Y"
             #:z-label "F(X, Y)"))

(define (plot-2d-function-with-point fn point x-min x-max)
  (define dc (send function-canvas get-dc))
  (send dc clear)
  (plot/dc (list
            (function fn x-min x-max)
            (points (list point) #:color 'red))
           dc
           0 0
           (/ (send canvas-panel get-width) 2)
           (send canvas-panel get-height)
           #:title "OPTIMIZATION"
           #:x-label "X"
           #:y-label "F(X)"))

(define (plot-performance y-by-gen med-by-gen fullscreen?)
  (define dc (send performance-canvas get-dc))
  (if fullscreen? (begin
                    (send dc clear)
                    (send (send function-canvas get-dc) clear))
      (send dc clear))
  (plot/dc (list (lines y-by-gen #:color 'red #:label "Best")
                 (lines med-by-gen #:color 'blue #:label "Average"))
           dc
           0 0
           (/ (send canvas-panel get-width) 2)
           (send canvas-panel get-height)
           #:title "PERFORMANCE"
           #:x-label "GENERATION"
           #:y-label "Y"))