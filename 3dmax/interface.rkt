#lang racket

(require racket/gui plot mrlib/gif "shared.rkt" "tournament-elitism.rkt")

(define SETUP-WIDTH 180)
(define PATH-GRAPH "./results/graph.gif")
(define PATH-PERFORMANCE "./results/performance.gif")
(define GAUGE-RANGE 10000)
(define GRAPH-WIDTH 400)
(define GRAPH-HEIGHT 400)

(define main-window (new frame% [label "GENETIC ALGORITHM"]
                         [style (list 'no-resize-border)]))

(define setup-panel (new vertical-panel%
                         [parent main-window]
                         [alignment '(left top)]
                         [min-width SETUP-WIDTH]
                         [stretchable-width #f]))

(define fn (new text-field%
                [label "FUNCTION"]
                [parent setup-panel]
                [init-value "(λ (x) (+ (* x (sin (expt x 4))) (cos (* x x))))"]))

(define threshold (new text-field%
                       [label "THRESHOLD"]
                       [parent setup-panel]
                       [init-value "500"]))

(define animation (new text-field%
                       [label "ANIMATION SPEED"]
                       [parent setup-panel]
                       [init-value "5"]))

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

(define range-min (new text-field%
                       [label "RANGE MIN"]
                       [parent setup-panel]
                       [init-value "0"]))

(define range-max (new text-field%
                       [label "RANGE MAX"]
                       [parent setup-panel]
                       [init-value "3"]))

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

(define fx-entry (new text-field%
                      [label "F(X)"]
                      [parent setup-panel]
                      [enabled #f]))

(define progress (new gauge%
                      [label "PROGRESS "]
                      [parent setup-panel]
                      [stretchable-width #t]
                      [stretchable-height #t]
                      [range GAUGE-RANGE]))

(define start (new button%
                   [label "START"]
                   [parent setup-panel]
                   [stretchable-width #t]
                   [stretchable-height #t]
                   [callback (λ (b e)
                               (let ((fn (get-value fn))
                                     (th (get-value threshold))
                                     (as (get-value animation))
                                     (sl (if (= 0 (send selection get-selection)) 'roulette 'tournament))
                                     (tk (get-value tournament-k))
                                     (res (get-value resolution))
                                     (el (get-value elitism))
                                     (x-min (get-value range-min))
                                     (x-max (get-value range-max))
                                     (pc (get-value pc))
                                     (pm (get-value pm))
                                     (lg (get-value last-generation))
                                     (nind (get-value number-individuals)))
                                 (let-values ([(points best best-for-generation med-for-generation) (genetic-algorithm #:function fn
                                                                                                    #:threshold th
                                                                                                    #:selection sl
                                                                                                    #:tournament-k tk
                                                                                                    #:resolution res
                                                                                                    #:elitism el
                                                                                                    #:range-min x-min
                                                                                                    #:range-max x-max
                                                                                                    #:pc pc
                                                                                                    #:pm pm
                                                                                                    #:last-generation lg
                                                                                                    #:number-of-individuals nind)])
                                   (plot-performance-bitmap best-for-generation med-for-generation)
                                   (plot-function-and-points-bitmap fn points x-min x-max as)
                                   (send x-entry set-value (real->decimal-string (first best) 5))
                                   (send fx-entry set-value (real->decimal-string (second best) 5)))))]))

(define (get-value widget)
  (eval (read (open-input-string (send widget get-value)))))

(define (plot-function-and-points-bitmap fn list-of-points x-min x-max animation-speed)
  (if (directory-exists? "./results")
      '()
      (make-directory "./results"))
  (if (file-exists? PATH-GRAPH)
      (delete-file PATH-GRAPH)
      '())
  (let ((gauge-increment (floor (/ GAUGE-RANGE (length list-of-points))))
        (gauge-value 0)
        (result '()))
    (for-each
     (λ (population)
       (set! result (cons
                     (plot-bitmap (list (points population
                                                #:x-min x-min
                                                #:x-max x-max
                                                #:color 'red
                                                #:alpha 0.5)
                                        (function fn x-min x-max 
                                                  #:color 'black 
                                                  #:alpha 1))
                                  #:title "MAXIMIZATION"
                                  #:x-label "X"
                                  #:y-label "F(X)"
                                  #:width GRAPH-WIDTH
                                  #:height GRAPH-HEIGHT)
                     result))
       (set! gauge-value (+ gauge-value gauge-increment))
       (send progress set-value gauge-value))
     list-of-points)
    (write-animated-gif (reverse result) animation-speed PATH-GRAPH)
    (send progress set-value GAUGE-RANGE)))

(define (plot-performance-bitmap y-by-gen med-by-gen)
  (if (directory-exists? "./results")
      '()
      (make-directory "./results"))
  (if (file-exists? PATH-PERFORMANCE)
      (delete-file PATH-PERFORMANCE)
      '())
  (write-gif (plot-bitmap (list (lines y-by-gen #:color 'red) (lines med-by-gen #:color 'blue))
                          #:title "MAXIMIZATION"
                          #:x-label "X"
                          #:y-label "F(X)"
                          #:width GRAPH-WIDTH
                          #:height GRAPH-HEIGHT) PATH-PERFORMANCE))

(send main-window show #t)