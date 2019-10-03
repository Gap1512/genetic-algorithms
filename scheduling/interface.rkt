#lang racket

(require racket/gui plot "genetic-algorithm.rkt")

(define WIDTH 1000)
(define HEIGHT 230)

(define main-window (new frame% [label "GENETIC ALGORITHM"]
                         [style (list 'no-resize-border)]
                         [width WIDTH]))

(define main-panel (new horizontal-panel% [parent main-window]))

(define setup-panel (new vertical-panel%
                         [parent main-panel]
                         [alignment '(left top)]
                         [stretchable-width #f]))

(define result-panel (new vertical-panel% [parent main-panel]))

(define graphs-panel (new horizontal-panel%
                          [parent result-panel]
                          [min-height HEIGHT]
                          [stretchable-height #f]))

(define scheduling-grid (new list-box%
                             (label "SCHEDULING")
                             (parent graphs-panel)
                             (style (list 'multiple
                                          'column-headers
                                          'vertical-label))
                             (columns (list "FIRST INTERVAL" "SECOND INTERVAL" "THIRD INTERVAL" "FOURTH INTERVAL"))
                             (choices '())
                             (enabled #f)))

(send/apply scheduling-grid set (list (list "." "." "." "." "." "." ".")
                                      (list "." "." "." "." "." "." ".")
                                      (list "." "." "." "." "." "." ".")
                                      (list "." "." "." "." "." "." ".")))

(define canvas-panel (new horizontal-panel%
                          [parent result-panel]
                          [alignment (list 'center 'center)]))

(define performance-canvas (new canvas%
                                [parent canvas-panel]
                                [min-height HEIGHT]
                                [stretchable-height #f]))

(define animation-panel (new horizontal-panel%
                          [parent graphs-panel]
                          [alignment (list 'center 'center)]))

(define lines-canvas (new canvas% [parent animation-panel]))

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
                     [label "TOTAL"]
                     [parent setup-panel]
                     [enabled #f]))

(define start (new button%
                   [label "START"]
                   [parent setup-panel]
                   [stretchable-width #t]
                   [stretchable-height #t]
                   [callback (λ (b e)
                               (let ((as (get-value animation))
                                     (al (get-value alpha))
                                     (sl (if (= 0 (send selection get-selection)) 'roulette 'tournament))
                                     (tk (get-value tournament-k))
                                     (el (get-value elitism))
                                     (pc (get-value pc))
                                     (pm (get-value pm))
                                     (lg (get-value last-generation))
                                     (nind (get-value number-individuals)))
                                 (let-values ([(best production liquid best-for-generation med-for-generation) (genetic-algorithm #:selection sl
                                                                                                                       #:tournament-k tk
                                                                                                                       #:elitism el
                                                                                                                       #:pc pc
                                                                                                                       #:pm pm
                                                                                                                       #:last-generation lg
                                                                                                                       #:number-of-individuals nind)])
                                   (send pt set-value (real->decimal-string (fifth best) 5))
                                   (plot-grid best)
                                   (plot-performance best-for-generation med-for-generation)
                                   (plot-lines production liquid as al))))]))

(define (get-value widget)
  (eval (read (open-input-string (send widget get-value)))))

(send main-window show #t)

(define (plot-performance y-by-gen med-by-gen)
  (plot/dc (list (lines y-by-gen #:color 'red #:label "Best")
                 (lines med-by-gen #:color 'blue #:label "Average"))
           (send performance-canvas get-dc)
           0 0
           (send canvas-panel get-width)
           (send canvas-panel get-height)
           #:title "PERFORMANCE"
           #:x-label "GENERATION"
           #:y-label "FITNESS"))

(define (plot-lines production liquid as al)
  (let ((aux (λ (period-production period-liquid)
               (plot/dc (cons (map (λ (x) (lines x #:color 'red #:alpha al)) period-production)
                              (map (λ (x) (lines x #:color 'blue #:alpha al)) period-liquid))
                        (send lines-canvas get-dc)
                        0 0
                        (send animation-panel get-width)
                        (send animation-panel get-height)
                        #:title "POWER BY GENERATION"
                        #:x-label "PERIOD"
                        #:y-label "PRODUCTION"
                        #:y-min 0
                        #:y-max 150)
               (sleep/yield as))))
  (map aux
       (map (λ (x) (map (λ (y) (map list (range (length y)) y)) x)) production)
       (map (λ (x) (map (λ (y) (map list (range (length y)) y)) x)) liquid))))

(define (plot-grid best)
  (send/apply scheduling-grid set (map (λ (x y z) (append (map char->printable (string->list x)) (list y z))) (reverse (subseq best 4))
                                       (map real->decimal-string (sixth best))
                                       (map real->decimal-string (seventh best)))))

(define (char->printable char)
  (if (char=? #\1 char) "Manutenção" "Produção"))