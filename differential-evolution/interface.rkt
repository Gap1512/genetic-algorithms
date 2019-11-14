#lang racket

(require racket/gui plot "differential-evolution.rkt" "shared.rkt")

(define HEIGHT 500)
(define WIDTH 1400)

(define main-window (new frame% [label "DIFFERENTIAL EVOLUTION"]
                         ;[style (list 'no-resize-border)]
                         [width WIDTH]
                         [height HEIGHT]))

(define main-panel (new horizontal-panel% [parent main-window]))

(define setup-panel (new vertical-panel%
                         [parent main-panel]
                         [alignment '(left top)]
                         [style (list 'auto-vscroll)]
                         [stretchable-width #f]))

(define canvas-panel (new horizontal-panel% [parent main-panel]))
(define performance-canvas (new canvas% [parent canvas-panel]))
(define function-canvas (new canvas% [parent canvas-panel]))

(define fn (new combo-field%
                [label "FUNCTION"]
                [parent setup-panel]
                [choices (list "(λ (x) (+ (* x (sin (expt x 4))) (cos (* x x))))"
                               "(λ (x) (- (+ 480 (* -1 (abs (* x (sin (sqrt (abs x)))))))))"
                               "(λ (x y) (+ (sqr (sub1 x)) (sqr (sub1 y))))"
                               "(λ (x y) (+ (* x (sin (* 4 x))) (* 1.1 (sin (* 2 y)))))"
                               "(λ (x y) (+ 21.5 (* x (sin (* 4 pi x))) (* y (sin (* 20 pi y)))))")]
                [init-value "(rosenbrock 2 1 100)"]))

(define fn-type (new text-field%
                     [label "NUMBER OF VARIABLES"]
                     [parent setup-panel]
                     [init-value "2"]))

(define mode (new radio-box%
                  [label "MODE"]
                  [parent setup-panel]
                  [choices (list "MINIMIZATION" "MAXIMIZATION")]))

(define range (new text-field%
                   [label "RANGE"]
                   [parent setup-panel]
                   [init-value "(n-range '(-3 3) 2)"]))

(define pc (new text-field%
                [label "CR"]
                [parent setup-panel]
                [init-value "0.9"]))

(define pm (new text-field%
                [label "F"]
                [parent setup-panel]
                [init-value "0.8"]))

(define last-generation (new text-field%
                             [label "LAST GENERATION"]
                             [parent setup-panel]
                             [init-value "70"]))

(define number-individuals (new text-field%
                                [label "NUMBER OF INDIVIDUALS"]
                                [parent setup-panel]
                                [init-value "20"]))

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
                                     (md (list-ref (list 'min 'max) (send mode get-selection)))
                                     (r (get-value range))
                                     (pc (get-value pc))
                                     (pm (get-value pm))
                                     (lg (get-value last-generation))
                                     (nind (get-value number-individuals)))
                                 (let-values ([(best best-for-generation med-for-generation) (time (differential-evolution #:function fn
                                                                                                                           #:number-of-variables fnt
                                                                                                                           #:mode md
                                                                                                                           #:range r
                                                                                                                           #:pc pc
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
              (points3d (list point) #:color 'red)
              (surface3d fn (first x-range) (second x-range) (first y-range) (second y-range)))
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
            (points (list point) #:color 'red)
            (function fn x-min x-max))
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