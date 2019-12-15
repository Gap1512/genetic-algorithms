#lang racket

(require racket/gui plot "source-analysis.rkt" "keyboard-render.rkt" "genetic-algorithm.rkt" "utilities.rkt")

(define (create-gui)
  (let* ((files '("C:\\home\\racket\\local-projects\\genetic-algorithm\\qwerty\\source\\Default.txt"))
         (source-material (list (multiple-files-frequencies files) (multiple-files-frequencies files #f 2)))
         (keyboard QWERTY)
         (evaluation-init '("I1: Tapping workload distribution"
                                           "I2: Hand alternation"
                                           "I3: Finger alternation"
                                           "I4: Avoidance of big steps"
                                           "I5: Hit direction"
                                           "I: Evaluation score"))
         (main-window (new frame%
                           [label "Keyboard Generator"]
                           [stretchable-width #t]
                           [stretchable-height #t]))
         (main-panel (new horizontal-panel%
                          [parent main-window]
                          [alignment '(left top)]
                          [stretchable-width #t]
                          [stretchable-height #t]))
         (setup-panel (new vertical-panel%
                           [parent main-panel]
                           [alignment '(center top)]
                           [stretchable-width #f]
                           [stretchable-height #t]))
         (result-panel (new vertical-panel%
                            [parent main-panel]
                            [alignment '(center center)]
                            [min-width 1000]))
         (keyboard-canvas (new canvas%
                               [parent result-panel]
                               [stretchable-height #t]
                               [paint-callback
                                (lambda (canvas dc)
                                  (send dc clear)
                                  (let* ((w (send result-panel get-width))
                                         (h (- (floor (/ (send result-panel get-height) 2)) 36)))
                                    (send dc draw-bitmap (image->bitmap (generate-keyboard keyboard (sub1 w) (sub1 h)) w h (make-bitmap w h)) 0 0)))]))
         (performance-panel (new horizontal-panel%
                                 [parent result-panel]))
         (performance-canvas (new canvas%
                                  [parent performance-panel]
                                  [stretchable-height #t]))
         (evaluation-list (new list-box%
                               [label #f]
                               [parent performance-panel]
                               [columns '("Indicators" "QWERTY" "Optimal" "Improvement")]
                               [choices evaluation-init]
                               [style (list 'single 'column-headers 'clickable-headers)]
                               [stretchable-height #t]))
         (source-group (new group-box-panel%
                            [parent setup-panel]
                            [label "Source Files"]
                            [alignment '(center top)]))
         (convert (new button% 
                       [label "Convert .pdf to .txt"]
                       [parent source-group]
                       [stretchable-width #t]
                       [callback (λ (b e)
                                   (let* ((files-list (get-file-list))
                                          (frame (new frame% [label "Progress"]))
                                          (gauge (new gauge% [label #f] [parent frame] [range (length (or files-list '()))])))
                                     (send frame show #t)
                                     (send gauge set-value 0)
                                     (multiple-files-pdf->txt (map path->string files-list) gauge)
                                     (send frame show #f)))]))
         (file-list (new list-box%
                         [label #f]
                         [parent source-group]
                         [choices '()]))
         (search (new button% 
                      [label "Search"]
                      [parent source-group]
                      [stretchable-width #t]
                      [callback (λ (b e) (begin
                                           (set! files (get-file-list))
                                           (send file-list set (map path->string files))))]))
         (char-list (new list-box%
                         [label #f]
                         [parent source-group]
                         [choices '()]
                         [style (list 'single 'column-headers 'clickable-headers)]
                         [columns '("Chars" "Freqs")]))
         (analyze (new button% 
                       [label "Analyze"]
                       [parent source-group]
                       [stretchable-width #t]
                       [callback (λ (b e)
                                   (let* ((frame (new frame% [label "Progress"]))
                                          (gauge (new gauge% [label #f] [parent frame] [range (* 3 (length (or files '())))])))
                                     (send frame show #t)
                                     (send gauge set-value 0)
                                     (set! source-material (list (multiple-files-frequencies files gauge 1)
                                                                 (multiple-files-frequencies files gauge 2)))
                                     (send/apply char-list set (format-source (sort (first source-material) < #:key second)))
                                     (send frame show #f)))]))
         (setup-group (new group-box-panel%
                           [parent setup-panel]
                           [label "Genetic Algorithm"]
                           [alignment '(center top)]
                           [stretchable-width #t]
                           [stretchable-height #f]))
         (tk (new text-field%
                  [label "Tournament"]
                  [parent setup-group]
                  [init-value "4"]
                  [style (list 'single 'vertical-label)]))
         (el (new text-field%
                  [label "Elitism"]
                  [parent setup-group]
                  [style (list 'single 'vertical-label)]
                  [init-value "20"]))
         (pc (new text-field%
                  [label "Crossover"]
                  [parent setup-group]
                  [style (list 'single 'vertical-label)]
                  [init-value "0.6"]))
         (pm (new text-field%
                  [label "Mutation"]
                  [parent setup-group]
                  [style (list 'single 'vertical-label)]
                  [init-value "0.01"]))
         (lg (new text-field%
                  [label "Last Generation"]
                  [parent setup-group]
                  [style (list 'single 'vertical-label)]
                  [init-value "70"]))
         (ni (new text-field%
                  [label "Number of Individuals"]
                  [parent setup-group]
                  [style (list 'single 'vertical-label)]
                  [init-value "500"]))
         (run (new button% 
                   [label "Run"]
                   [parent setup-group]
                   [stretchable-width #t]
                   [callback (λ (b e)
                               (let* ((tk (get-value tk))
                                      (el (get-value el))
                                      (pc (get-value pc))
                                      (pm (get-value pm))
                                      (lg (get-value lg))
                                      (ni (get-value ni))
                                      (frame (new frame% [label "Progress"]))
                                      (gauge (new gauge% [label #f] [parent frame] [range lg])))
                                 (send frame show #t)
                                 (send gauge set-value 0)
                                 (let-values ([(best best-for-generation med-for-generation scores) (time (genetic-algorithm source-material gauge
                                                                                                                      #:tournament-k tk
                                                                                                                      #:elitism el
                                                                                                                      #:pc pc
                                                                                                                      #:pm pm
                                                                                                                      #:last-generation lg
                                                                                                                      #:number-of-individuals ni))])
                                   (plot-performance performance-canvas best-for-generation med-for-generation)
                                   (set! keyboard best)
                                   (send keyboard-canvas refresh-now)
                                   (send/apply evaluation-list set (cons evaluation-init scores))
                                   (send frame show #f))))]))
         (export-group (new group-box-panel%
                            [parent setup-panel]
                            [label "Export Keyboard"]
                            [alignment '(center top)]))
         (export-width (new text-field%
                            [label "Width"]
                            [parent export-group]
                            [style (list 'single 'vertical-label)]
                            [init-value "1920"]))
         (export-height (new text-field%
                             [label "Height"]
                             [parent export-group]
                             [style (list 'single 'vertical-label)]
                             [init-value "686"]))
         (export-foreground (new text-field%
                                 [label "Foreground"]
                                 [parent export-group]
                                 [style (list 'single 'vertical-label)]
                                 [init-value "Black"]))
         (export-background (new text-field%
                                 [label "Background"]
                                 [parent export-group]
                                 [style (list 'single 'vertical-label)]
                                 [init-value "White"]))
         (export-button (new button% 
                             [label "Export"]
                             [parent export-group]
                             [stretchable-width #t]
                             [callback (λ (b e) (save-image (generate-keyboard keyboard
                                                                               (get-value export-width)
                                                                               (get-value export-height)
                                                                               #:foreground (get-string export-foreground)
                                                                               #:background (get-string export-background))
                                                            (path->string (path-replace-extension (get-file) #".png"))))])))
    (send main-window show #t)))

(define (get-forbiddens widget)
  (map format-forbidden (get-value widget)))

(define (get-value widget)
  (eval (read (open-input-string (send widget get-value)))))

(define (get-string widget)
  (string-downcase (send widget get-value)))

(define (format-forbidden char)
  (cond
    ((char? char) char)
    ((number? char) (integer->char char))))

(define (plot-performance performance-canvas y-by-gen med-by-gen)
  (define dc (send performance-canvas get-dc))
  (send dc clear)
  (plot/dc (list (lines y-by-gen #:color 'red #:label "Best")
                 (lines med-by-gen #:color 'blue #:label "Average"))
           dc
           0 0
           (send performance-canvas get-width)
           (send performance-canvas get-height)
           #:title "Performance"
           #:x-label "Generation"
           #:y-label "Score"))