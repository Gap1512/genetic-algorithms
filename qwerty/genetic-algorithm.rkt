#lang racket

(require "utilities.rkt" "source-analysis.rkt" racket/performance-hint)

(provide genetic-algorithm)


(define (genetic-algorithm source (gauge #f) (weight-coefficients '(0.45 1.0 0.8 0.7 0.6)) #:tournament-k tk #:elitism el #:pc pc #:pm pm #:last-generation lg #:number-of-individuals ni)
  (update-population (generate-initial-population ni) (relative-frequency (first source)) (relative-frequency (second source)) weight-coefficients gauge tk el pc pm lg))

;(generate-initial-population 5)
(define (generate-initial-population ni)
  (letrec ((rec (λ (n result)
                  (cond
                    ((zero? n) result)
                    (else (rec (sub1 n) (cons (new-individual) result)))))))
    (rec ni '())))

;(new-individual)
(begin-encourage-inline (define (new-individual)
                          (shuffle ALPHABET)))

(define (update-population initial-population relative-frequency-of-single-letter relative-frequency-of-letter-pair weight-coefficients gauge tournament-k elitism pc pm last-generation)
  (let ((populations '())
        (actual-population initial-population))
    (letrec ((rec (λ (actual-generation)
                    (let ((formated-population (add-fitness actual-population relative-frequency-of-single-letter relative-frequency-of-letter-pair weight-coefficients)))
                      (cond
                        ((= actual-generation last-generation)
                         (let* ((result (reverse populations))
                                (best (best result)))
                           (values best (best-for-generation result) (med-for-generation result) (compare-keyboards QWERTY best relative-frequency-of-single-letter relative-frequency-of-letter-pair weight-coefficients))))
                        (else
                         (begin
                           (set! actual-population (compose-new-generation formated-population tournament-k elitism pc pm))
                           (set! populations (cons formated-population populations))
                           (when gauge (send gauge set-value (add1 (send gauge get-value))))
                           (rec (add1 actual-generation)))))))))
      (rec 0))))

(define (best populations)
  (list-tail (first (sort (last populations) < #:key first)) 1))

(define (best-for-generation populations)
  (let ((result '()))
    (for ([i (length populations)]
          [population populations])
      (set! result (cons (list i (first (first (sort population < #:key first)))) result)))
    (reverse result)))

(define (med-for-generation populations)
  (let* ((m-population (map med populations))
         (result '()))
    (for ([i (length m-population)]
          [population m-population])
      (set! result (cons (list i population) result)))
    (reverse result)))

(begin-encourage-inline (define (med population)
                          (/ (apply + (map first population)) (length population))))

(define (compare-keyboards kbd1 kbd2 relative-frequency-of-single-letter relative-frequency-of-letter-pair weight-coefficients)
  (let* ((ikbd1 (indicator-scores kbd1 relative-frequency-of-single-letter relative-frequency-of-letter-pair))
         (ikbd2 (indicator-scores kbd2 relative-frequency-of-single-letter relative-frequency-of-letter-pair))
         (ikbd1-t (append ikbd1 (list (apply + (map * weight-coefficients ikbd1)))))
         (ikbd2-t (append ikbd2 (list (apply + (map * weight-coefficients ikbd2))))))
    (list (map (λ (a) (real->decimal-string a 4)) ikbd1-t)
          (map (λ (a) (real->decimal-string a 4)) ikbd2-t)
          (map (λ (a) (real->decimal-string a 4)) (relative-indicator-scores ikbd1-t ikbd2-t)))))

(define (relative-indicator-scores ikbd1 ikbd2)
  (map (λ (a b) (* (/ (- a b) a) 100)) ikbd1 ikbd2))

;(add-fitness (generate-initial-population 5)
;             (relative-frequency (multiple-files-frequencies '("C:\\home\\racket\\local-projects\\genetic-algorithm\\qwerty\\source\\Auto Da Barca Do Inferno.txt")))
;             (relative-frequency (multiple-files-frequencies '("C:\\home\\racket\\local-projects\\genetic-algorithm\\qwerty\\source\\Auto Da Barca Do Inferno.txt") #f 2))
;             '(0.45 1.0 0.8 0.7 0.6))
(define (add-fitness population relative-frequency-of-single-letter relative-frequency-of-letter-pair weight-coefficients)
  (map (λ (x) (cons (keyboard-fitness x relative-frequency-of-single-letter relative-frequency-of-letter-pair weight-coefficients) x)) population))

;(keyboard-fitness (new-individual)
;                  (relative-frequency (multiple-files-frequencies '("C:\\home\\racket\\local-projects\\genetic-algorithm\\qwerty\\source\\Auto Da Barca Do Inferno.txt")))
;                  (relative-frequency (multiple-files-frequencies '("C:\\home\\racket\\local-projects\\genetic-algorithm\\qwerty\\source\\Auto Da Barca Do Inferno.txt") #f 2))
;                  '(0.45 1.0 0.8 0.7 0.6))
(define (keyboard-fitness keyboard relative-frequency-of-single-letter relative-frequency-of-letter-pair weight-coefficients)
  (apply + (map * weight-coefficients (indicator-scores keyboard relative-frequency-of-single-letter relative-frequency-of-letter-pair))))

;(indicator-scores QWERTY
;                  (relative-frequency (multiple-files-frequencies '("C:\\home\\racket\\local-projects\\genetic-algorithm\\qwerty\\source\\Auto Da Barca Do Inferno.txt")))
;                  (relative-frequency (multiple-files-frequencies '("C:\\home\\racket\\local-projects\\genetic-algorithm\\qwerty\\source\\Auto Da Barca Do Inferno.txt") #f 2)))
(define (indicator-scores keyboard relative-frequency-of-single-letter relative-frequency-of-letter-pair)
  (list (tapping-workload-distribution keyboard relative-frequency-of-single-letter)
        (hand-alternation keyboard relative-frequency-of-letter-pair)
        (finger-alternation keyboard relative-frequency-of-letter-pair)
        (avoiding-big-steps keyboard relative-frequency-of-letter-pair)
        (hit-direction keyboard relative-frequency-of-letter-pair)))

;(tapping-workload-distribution (new-individual) (relative-frequency (multiple-files-frequencies '("C:\\home\\racket\\local-projects\\genetic-algorithm\\qwerty\\source\\Auto Da Barca Do Inferno.txt"))))
(define (tapping-workload-distribution keyboard relative-frequency-of-single-letter)
  (letrec ((rec (λ (lst ideal acc)
                  (cond
                    ((empty? lst) acc)
                    (else (rec (cdr lst) (cdr ideal) (+ acc (sqr (- (second (or (assoc (list (car lst)) relative-frequency-of-single-letter) '(0 0))) (car ideal))))))))))
    (rec keyboard IDEAL-TAPPING-WORKLOAD 0)))

;(hand-alternation (new-individual) (relative-frequency (multiple-files-frequencies '("C:\\home\\racket\\local-projects\\genetic-algorithm\\qwerty\\source\\Auto Da Barca Do Inferno.txt") #f 2)))
(define (hand-alternation keyboard relative-frequency-of-letter-pair)
  (letrec ((rec (λ (pairs acc)
                  (cond
                    ((empty? pairs) acc)
                    (else (let* ((pair (car pairs))
                                 (i (location (caar pair) keyboard))
                                 (j (location (cadar pair) keyboard)))
                            (rec (cdr pairs) (+ acc (* (second pair) (same i j first))))))))))
    (rec relative-frequency-of-letter-pair 0)))

;(same (location #\A QWERTY) (location #\B QWERTY) first)
(begin-encourage-inline  (define (same i j pos)
                           (if (equal? (pos i) (pos j)) 1 0)))

;(location #\C QWERTY)
(define (location i keyboard)
  (letrec ((rec (λ (kbd ref)
                  (cond
                    ((empty? kbd) #f)
                    ((char=? i (car kbd)) (car ref))
                    (else (rec (cdr kbd) (cdr ref)))))))
    (rec keyboard KEYBOARD-REPRESENTATION)))

;(finger-alternation (new-individual) (relative-frequency (multiple-files-frequencies '("C:\\home\\racket\\local-projects\\genetic-algorithm\\qwerty\\source\\Auto Da Barca Do Inferno.txt") #f 2)))
(define (finger-alternation keyboard relative-frequency-of-letter-pair)
  (letrec ((rec (λ (pairs acc)
                  (cond
                    ((empty? pairs) acc)
                    (else (let* ((pair (car pairs))
                                 (i (location (caar pair) keyboard))
                                 (j (location (cadar pair) keyboard)))
                            (rec (cdr pairs) (+ acc (* (second pair) (same i j first) (same i j fourth) (distance i j))))))))))
    (rec relative-frequency-of-letter-pair 0)))

;(distance (location #\A QWERTY) (location #\B QWERTY))
(begin-encourage-inline  (define (distance i j)
                           (+ (abs (- (second i) (second j)))
                              (abs (- (third i) (third j))))))

;(avoiding-big-steps (new-individual) (relative-frequency (multiple-files-frequencies '("C:\\home\\racket\\local-projects\\genetic-algorithm\\qwerty\\source\\Auto Da Barca Do Inferno.txt") #f 2)))
(define (avoiding-big-steps keyboard relative-frequency-of-letter-pair)
  (letrec ((rec (λ (pairs acc)
                  (cond
                    ((empty? pairs) acc)
                    (else (let* ((pair (car pairs))
                                 (i (location (caar pair) keyboard))
                                 (j (location (cadar pair) keyboard)))
                            (rec (cdr pairs) (+ acc (* (second pair) (same i j first) (second (assoc (list (fourth i) (fourth j)) BIG-STEPS)))))))))))
    (rec relative-frequency-of-letter-pair 0)))

;(hit-direction (new-individual) (relative-frequency (multiple-files-frequencies '("C:\\home\\racket\\local-projects\\genetic-algorithm\\qwerty\\source\\Auto Da Barca Do Inferno.txt") #f 2)))
(define (hit-direction keyboard relative-frequency-of-letter-pair)
  (letrec ((rec (λ (pairs acc)
                  (cond
                    ((empty? pairs) acc)
                    (else (let* ((pair (car pairs))
                                 (i (location (caar pair) keyboard))
                                 (j (location (cadar pair) keyboard)))
                            (rec (cdr pairs) (+ acc (* (second pair) (same i j first) (if (> (fourth i) (fourth j)) 1 0))))))))))
    (rec relative-frequency-of-letter-pair 0)))

(define (compose-new-generation f-population tournament-k elitism pc pm)
  (let ((new-generation (select-best elitism f-population)))
    (for ([i (/ (- (length f-population) elitism) 2)])
      (let*-values ([(p1 p2) (select-parents f-population tournament-k)]
                    [(f1 f2) (crossover p1 p2 pc)]
                    [(m1 m2) (mutation f1 f2 pm)])
        (set! new-generation (cons m1 (cons m2 new-generation)))))
    new-generation))

;(select-best 2 (add-fitness (generate-initial-population 5)
;                            (relative-frequency (multiple-files-frequencies '("C:\\home\\racket\\local-projects\\genetic-algorithm\\qwerty\\source\\Auto Da Barca Do Inferno.txt")))
;                            (relative-frequency (multiple-files-frequencies '("C:\\home\\racket\\local-projects\\genetic-algorithm\\qwerty\\source\\Auto Da Barca Do Inferno.txt") #f 2))
;                            '(0.45 1.0 0.8 0.7 0.6)))
(define (select-best k f-population)
  (map (λ (x) (list-tail x 1))
       (take (sort f-population < #:key first) k)))

;(select-parents (add-fitness (generate-initial-population 5)
;                             (relative-frequency (multiple-files-frequencies '("C:\\home\\racket\\local-projects\\genetic-algorithm\\qwerty\\source\\Auto Da Barca Do Inferno.txt")))
;                             (relative-frequency (multiple-files-frequencies '("C:\\home\\racket\\local-projects\\genetic-algorithm\\qwerty\\source\\Auto Da Barca Do Inferno.txt") #f 2))
;                             '(0.45 1.0 0.8 0.7 0.6)) 3)
(begin-encourage-inline (define (select-parents population tournament-k)
                          (values (list-tail (first (sort (select-random tournament-k population) < #:key first)) 1) (list-tail (first (sort (select-random tournament-k population) < #:key first)) 1))))

(define (select-random k population)
  (letrec ((select-random-aux (λ (result k population lt)
                                (cond
                                  ((zero? k) result)
                                  (else (select-random-aux (cons (list-ref population (random lt)) result)
                                                           (sub1 k)
                                                           population lt))))))
    (select-random-aux '() k population (length population))))

;(crossover '(#\M #\P #\O #\F #\W #\R #\Q #\A #\K #\H #\G #\I #\L #\D #\U #\J #\N #\V #\T #\Z #\C #\X #\S #\B #\Y #\E #\Ç)
;           '(#\B #\Ç #\I #\E #\P #\M #\S #\Z #\D #\U #\F #\Y #\A #\X #\R #\N #\C #\Q #\J #\T #\W #\L #\K #\O #\V #\G #\H)
;           0.6)
(define (crossover p1 p2 pc)
  (if (<= (%RANDOM) pc)
      (let* ((index1 (random 27))
             (index2 (random 27))
             (p? (< index1 index2)))
        (pmx p1 p2 (if p? index1 index2) (if p? index2 index1)))
      (values p1 p2)))

(define %RANDOM (λ () (/ (random 1000000) 1000000)))

(define (pmx ind1 ind2 begin end)
  (let ((table1 (list-tail (take ind1 end) begin))
        (table2 (list-tail (take ind2 end) begin)))
    (letrec ((rec (λ (ind1 ind2 x f1 f2)
                    (cond
                      ((empty? ind1) (values (reverse f1) (reverse f2)))
                      (else (rec (cdr ind1) (cdr ind2) (add1 x)
                              (cons (if (and (>= x begin) (< x end))
                                        (car ind2)
                                        (inner-rec (car ind1) table2 table1))
                                    f1)
                              (cons (if (and (>= x begin) (< x end))
                                        (car ind1)
                                        (inner-rec (car ind2) table1 table2))
                                    f2))))))
             (inner-rec (λ (a table-a table-b)
                          (let ((pos (index-of table-a a)))
                            (cond
                              ((not pos) a)
                              (else (inner-rec (list-ref table-b pos) table-a table-b)))))))
      (rec ind1 ind2 0 '() '()))))

;(mutation '(#\B #\Ç #\I #\E #\W #\R #\Q #\A #\K #\H #\G #\O #\L #\D #\U #\J #\N #\V #\T #\Z #\C #\X #\S #\M #\Y #\F #\P)
;          '(#\M #\P #\O #\F #\Ç #\B #\S #\Z #\D #\U #\E #\Y #\A #\X #\R #\N #\C #\Q #\J #\T #\W #\L #\K #\I #\V #\G #\H)
;          0.8)
(define (mutation i1 i2 pm)
  (values (single-mutation i1 pm) (single-mutation i2 pm)))

(define (single-mutation individual pm)
  (let ((result individual))
    (for ([í result]
          [j 27])
      (if (<= (%RANDOM) pm)
          (let ((pos (random j 27)))
            (set! result (swap-element result j pos)))
          '()))
    result))

(define (swap-element lst x y)
  (if (equal? x y) (reverse lst)
  (append
   (take lst x)
   (cons (list-ref lst y)
         (take (list-tail lst (add1 x)) (- y (add1 x))))
   (cons (list-ref lst x)
         (list-tail lst (add1 y))))))