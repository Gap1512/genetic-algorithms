#lang racket

(require "shared.rkt")

(provide genetic-algorithm)

(define %RANDOM (λ () (/ (random 1000000) 1000000)))

(define (genetic-algorithm #:function (fn GA-FUNCTION-3D)
                           #:3d (3d #t)
                           #:threshold (threshold 500)
                           #:selection (selection 'tournament)
                           #:tournament-k (tournament-k 4)
                           #:resolution (resolution 10)
                           #:elitism (elitism 2)
                           #:x-min (x-min -3.1)
                           #:x-max (x-max 12.1)
                           #:y-min (y-min 4.1)
                           #:y-max (y-max 5.8)
                           #:pc (pc 0.6)
                           #:pm (pm 0.01)
                           #:last-generation (last-generation 6)
                           #:number-of-individuals (number-of-individuals 4))
  (let* ((max-value (expt 2 resolution))
         (x-range (- x-max x-min))
         (y-range (- y-max y-min))
         (x-precision (/ max-value x-range))
         (y-precision (/ max-value y-range))
         (initial-population
          (generate-initial-population number-of-individuals x-min y-min max-value x-precision y-precision)))
    (update-population initial-population fn 3d threshold
                       selection tournament-k elitism
                       resolution x-precision y-precision pc pm last-generation
                       x-min y-min)))

(define (generate-initial-population number-of-individuals x-min y-min max-value x-precision y-precision)
  (let ((initial-population '()))
    (for ([i number-of-individuals])
      (set! initial-population (cons (list (+ x-min (/ (random max-value) x-precision))
                                           (+ y-min (/ (random max-value) y-precision)))
                                     initial-population)))
    initial-population))

(define (update-population
         initial-population function 3d threshold selection tournament-k
         elitism resolution x-precision y-precision pc pm last-generation x-min y-min)
  (let ((populations '())
        (actual-population initial-population))
    (for ([actual-generation last-generation])
      (let* ((formated-population (add-fitness actual-population function 3d threshold))
             (new-generation (compose-new-generation formated-population
                                                     elitism selection tournament-k
                                                     resolution x-precision y-precision
                                                     pc pm x-min y-min 3d)))
        (set! actual-population new-generation)
        (set! populations (cons formated-population populations))))
    (let* ((result (map (λ (population)
                          (map (λ (coord)
                                 (if 3d
                                     (list (first coord)
                                           (second coord)
                                           (- (third coord) threshold))
                                     (list (first coord)
                                           (- (third coord) threshold))))
                               population))
                        (reverse populations)))
           (best (best result 3d))
           (best-for-generation (best-for-generation result 3d))
           (med-for-generation (med-for-generation result 3d)))
      (values result best best-for-generation med-for-generation))))

(define (best-for-generation populations 3d)
  (let ((result '()))
    (for ([i (length populations)]
          [population populations])
      (set! result (cons (if 3d
                             (list i (third (first (sort population > #:key third))))
                             (list i (second (first (sort population > #:key second)))))
                         result)))
    result))

(define (med-for-generation populations 3d)
  (let* ((pos (if 3d third second))
         (m-population (map (λ (x) (med x pos)) populations))
         (result '()))
    (for ([i (length m-population)]
          [population m-population])
      (set! result (cons (list i population) result)))
    result))

(define (med population pos)
  (/ (apply + (map (λ (x) (pos x)) population)) (length population)))

(define (best populations 3d)
    (if 3d
        (first (sort (first (reverse populations)) > #:key third))
        (first (sort (first (reverse populations)) > #:key second))))

(define (add-fitness population function 3d threshold)
  (map (λ (coord)
             (let ((x (first coord))
                   (y (second coord)))
               (list x y (+ threshold (if 3d (function x y) (function x))))))
       population))

(define (compose-new-generation f-population elitism selection tournament-k resolution x-precision y-precision
                                pc pm x-min y-min 3d)
  (let ((new-generation (select-best elitism f-population))
        (population (case selection
                      ('roulette (add-cumulative-fitness f-population))
                      ('tournament f-population))))
    (for ([i (/ (- (length f-population) elitism) 2)])
      (let*-values ([(p1 p2) (select-parents population selection tournament-k)]
                    [(b1 b2) (binary-parents (list (- (first p1) x-min)
                                                   (- (second p1) y-min))
                                             (list (- (first p2) x-min)
                                                   (- (second p2) y-min)) resolution x-precision y-precision)]
                    [(f1 f2) (crossover b1 b2 pc)]
                    [(m1 m2) (mutation f1 f2 pm)]
                    [(ind1 ind2) (decimal-sons m1 m2 x-precision y-precision)])
        (set! new-generation (cons (list (+ (first ind1) x-min)
                                         (+ (second ind1) y-min))
                                   (cons (list (+ (first ind2) x-min)
                                               (+ (second ind2) y-min)) new-generation)))))
    new-generation))

(define (select-best k f-population)
  (subseq (sort f-population > #:key third) k))

(define (subseq list end)
  (subseq-aux '() list end))

(define (subseq-aux result lst end)
  (cond
    ((zero? end) result)
    (else (subseq-aux (cons (list (caar lst) (cadar lst)) result) (cdr lst) (sub1 end)))))

(define (add-cumulative-fitness f-population)
  (let ((sum (apply + (map third f-population)))
        (acc 0))
    (map (λ (ind)
           (let ((aux acc))
             (set! acc (+ acc (/ (third ind) sum)))
             (list (first ind)
                   (second ind)
                   aux
                   acc)))
         f-population)))

(define (select-parents population selection tournament-k)
  (case selection
    ('roulette (let* ((rnd1 (%RANDOM))
                      (rnd2 (%RANDOM))
                      (p1 (findf (λ (x) (search-proc x rnd1)) population))
                      (p2 (findf (λ (x) (search-proc x rnd2)) population)))
                 (values (list (first p1)
                               (second p1))
                         (list (first p2)
                               (second p2)))))
    ('tournament
     (let* ((r1 (first (sort (select-random tournament-k population) > #:key third)))
            (r2 (first (sort (select-random tournament-k population) > #:key third)))
            (p1 (list (first r1) (second r1)))
            (p2 (list (first r2) (second r2))))
       (values p1 p2)))))

(define (select-random k population)
  (select-random-aux '() k population (length population)))

(define (select-random-aux result k population lt)
  (cond
    ((zero? k) result)
    (else (select-random-aux (cons (list-ref population (random lt)) result)
                             (sub1 k)
                             population lt))))

(define (search-proc x rnd)
  (and (< (third x) rnd) (> (fourth x) rnd)))

(define (binary-parents p1 p2 resolution x-precision y-precision)
  (values (decimal->binary (ceiling (* (first p1) x-precision))
                           (ceiling (* (second p1) y-precision))
                           resolution)
          (decimal->binary (ceiling (* (first p2) x-precision))
                           (ceiling (* (second p2) y-precision))
                           resolution)))

(define (decimal->binary x y resolution)
        (list (decimal->binary-aux x "" resolution resolution)
              (decimal->binary-aux y "" resolution resolution)))
  
(define (decimal->binary-aux decimal result i resolution)
  (cond
    ((zero? i) result)
    ((zero? decimal) (decimal->binary-aux decimal (string-append "0" result)
                                          (sub1 i)
                                          resolution))
    (else (decimal->binary-aux (quotient decimal 2)
                               (string-append (number->string (inexact->exact (remainder decimal 2))) result)
                               (sub1 i)
                               resolution))))

(define (crossover p1 p2 pc)
  (if (<= (%RANDOM) pc)
      (let* ((resolution (string-length (first p1)))
             (index1 (random resolution))
             (index2 (random resolution))
             (fp1 (first p1))
             (sp1 (second p1))
             (fp2 (first p2))
             (sp2 (second p2))
             (c1 (list (string-append (substring fp1 0 index1)
                                      (substring fp2 index1))
                       (string-append (substring sp1 0 index2)
                                      (substring sp2 index2))))
             (c2 (list (string-append (substring fp2 0 index1)
                                      (substring fp1 index1))
                       (string-append (substring sp2 0 index2)
                                      (substring sp1 index2)))))
        (values c1 c2))
      (values p1 p2)))

(define (mutation i1 i2 pm)
  (values (list (single-mutation (first i1) pm) (single-mutation (second i1) pm))
          (list (single-mutation (first i2) pm) (single-mutation (second i2) pm))))

(define (single-mutation individual pm)
  (list->string
   (map (λ (y)
          (if (<= (%RANDOM) pm) (if (char=? y #\0) #\1 #\0) y))
        (string->list individual))))

(define (decimal-sons m1 m2 x-precision y-precision)
  (values (list (value-from-string (first m1) x-precision)
                (value-from-string (second m1) y-precision))
          (list (value-from-string (first m2) x-precision)
                (value-from-string (second m2) y-precision))))
  
(define (value-from-string string precision)
  (/ (string->number string 2) precision))