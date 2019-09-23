#lang racket

(require "shared.rkt")

(provide genetic-algorithm)

(define %RANDOM (λ () (/ (random 1000000) 1000000)))

(define (genetic-algorithm #:function (fn GA-FUNCTION)
                           #:threshold (threshold 4)
                           #:selection (selection 'roulette)
                           #:tournament-k (tournament-k 4)
                           #:resolution (resolution 10)
                           #:elitism (elitism 2)
                           #:range-min (range-min 0)
                           #:range-max (range-max 3)
                           #:pc (pc 0.6)
                           #:pm (pm 0.01)
                           #:last-generation (last-generation 70)
                           #:number-of-individuals (number-of-individuals 50))
  (let* ((max-value (expt 2 resolution))
         (total-range (- range-max range-min))
         (precision (/ max-value total-range))
         (initial-population (generate-initial-population number-of-individuals range-min max-value precision)))
    (update-population initial-population fn threshold
                       selection tournament-k elitism resolution precision pc pm last-generation)))

(define (generate-initial-population number-of-individuals range-min max-value precision)
  (let ((initial-population '()))
    (for ([i number-of-individuals])
      (set! initial-population (cons (+ range-min (/ (random max-value) precision)) initial-population)))
    initial-population))

(define (update-population
         initial-population function threshold selection tournament-k
         elitism resolution precision pc pm last-generation)
  (let ((populations '())
        (actual-population initial-population))
    (for ([actual-generation last-generation])
      (let* ((formated-population (add-fitness actual-population function threshold))
             (new-generation (compose-new-generation formated-population
                                                     elitism selection tournament-k
                                                     resolution precision pc pm)))
        (set! actual-population new-generation)
        (set! populations (cons formated-population populations))))
    (let* ((result (map (λ (population)
                          (map (λ (coord)
                                 (list (first coord)
                                       (- (second coord) threshold)))
                               population))
                        (reverse populations)))
           (best (best result))
           (best-for-generation (best-for-generation result))
           (med-for-generation (med-for-generation result)))
      (values result best best-for-generation med-for-generation))))

(define (best-for-generation populations)
  (let ((result '()))
    (for ([i (length populations)]
          [population populations])
      (set! result (cons (list i (second (first (sort population > #:key second)))) result)))
    result))

(define (med-for-generation populations)
  (let ((m-population (map (λ (x) (med x)) populations))
        (result '()))
    (for ([i (length m-population)]
          [population m-population])
      (set! result (cons (list i population) result)))
    result))

(define (med population)
  (/ (apply + (map second population)) (length population)))

(define (best populations)
  (first (sort (first (reverse populations)) > #:key second)))

(define (add-fitness population function threshold)
  (map (λ (x) (list x (+ threshold (function x))))
       population))

(define (compose-new-generation f-population elitism selection tournament-k resolution precision pc pm)
  (let ((new-generation (select-best elitism f-population))
        (population (case selection
                      ('roulette (add-cumulative-fitness f-population))
                      ('tournament f-population))))
    (for ([i (/ (- (length f-population) elitism) 2)])
      (let*-values ([(p1 p2) (select-parents population selection tournament-k)]
                    [(b1 b2) (binary-parents p1 p2 resolution precision)]
                    [(f1 f2) (crossover b1 b2 pc)]
                    [(m1 m2) (mutation f1 f2 pm)]
                    [(ind1 ind2) (decimal-sons m1 m2 precision)])
        (set! new-generation (cons ind1 (cons ind2 new-generation)))))
    new-generation))

(define (select-best k f-population)
  (subseq (sort f-population > #:key second) k))

(define (subseq list end)
  (subseq-aux '() list end))

(define (subseq-aux result list end)
  (cond
    ((zero? end) result)
    (else (subseq-aux (cons (caar list) result) (cdr list) (sub1 end)))))

(define (add-cumulative-fitness f-population)
  (let ((sum (apply + (map second f-population)))
        (acc 0))
    (map (λ (ind)
           (let ((aux acc))
             (set! acc (+ acc (/ (second ind) sum)))
             (list (first ind)
                   aux
                   acc)))
         f-population)))

(define (select-parents population selection tournament-k)
  (case selection
    ('roulette (let* ((rnd1 (%RANDOM))
                      (rnd2 (%RANDOM))
                      (p1 (findf (λ (x) (search-proc x rnd1)) population))
                      (p2 (findf (λ (x) (search-proc x rnd2)) population)))
                 (values (first p1) (first p2))))
    ('tournament
     (let ((p1 (car (sort (select-random tournament-k population) > #:key second)))
           (p2 (car (sort (select-random tournament-k population) > #:key second))))
       (values (first p1) (first p2))))))

(define (select-random k population)
  (select-random-aux '() k population (length population)))

(define (select-random-aux result k population lt)
  (cond
    ((zero? k) result)
    (else (select-random-aux (cons (list-ref population (random lt)) result)
                             (sub1 k)
                             population lt))))

(define (search-proc x rnd)
  (and (< (second x) rnd) (> (third x) rnd)))

(define (binary-parents p1 p2 resolution precision)
  (values (decimal->binary (ceiling (* p1 precision)) resolution)
          (decimal->binary (ceiling (* p2 precision)) resolution)))

(define (decimal->binary decimal resolution)
  (decimal->binary-aux decimal "" resolution resolution))

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
      (let* ((resolution (string-length p1))
             (index (random resolution))
             (c1 (string-append (substring p1 0 index)
                                (substring p2 index resolution)))
             (c2 (string-append (substring p2 0 index)
                                (substring p1 index resolution))))
        (values c1 c2))
      (values p1 p2)))

(define (mutation i1 i2 pm)
  (values (single-mutation i1 pm)
          (single-mutation i2 pm)))

(define (single-mutation individual pm)
  (list->string
   (map (λ (y)
          (if (<= (%RANDOM) pm) (if (char=? y #\0) #\1 #\0) y))
        (string->list individual))))

(define (decimal-sons m1 m2 precision)
  (values (value-from-string m1 precision)
          (value-from-string m2 precision)))
  
(define (value-from-string string precision)
  (/ (string->number string 2) precision))