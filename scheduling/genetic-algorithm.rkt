#lang racket

(provide genetic-algorithm subseq)

(define %RANDOM (λ () (/ (random 1000000) 1000000)))

(define (interval-number machine)
  (list-ref '(2 2 1 1 1 1 1) machine))

(define (capacity-number machine)
  (list-ref '(20 15 35 40 15 15 10) machine))

(define (power-per-period period)
  (list-ref '(80 90 65 70) period))

(define (genetic-algorithm #:threshold (threshold 500)
                           #:selection (selection 'tournament)
                           #:tournament-k (tournament-k 4)
                           #:resolution (resolution 10)
                           #:elitism (elitism 2)
                           #:pc (pc 0.6)
                           #:pm (pm 0.01)
                           #:last-generation (last-generation 6)
                           #:number-of-individuals (number-of-individuals 4))
    (update-population (generate-initial-population number-of-individuals)
                       threshold selection tournament-k elitism pc pm last-generation))

(define (generate-initial-population number-of-individuals)
  (let ((initial-population '()))
    (for ([i number-of-individuals])
      (set! initial-population (cons (generate-valid-individual) initial-population)))
    initial-population))

(define (generate-valid-individual)
  (letrec ((rec (λ (individual)
                  (if (is-valid? individual)
                      individual
                      (rec (list (generate-valid-period 0)
                                 (generate-valid-period 1)
                                 (generate-valid-period 2)
                                 (generate-valid-period 3)))))))
    (rec '())))

(define (generate-valid-period index)
  (letrec ((rec (λ (period)
                  (if (is-period-valid? period)
                      period
                      (let* ((p (decimal->binary (random 255) 7))
                             (pp (period-production p)))
                        (rec (list p pp (- pp (power-per-period index)))))))))
    (rec '())))

(define (is-period-valid? period)
  (and (not (empty? period))
       (maximum-demand-check? (third period))))

(define (maximum-demand-check? liquid-production)
  (> liquid-production 0))

(define (period-production period)
  (letrec ((rec (λ (lst acc n)
                  (cond
                    ((empty? lst) acc)
                    (else (rec (cdr lst) (if (char=? #\0 (car lst))
                                             (+ acc (capacity-number n))
                                             acc) (add1 n)))))))
    (rec (string->list period) 0 0)))

(define (is-valid? individual)
  (and (not (empty? individual))
       (for/and ([i 7])
         (interval-number-by-machine-check? i (list (string-ref (first (first individual)) i)
                                                    (string-ref (first (second individual)) i)
                                                    (string-ref (first (third individual)) i)
                                                    (string-ref (first (fourth individual)) i))))))

(define (interval-number-by-machine-check? machine lst)
  (let ((b-lst (map char->boolean lst))
        (i-n (interval-number machine)))
    (if (> (length-true b-lst) i-n) #f
        (xormap (λ (x) (andmap identity x))
                (group b-lst i-n)))))

(define (char->boolean char)
  (not (char=? #\0 char)))

(define (group lst n)
  (if (zero? n) (error "Length Zero")
      (letrec ((rec (λ (lst acc)
                      (cond
                        ((< (length lst) n) (reverse acc))
                        (else (rec (cdr lst) (cons (reverse (subseq lst n)) acc)))))))
        (rec lst '()))))

(define (xormap proc lst)
  (letrec ((rec (λ (lst acc)
                  (if (empty? lst) acc
                      (let ((elem (proc (car lst))))
                        (cond 
                          ((and elem acc) #f)
                          (elem (rec (cdr lst) #t))
                          (else (rec (cdr lst) acc))))))))
    (rec lst #f)))

(define (length-true blst)
  (letrec ((rec (λ (blst acc)
                  (cond
                    ((empty? blst) acc)
                    (else (rec (cdr blst) (if (car blst)
                                              (add1 acc)
                                              acc)))))))
    (rec blst 0)))

(define (update-population initial-population threshold selection tournament-k elitism pc pm last-generation)
  (let ((populations '())
        (actual-population initial-population))
    (for ([actual-generation last-generation])
      (let* ((formated-population (add-fitness actual-population threshold)))
        (set! actual-population (compose-new-generation formated-population elitism selection tournament-k pc pm))
        (set! populations (cons formated-population populations))))
    (let* ((result (map (λ (population)
                          (map (λ (ind)
                                 (list (first (second ind))
                                       (first (third ind))
                                       (first (fourth ind))
                                       (first (fifth ind))
                                       (- threshold (first ind))
                                       (map third (list-tail ind 1))
                                       (map second (list-tail ind 1))))
                               population))
                        (reverse populations)))
           (best (best result))
           (production (map (λ (population) (map (λ (individual) (seventh individual)) population)) result))
           (liquid (map (λ (population) (map (λ (individual) (sixth individual)) population)) result))
           (best-for-generation (best-for-generation result))
           (med-for-generation (med-for-generation result)))
           (values best production liquid best-for-generation med-for-generation))))

(define (best-for-generation populations)
  (let ((result '()))
    (for ([i (length populations)]
          [population populations])
      (set! result (cons (list i (fifth (first (sort population < #:key fifth)))) result)))
    (reverse result)))

(define (med-for-generation populations)
  (let* ((m-population (map (λ (x) (med x)) populations))
         (result '()))
    (for ([i (length m-population)]
          [population m-population])
      (set! result (cons (list i population) result)))
    (reverse result)))

(define (med population)
  (/ (apply + (map (λ (x) (fifth x)) population)) (length population)))

(define (best populations)
  (first (sort (last populations) < #:key fifth)))

(define (add-fitness population threshold)
  (map (λ (x) 
         (cons (- threshold (standard-deviation (map third x))) x))
       population))

(define (standard-deviation lst)
  (let* ((lt (length lst))
         (av (/ (apply + lst) lt)))
    (letrec ((rec (λ (lst acc)
                    (cond
                      ((empty? lst) (sqrt (/ acc (sub1 lt))))
                      (else (rec (cdr lst) (+ (expt (- (car lst) av) 2) acc)))))))
      (rec lst 0))))

(define (compose-new-generation f-population elitism selection tournament-k pc pm)
  (let ((new-generation (select-best elitism f-population))
        (population (case selection
                      ('roulette (add-cumulative-fitness f-population))
                      ('tournament f-population))))
    (for ([i (/ (- (length f-population) elitism) 2)])
      (letrec ((ind-rec (λ (n1 n2)
                          (if (and (is-valid? n1)
                                   (is-valid? n2))
                              (set! new-generation (cons n1 (cons n2 new-generation)))
                              (let*-values ([(p1 p2) (select-parents population selection tournament-k)]
                                            [(b1 b2) (binary-parents p1 p2)])
                                (letrec ((rec (λ (c1 c2)
                                                (if (and (is-all-periods-valid? c1)
                                                         (is-all-periods-valid? c2))
                                                    (values c1 c2)
                                                    (let*-values ([(f1 f2) (crossover b1 b2 pc)]
                                                                  [(m1 m2) (mutation f1 f2 pm)]
                                                                  [(d1 d2) (decimal-sons m1 m2)])
                                                      (rec d1 d2))))))
                                  (let-values ([(ind1 ind2) (rec '() '())])
                                    (ind-rec ind1 ind2))))))))
        (ind-rec '() '())))
    new-generation))

(define (is-all-periods-valid? cromossomes)
  (if (empty? cromossomes) #f
      (letrec ((rec (λ (cromossomes index acc)
                      (if (empty? cromossomes) acc
                          (rec (cdr cromossomes) (add1 index) (and acc (is-period-valid? (car cromossomes))))))))
        (rec cromossomes 0 #t))))
          
(define (select-best k f-population)
  (map (λ (x) (list-tail x 1))
       (subseq (sort f-population > #:key first) k)))

(define (subseq list end)
  (letrec ((subseq-aux (λ (result lst end)
                         (cond
                           ((or (zero? end) (empty? lst)) (reverse result))
                           (else (subseq-aux (cons (car lst) result) (cdr lst) (sub1 end)))))))
    (subseq-aux '() list end)))

(define (add-cumulative-fitness f-population)
  (let ((sum (apply + (map first f-population)))
        (acc 0))
    (map (λ (ind)
           (let ((aux acc))
             (set! acc (+ acc (/ (first ind) sum)))
             (list 
                   aux
                   acc
                   (second ind)
                   (third ind)
                   (fourth ind)
                   (fifth ind))))
         f-population)))

(define (select-parents population selection tournament-k)
  (case selection
    ('roulette (let* ((rnd1 (%RANDOM))
                      (rnd2 (%RANDOM))
                      (p1 (findf (λ (x) (search-proc x rnd1)) population))
                      (p2 (findf (λ (x) (search-proc x rnd2)) population)))
                 (values (list-tail p1 2) (list-tail p2 2))))
    ('tournament (let* ((r1 (list-tail (first (sort (select-random tournament-k population) > #:key first)) 1))
                        (r2 (list-tail (first (sort (select-random tournament-k population) > #:key first)) 1)))
                   (values r1 r2)))))

(define (select-random k population)
  (letrec ((select-random-aux (λ (result k population lt)
                                (cond
                                  ((zero? k) result)
                                  (else (select-random-aux (cons (list-ref population (random lt)) result)
                                                           (sub1 k)
                                                           population lt))))))
    (select-random-aux '() k population (length population))))


(define (search-proc x rnd)
  (and (< (first x) rnd) (> (second x) rnd)))

(define (binary-parents p1 p2)
  (values (map first p1) (map first p2)))

(define (decimal->binary x resolution)
  (letrec ((decimal->binary-aux (λ (decimal result i resolution)
                                  (cond
                                    ((zero? i) result)
                                    ((zero? decimal) (decimal->binary-aux decimal (string-append "0" result)
                                                                          (sub1 i)
                                                                          resolution))
                                    (else (decimal->binary-aux (quotient decimal 2)
                                                               (string-append (number->string (inexact->exact (remainder decimal 2))) result)
                                                               (sub1 i)
                                                               resolution))))))
    (decimal->binary-aux x "" resolution resolution)))

(define (crossover p1 p2 pc)
  (if (<= (%RANDOM) pc)
      (let* ((resolution (string-length (first p1)))
             (index (list (random resolution)
                          (random resolution)
                          (random resolution)
                          (random resolution))))
        (values (map swap p1 p2 index) (map swap p2 p1 index)))
      (values p1 p2)))

(define (swap str1 str2 index)
  (string-append (substring str1 0 index) (substring str2 index)))

(define (mutation i1 i2 pm)
  (values (map (λ (x) (single-mutation x pm)) i1)
          (map (λ (x) (single-mutation x pm)) i2)))

(define (single-mutation individual pm)
  (list->string
   (map (λ (y)
          (if (<= (%RANDOM) pm) (if (char=? y #\0) #\1 #\0) y))
        (string->list individual))))

(define (decimal-sons m1 m2)
    (values (decimal-sons-aux m1 3 '()) (decimal-sons-aux m1 3 '())))

(define (decimal-sons-aux lst index acc)
  (if (empty? lst) acc
      (let* ((x (car lst))
             (p (period-production x)))
        (decimal-sons-aux (cdr lst) (sub1 index) (cons (list x p (- p (power-per-period index))) acc)))))

(define (get-liquid-power individual)
  (decimal-sons-aux (subseq individual 4) 3 '()))