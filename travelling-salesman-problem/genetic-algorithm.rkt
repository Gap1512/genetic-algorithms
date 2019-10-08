#lang racket

(require  "utilities.rkt")

(provide genetic-algorithm)

(define %RANDOM (λ () (/ (random 1000000) 1000000)))

(define %RANDOM-POINT (λ () (random 1000)))
(define MAX-TRAVEL 1500)

(define (genetic-algorithm #:selection (selection 'tournament)
                           #:tournament-k (tournament-k 4)
                           #:resolution (resolution 6)
                           #:elitism (elitism 2)
                           #:pc (pc 0.6)
                           #:pm (pm 0.01)
                           #:last-generation (last-generation 15)
                           #:number-of-individuals (number-of-individuals 40))
    (update-population (generate-initial-population number-of-individuals resolution) selection tournament-k elitism pc pm last-generation (* MAX-TRAVEL resolution)))

(define (generate-initial-population number-of-individuals resolution)
  (let ((initial-population '())
        (cities (generate-random-points resolution)))
    (for ([i number-of-individuals])
      (set! initial-population (cons (generate-individual cities) initial-population)))
    initial-population))

(define (generate-random-points n)
  (letrec ((rec (λ (n result)
                  (cond
                    ((zero? n) result)
                    (else (rec (sub1 n) (cons (list (%RANDOM-POINT) (%RANDOM-POINT)) result)))))))
    (rec n '())))

(define (generate-individual cities)
  (shuffle cities))

(define (update-population initial-population selection tournament-k elitism pc pm last-generation threshold)
  (let ((populations '())
        (actual-population initial-population))
    (for ([actual-generation last-generation])
      (let* ((formated-population (add-fitness actual-population threshold)))
        (set! actual-population (compose-new-generation formated-population elitism selection tournament-k pc pm))
        (set! populations (cons formated-population populations))))
    (let ((result (map (λ (population) (map (λ (x) (cons (- threshold (first x)) (list-tail x 1))) population)) (reverse populations))))
    (values (map (λ (x) (map (λ (y) (list-tail y 1)) x)) result)
            (best result first) (best-for-generation result first) (med-for-generation result first)))))

(define (best populations key)
  (first (sort (last populations) < #:key key)))

(define (best-for-generation populations key)
  (let ((result '()))
    (for ([i (length populations)]
          [population populations])
      (set! result (cons (list i (key (first (sort population < #:key key)))) result)))
    (reverse result)))

(define (med-for-generation populations key)
  (let* ((m-population (map (λ (x) (med x key)) populations))
         (result '()))
    (for ([i (length m-population)]
          [population m-population])
      (set! result (cons (list i population) result)))
    (reverse result)))

(define (med population pos)
  (/ (apply + (map (λ (x) (pos x)) population)) (length population)))

(define (add-fitness population threshold)
  (let ((fitness (map (λ (x) (let ((y (group x 2))) (- threshold (apply + (map distance y))))) population)))
    (map cons fitness population)))

(define (distance lst)
  (let ((a (first lst))
        (b (second lst)))
    (sqrt (+ (sqr (- (first b)
                      (first a)))
             (sqr (- (second b)
                      (second a)))))))

(define (compose-new-generation f-population elitism selection tournament-k pc pm)
  (let ((new-generation (select-best elitism f-population))
        (population (case selection
                      ('roulette (add-cumulative-fitness f-population))
                      ('tournament f-population))))
    (for ([i (/ (- (length f-population) elitism) 2)])
      (let*-values ([(p1 p2) (select-parents population selection tournament-k)]
                    [(f1 f2) (crossover p1 p2 pc)]
                    [(m1 m2) (mutation f1 f2 pm)])
        (set! new-generation (cons m1 (cons m2 new-generation)))))
    new-generation))

(define (select-best k f-population)
  (map (λ (x) (list-tail x 1))
       (take (sort f-population > #:key first) k)))

(define (add-cumulative-fitness f-population)
  (let ((sum (apply + (map (λ (x) (first x)) f-population)))
        (acc 0))
    (map (λ (ind)
           (let ((aux acc))
             (set! acc (+ acc (/ (first ind) sum)))
             (cons aux (cons acc (list-tail ind 1)))))
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

(define (search-proc x rnd)
  (and (< (first x) rnd) (> (second x) rnd)))

(define (select-random k population)
  (letrec ((select-random-aux (λ (result k population lt)
                                (cond
                                  ((zero? k) result)
                                  (else (select-random-aux (cons (list-ref population (random lt)) result)
                                                           (sub1 k)
                                                           population lt))))))
    (select-random-aux '() k population (length population))))

(define (crossover p1 p2 pc)
  (if (<= (%RANDOM) pc)
      (let* ((resolution (length p1))
             (index1 (random resolution))
             (index2 (random resolution)))
        (pnx p1 p2
             (if (< index1 index2) index1 index2)
             (if (< index1 index2) index2 index1)))
      (values p1 p2)))

(define (pnx ind1 ind2 begin end)
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

(define (mutation i1 i2 pm)
  (values (single-mutation i1 pm) (single-mutation i2 pm)))

(define (single-mutation individual pm)
  (let ((result individual)
        (resolution (length individual)))
    (for ([í result]
          [j resolution])
      (if (<= (%RANDOM) pm)
          (let ((pos (random j resolution)))
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