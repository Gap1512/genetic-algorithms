#lang racket

(require "utilities.rkt")

(provide genetic-algorithm)

(define (genetic-algorithm #:function (fn (λ (x y) (+ (sqr (sub1 x)) (sqr (sub1 y)))))
                           #:number-of-variables (number-of-variables 2)
                           #:mode (mode 'min)
                           #:continous? (continous? #f)
                           #:threshold (threshold 500)
                           #:selection (selection 'tournament)
                           #:tournament-k (tournament-k 4)
                           #:elitism (elitism 2)
                           #:resolution (resolution 10)
                           #:range (range '((-3 5) (-3 5)))
                           #:restrictions? (restrictions? #t)
                           #:rp (rp 0.5)
                           #:equation-restriction (equation-restriction (λ (x y) (sqr (- x y 2))))
                           #:inequation-restriction (inequation-restriction (λ (x y) (sqr (max 0 (+ x y -0.5)))))
                           #:c-mode (c-mode 'traditional)
                           #:pc (pc 0.6)
                           #:m-mode (m-mode 'traditional)
                           #:pm (pm 0.01)
                           #:last-generation (last-generation 70)
                           #:number-of-individuals (number-of-individuals 50))
  (update-population (generate-initial-population number-of-variables number-of-individuals continous? resolution range)
                       fn number-of-variables mode
                       continous? threshold selection
                       tournament-k elitism resolution range
                       restrictions? rp equation-restriction inequation-restriction
                       c-mode pc m-mode pm last-generation))

(define (generate-initial-population number-of-variables number-of-individuals continous? resolution range)
    (letrec ((rec (λ (n result)
                    (cond
                      ((zero? n) result)
                      (else (rec (sub1 n) (cons (new-individual number-of-variables resolution range continous?) result)))))))
      (rec number-of-individuals '())))

(define (new-individual number-of-variables resolution range continous?)
  (letrec ((rec (λ (n range result)
                  (cond
                    ((zero? n) (reverse result))
                    (else (rec (sub1 n) (cdr range) (cons (if continous?
                                                              (real-random (caar range) (cadar range))
                                                              (new-discrete-variable resolution))
                                                          result)))))))
    (rec number-of-variables range '())))

(define (new-discrete-variable resolution)
  (letrec ((rec (λ (n res)
                  (cond
                    ((zero? n) res)
                    (else (rec (sub1 n) (string-append (if (zero? (random 2)) "0" "1") res)))))))
    (rec resolution "")))

(define (real-random min max)
  (+ min (* (%RANDOM) (- max min))))

(define (update-population initial-population
                           function number-of-variables mode
                           continous? threshold selection
                           tournament-k elitism resolution range
                           penalty? rp eq-r ineq-r
                           c-mode pc m-mode pm last-generation)
  (let ((max-value (expt 2 resolution))
        (populations '())
        (actual-population initial-population))
    (letrec ((rec (λ (actual-generation)
                    (let ((formated-population (add-fitness actual-population function threshold continous? range max-value (case mode ('max +) ('min -))
                                                            penalty? (case mode ('max -) ('min +)) rp eq-r ineq-r)))
                      (cond
                        ((= actual-generation last-generation)
                         (let* ((result (remove-threshold (reverse populations) threshold mode))
                                (fn (case mode ('max >) ('min <))))
                           (values (best result function fn continous? range max-value) (best-for-generation result fn) (med-for-generation result))))
                        (else
                         (begin
                           (set! actual-population (compose-new-generation formated-population elitism selection tournament-k c-mode pc m-mode pm range))
                           (set! populations (cons formated-population populations))
                           (rec (add1 actual-generation)))))))))
      (rec 0))))

(define (remove-threshold populations threshold mode)
  (map (λ (population)
         (map (λ (individual)
                (cons (case mode
                        ('max (- (first individual) threshold))
                        ('min (- threshold (first individual))))
                      (list-tail individual 1)))
              population))
       populations))

(define (best populations op-function fn continous? range max-value)
  (let ((result (if continous?
                    (list-tail (first (sort (last populations) fn #:key first)) 1)
                    (format-individual (list-tail (first (sort (last populations) fn #:key first)) 1) range max-value))))
    (append result (list (apply op-function result)))))

(define (best-for-generation populations fn)
  (let ((result '()))
    (for ([i (length populations)]
          [population populations])
      (set! result (cons (list i (first (first (sort population fn #:key first)))) result)))
    (reverse result)))

(define (med-for-generation populations)
  (let* ((m-population (map med populations))
         (result '()))
    (for ([i (length m-population)]
          [population m-population])
      (set! result (cons (list i population) result)))
    (reverse result)))

(define (med population)
  (/ (apply + (map first population)) (length population)))

(define (add-fitness population function threshold continous? range max-value mode penalty? penalty-mode rp eq-r ineq-r)
  (map (λ (ind)
         (let ((f-ind (if continous? ind (format-individual ind range max-value))))
           (cons (mode threshold
                       (if penalty?
                           (penalty-mode (apply function f-ind) (* rp (+ (apply eq-r f-ind) (apply ineq-r f-ind))))
                           (apply function f-ind)))
                 ind)))
       population))

(define (format-individual individual range max-value)
  (map (λ (i r) (format-variable i r max-value)) individual range))

(define (format-variable variable range max-value)
  (+ (car range) (* (/ (string->number variable 2) max-value) (- (cadr range) (car range)))))

(define (compose-new-generation f-population elitism selection tournament-k c-mode pc m-mode pm range)
  (let ((new-generation (select-best elitism f-population))
        (population (case selection
                      ('roulette (add-cumulative-fitness f-population))
                      ('tournament f-population))))
    (for ([i (/ (- (length f-population) elitism) 2)])
      (let*-values ([(p1 p2) (select-parents population selection tournament-k)]
                    [(f1 f2) (crossover p1 p2 pc c-mode range)]
                    [(m1 m2) (mutation f1 f2 pm m-mode range)])
        (set! new-generation (cons m1 (cons m2 new-generation)))))
    new-generation))

(define (select-best k f-population)
  (map (λ (x) (list-tail x 1))
       (take (sort f-population > #:key first) k)))

(define (add-cumulative-fitness f-population)
  (let ((sum (apply + (map first f-population)))
        (acc 0))
    (map (λ (ind)
           (let ((aux acc))
             (set! acc (+ acc (/ (first ind) sum)))
             (cons aux (cons acc (list-tail ind 1)))))
         f-population)))

(define (select-parents population selection tournament-k)
  (case selection
    ('roulette
     (let* ((rnd1 (%RANDOM))
            (rnd2 (%RANDOM))
            (p1 (findf (λ (x) (search-proc x rnd1)) population))
            (p2 (findf (λ (x) (search-proc x rnd2)) population)))
       (values (list-tail p1 2) (list-tail p2 2))))
    ('tournament
     (let* ((r1 (list-tail (first (sort (select-random tournament-k population) > #:key first)) 1))
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

(define (crossover p1 p2 pc c-mode range)
  (let ((resolution (if (equal? c-mode 'traditional) (string-length (first p1)) 1)))
    (letrec ((rec (λ (p1 p2 res1 res2 range)
                    (cond
                      ((empty? p1) (values (reverse res1) (reverse res2)))
                      (else
                       (let ((index (if (equal? c-mode 'traditional)
                                        (random resolution)
                                        (%RANDOM)))
                             (i1 (car p1))
                             (i2 (car p2)))
                         (if (<= (%RANDOM) pc)
                             (case c-mode
                               ('traditional
                                (rec (cdr p1) (cdr p2)
                                  (cons (single-point-crossover i1 i2 index 0) res1)
                                  (cons (single-point-crossover i1 i2 index 1) res2)
                                  (cdr range)))
                               ('radcliff
                                (rec (cdr p1) (cdr p2)
                                  (cons (radcliff-crossover i1 i2 index 0) res1)
                                  (cons (radcliff-crossover i1 i2 index 1) res2)
                                  (cdr range)))
                               ('wright
                                (let ((r (random 10000)))
                                  (rec (cdr p1) (cdr p2)
                                    (cons (wright-crossover i1 i2 r (car range)) res1)
                                    (cons (wright-crossover i1 i2 (add1 r) (car range)) res2)
                                    (cdr range)))))
                             (rec (cdr p1) (cdr p2)
                               (cons i1 res1)
                               (cons i1 res2)
                               (cdr range)))))))))
      (rec p1 p2 '() '() range))))

(define (single-point-crossover str1 str2 begin n)
    (if (zero? n)
        (string-append (substring str1 0 begin) (substring str2 begin))
        (string-append (substring str2 0 begin) (substring str1 begin))))

(define (radcliff-crossover real1 real2 beta n)
  (if (zero? n)
      (+ (* beta real1) (* (- 1 beta) real2))
      (+ (* (- 1 beta) real1) (* beta real2))))

(define (wright-crossover real1 real2 n range)
  (let ((x (case (remainder n 3)
             ('0 (+ (* 0.5 real1) (* 0.5 real2)))
             ('1 (- (* 1.5 real1) (* 0.5 real2)))
             ('2 (- (* 1.5 real2) (* 0.5 real1))))))
    (cond
      ((> x (second range)) (second range))
      ((< x (first range)) (first range))
      (else x))))

(define (mutation p1 p2 pm m-mode range)
  (letrec ((rec (λ (p1 p2 res1 res2 range)
                  (cond
                    ((empty? p1) (values (reverse res1) (reverse res2)))
                    (else (case m-mode
                            ('traditional 
                             (rec (cdr p1) (cdr p2)
                               (cons (single-mutation (car p1) pm) res1)
                               (cons (single-mutation (car p2) pm) res2)
                               (cdr range)))
                             ('continous
                              (rec (cdr p1) (cdr p2)
                                (cons (single-continous-mutation (car p1) pm (car range)) res1)
                                (cons (single-continous-mutation (car p2) pm (car range)) res2)
                                (cdr range)))))))))
    (rec p1 p2 '() '() range)))

(define (single-mutation individual pm)
  (list->string
   (map (λ (y)
          (if (<= (%RANDOM) pm) (if (char=? y #\0) #\1 #\0) y))
        (string->list individual))))

(define (single-continous-mutation individual pm range)
  (if (<= (%RANDOM) pm)
      (real-random (car range) (cadr range))
      individual))