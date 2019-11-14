#lang racket

(require "utilities.rkt" "shared.rkt")

(provide differential-evolution)

(define (differential-evolution #:function (fn (rosenbrock 2 1 100)) 
                                #:number-of-variables (fnt 2)
                                #:mode (md 'min)
                                #:range (r (n-range '(-3 3) 2))
                                #:pc (pc 0.8)
                                #:pm (pm 0.01)
                                #:last-generation (lg 70)
                                #:number-of-individuals (nind 20))
  (update-population (generate-initial-population fnt nind r) fn fnt md r pc pm lg))

(define (generate-initial-population number-of-variables number-of-individuals range)
    (letrec ((rec (λ (n result)
                    (cond
                      ((zero? n) result)
                      (else (rec (sub1 n) (cons (new-individual number-of-variables range) result)))))))
      (rec number-of-individuals '())))

(define (new-individual number-of-variables range)
  (letrec ((rec (λ (n range result)
                  (cond
                    ((zero? n) (reverse result))
                    (else (rec (sub1 n) (cdr range) (cons (real-random (caar range) (cadar range)) result)))))))
    (rec number-of-variables range '())))

(define (real-random min max)
  (+ min (* (%RANDOM) (- max min))))

(define (update-population initial-population function number-of-variables mode range pc pm last-generation)
  (let ((populations '())
        (actual-population initial-population))
    (letrec ((rec (λ (actual-generation)
                    (let ((formated-population (add-fitness actual-population function)))
                      (cond
                        ((= actual-generation last-generation)
                         (let* ((result (reverse populations))
                                (fn (case mode ('max >) ('min <))))
                           (values (best result function fn) (best-for-generation result fn) (med-for-generation result))))
                        (else
                         (begin
                           (set! actual-population (compose-new-generation function formated-population pc pm range (case mode ('max >) ('min <))))
                           (set! populations (cons formated-population populations))
                           (rec (add1 actual-generation)))))))))
      (rec 0))))

(define (best populations op-function fn)
  (let ((result (list-tail (first (sort (last populations) fn #:key first)) 1)))
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

(define (add-fitness actual-population function)
  (map (λ (ind) (add-fitness-to-individual ind function)) actual-population))

(define (add-fitness-to-individual ind function)
  (cons (apply function ind) ind))

(define (compose-new-generation function formated-population pc pm range optimization-mode)
  (map (λ (ind) (select-between ind (add-fitness-to-individual (verify-range (recombination (list-tail ind 1) (mutation (select-random 3 (remove ind formated-population)) pm) pc) range) function) optimization-mode))
       formated-population))

(define (select-random k population)
  (letrec ((select-random-aux (λ (result k population lt)
                                (cond
                                  ((zero? k) result)
                                  (else (select-random-aux (cons (list-tail (list-ref population (random lt)) 1) result)
                                                           (sub1 k)
                                                           population lt))))))
    (select-random-aux '() k population (length population))))

(define (mutation vectors-list F)
  (map + (first vectors-list) (map (λ (vector) (* vector F)) (map - (second vectors-list) (third vectors-list)))))

(define (recombination target-vector donor-vector CR)
  (let ((j 0)
        (I (random (length target-vector))))
    (map (λ (v x)
           (set! j (add1 j))
           (let ((r (%RANDOM)))
             (cond
               ((or (= j I) (<= r CR)) v)
               (else x))))
         donor-vector target-vector)))

(define (select-between target-vector trial-vector optimization-mode)
  (if (optimization-mode (first target-vector) (first trial-vector))
      (list-tail target-vector 1)
      (list-tail trial-vector 1)))

(define (verify-range ind range)
  (letrec ((rec (λ (x r result)
                  (cond
                    ((empty? x) (reverse result))
                    (else (let ((n (car x))
                                (a (car r)))
                            (rec (cdr x) (cdr r) (cons (cond
                                                         ((< n (first a)) (first a))
                                                         ((> n (second a)) (second a))
                                                         (else n)) result))))))))
    (rec ind range '())))