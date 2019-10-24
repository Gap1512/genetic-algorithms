#lang racket

(define GA-FUNCTION (λ (x) (+ (* x (sin (expt x 4))) (cos (* x x)))))

(define GA-FUNCTION-3D (λ (x y) (+ 21.5 (* x (sin (* 4 pi x))) (* y (sin (* 20 pi y))))))
;(lambda (x) (- (+ 480 (* -1 (abs (* x (sin (sqrt (abs x)))))))))

(provide GA-FUNCTION GA-FUNCTION-3D rastrigin rastrigin-range)

(define (rastrigin n)
  (let ((variables (variable-list n)))
    (eval (list 'λ variables
                (cons '+ (cons (* 10 n) (get-rastrigin-terms variables)))))))

(define (variable-list n)
  (letrec ((rec (λ (i res)
                  (cond
                    ((zero? i) res)
                    (else (rec (sub1 i) (cons (gensym) res)))))))
    (rec n '())))

(define (get-rastrigin-terms variable-list)
  (letrec ((rec (λ (v-lst result)
                  (cond
                    ((empty? v-lst) result)
                    (else (rec (cdr v-lst) (cons `(- (* ,(car v-lst) ,(car v-lst)) (* 10 (cos (* 2 pi ,(car v-lst)))))
                                                 result)))))))
    (rec variable-list '())))

(define (rastrigin-range range n)
  (letrec ((rec (λ (i res)
                  (cond
                    ((zero? i) res)
                    (else (rec (sub1 i) (cons range res)))))))
    (rec n '())))