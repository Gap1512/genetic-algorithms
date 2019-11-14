#lang racket

(provide rastrigin rosenbrock n-range)

(define (rastrigin n)
  (let ((variables (variable-list n)))
    (eval (list 'λ variables
                (cons '+ (cons (* 10 n) (get-rastrigin-terms variables)))))))

(define (rosenbrock n a b)
  (let ((variables (variable-list n)))
    (eval (list 'λ variables
                (cons '+ (get-rosenbrock-terms variables (sub1 n) a b))))))

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

(define (get-rosenbrock-terms variable-list n a b)
  (letrec ((rec (λ (x result)
                  (cond
                    ((= n x) result)
                    (else (rec (add1 x)
                            (cons `(+ (sqr (+ ,a ,(list-ref variable-list x))) (* ,b (sqr (- ,(list-ref variable-list x) (sqr ,(list-ref variable-list (add1 x))))))) result)))))))
    (rec 0 '())))

(define (n-range range n)
  (letrec ((rec (λ (i res)
                  (cond
                    ((zero? i) res)
                    (else (rec (sub1 i) (cons range res)))))))
    (rec n '())))