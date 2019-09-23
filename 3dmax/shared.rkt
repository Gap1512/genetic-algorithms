#lang racket

(define GA-FUNCTION (λ (x) (+ (* x (sin (expt x 4))) (cos (* x x)))))

;(define GA-FUNCTION (λ (x y) (+ 21.5 (* x (sin (* 4 pi x))) (* y (sin (* 20 pi y))))))
;(lambda (x) (- (+ 480 (* -1 (abs (* x (sin (sqrt (abs x)))))))))

(provide GA-FUNCTION)
