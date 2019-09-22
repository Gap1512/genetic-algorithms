;;;; shared.lisp

(in-package #:tournament-elitism)

(defparameter *rnd-precision* 1000000)
(defvar *rnd-fn* #'(lambda () (/ (random *rnd-precision*) *rnd-precision*)))
(defparameter *fn* #'(lambda (x) (+ (* x (sin (expt x 4))) (cos (* x x)))))
;(defparameter *fn* #'(lambda (x) (* -1 (abs (* x (sin (sqrt (abs x))))))))
