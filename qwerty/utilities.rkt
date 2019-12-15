#lang racket

(provide group format-keyboard ALPHABET QWERTY IDEAL-TAPPING-WORKLOAD KEYBOARD-REPRESENTATION BIG-STEPS)

(define (group lst n)
  (if (zero? n) (error "Length Zero")
      (letrec ((rec (λ (lst acc)
                      (cond
                        ((< (length lst) n) (reverse acc))
                        (else (rec (cdr lst) (cons (take lst n) acc)))))))
        (rec lst '()))))

(define (format-keyboard keyboard)
  (list (take keyboard 10)
        (take-right (take keyboard 20) 10)
        (take-right keyboard 7)))

(define ALPHABET '(#\A #\B #\C #\Ç #\D #\E #\F #\G #\H #\I #\J #\K #\L #\M #\N #\O #\P #\Q #\R #\S #\T #\U #\V #\W #\X #\Y #\Z))

(define QWERTY '(#\Q #\W #\E #\R #\T #\Y #\U #\I #\O #\P #\A #\S #\D #\F #\G #\H #\J #\K #\L #\Ç #\Z #\X #\C #\V #\B #\N #\M))

(define IDEAL-TAPPING-WORKLOAD '(0.01667 0.00833 0.02500 0.04167 0.03333 0.03333 0.04167 0.03333 0.01111 0.02222
                                         0.03333 0.01667 0.05000 0.08333 0.06667 0.06667 0.08333 0.06667 0.02222 0.04444
                                         0.01667 0.00833 0.02500 0.04167 0.03333 0.03333 0.04167))

(define KEYBOARD-REPRESENTATION '((1 1 5 4) (1 1 4 3) (1 1 3 2) (1 1 2 1) (1 1 1 1) (2 1 1 1) (2 1 2 1) (2 1 3 2) (2 1 4 3) (2 1 5 4)
                                          (1 2 5 4) (1 2 4 3) (1 2 3 2) (1 2 2 1) (1 2 1 1) (2 2 1 1) (2 2 2 1) (2 2 3 2) (2 2 4 3) (2 2 5 4)
                                          (1 3 5 4) (1 3 4 3) (1 3 3 2) (1 3 2 1) (1 3 1 1) (2 3 1 1) (2 3 2 1)))

(define BIG-STEPS '(((1 1) 0) ((1 2) 5) ((1 3) 8) ((1 4) 6)
                              ((2 1) 5) ((2 2) 0) ((2 3) 9) ((2 4) 7)
                              ((3 1) 8) ((3 2) 9) ((3 3) 0) ((3 4) 10)
                              ((4 1) 6) ((4 2) 7) ((4 3) 10) ((4 4) 0)))