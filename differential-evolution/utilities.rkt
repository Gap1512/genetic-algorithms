#lang racket

(provide %RANDOM subseq sort-first swap)

(define %RANDOM (λ () (/ (random 1000000) 1000000)))

(define (subseq list end)
  (letrec ((subseq-aux (λ (result lst end)
                         (cond
                           ((or (zero? end) (empty? lst)) (reverse result))
                           (else (subseq-aux (cons (car lst) result) (cdr lst) (sub1 end)))))))
    (subseq-aux '() list end)))

(define (sort-first lst #:proc (proc <) #:key (key identity))
  (letrec ((rec (λ (lst min)
                  (cond
                    ((empty? lst) min)
                    (else (rec (cdr lst) (if (proc (key (car lst)) (key min))
                                             (car lst)
                                             min)))))))
    (rec (cdr lst) (car lst))))

(define (swap str1 str2 index)
  (string-append (substring str1 0 index) (substring str2 index)))