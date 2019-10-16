#lang racket

(provide char->boolean group xormap count-true standard-deviation swap subseq)

(define (char->boolean char)
  (not (char=? #\0 char)))

(define (group lst n)
  (if (zero? n) (error "Length Zero")
      (letrec ((rec (λ (lst acc)
                      (cond
                        ((< (length lst) n) (reverse acc))
                        (else (rec (cdr lst) (cons (subseq lst n) acc)))))))
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

(define (count-true blst)
  (letrec ((rec (λ (blst acc)
                  (cond
                    ((empty? blst) acc)
                    (else (rec (cdr blst) (if (car blst)
                                              (add1 acc)
                                              acc)))))))
    (rec blst 0)))

(define (standard-deviation lst)
  (let* ((lt (length lst))
         (av (/ (apply + lst) lt)))
    (letrec ((rec (λ (lst acc)
                    (cond
                      ((empty? lst) (sqrt (/ acc (sub1 lt))))
                      (else (rec (cdr lst) (+ (expt (- (car lst) av) 2) acc)))))))
      (rec lst 0))))

(define (subseq list end)
  (letrec ((subseq-aux (λ (result lst end)
                         (cond
                           ((or (zero? end) (empty? lst)) (reverse result))
                           (else (subseq-aux (cons (car lst) result) (cdr lst) (sub1 end)))))))
    (subseq-aux '() list end)))

(define (swap str1 str2 index)
  (string-append (substring str1 0 index) (substring str2 index)))