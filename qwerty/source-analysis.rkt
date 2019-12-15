#lang racket

(require math racket/system "utilities.rkt")

(provide pdf->txt multiple-files-frequencies format-source multiple-files-pdf->txt relative-frequency)

(define (pdf->txt input-path output-path)
  (system (string-append "gswin64c -sDEVICE=txtwrite -o \"" output-path "\" \"" input-path "\"")))

(define (multiple-files-pdf->txt input-path-list gauge)
  (for-each (λ (path)
              (begin
                (pdf->txt path (path->string (path-replace-extension path #".txt")))
                (send gauge set-value (add1 (send gauge get-value)))))
            input-path-list))

(define (read-chars path valid gauge)
  (when gauge (send gauge set-value (add1 (send gauge get-value))))
  (filter (λ (c) (member c valid)) (map char-upcase (port->list read-char (open-input-file path)))))

;(multiple-files-frequencies '("C:\\home\\racket\\local-projects\\genetic-algorithm\\qwerty\\source\\Auto Da Barca Do Inferno.txt"))
(define (multiple-files-frequencies list-of-files (gauge #f) (group-size 1) (valid ALPHABET))
  (let-values ([(samples frequencies) (count-samples (read-multiple-groups list-of-files gauge group-size valid))])
    (map list samples frequencies)))
  
(define (read-multiple-groups list-of-files gauge group-size valid)
  (letrec ((rec (λ (lst result)
                  (cond
                    ((empty? lst) result)
                    (else (rec (cdr lst) (append (group (read-chars (car lst) valid gauge) group-size) result)))))))
    (rec list-of-files '())))

(define (format-source f-lst)
  (letrec ((rec (λ (lst chrs freqs)
                  (cond
                    ((empty? lst) (list chrs freqs))
                    (else (rec (cdr lst)
                            (cons (string (caaar lst)) chrs)
                            (cons (number->string (cadar lst)) freqs)))))))
    (rec f-lst '() '())))

;(relative-frequency (multiple-files-frequencies '("C:\\home\\racket\\local-projects\\genetic-algorithm\\qwerty\\source\\Auto Da Barca Do Inferno.txt")))
(define (relative-frequency letters)
  (let ((total (apply + (map second letters))))
    (map (λ (term) (list-set term 1 (/ (second term) total))) letters)))