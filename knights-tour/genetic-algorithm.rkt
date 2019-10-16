#lang racket

(require  "utilities.rkt")

(provide genetic-algorithm)

(define %RANDOM (λ () (/ (random 1000000) 1000000)))

(define (genetic-algorithm #:selection (selection 'tournament)
                           #:tournament-k (tournament-k 8)
                           #:elitism (elitism 0)
                           #:pc (pc 0.6)
                           #:pm (pm 0.01)
                           #:pa (pa 0.5)
                           #:number-of-individuals (number-of-individuals 80))
  (update-population (generate-initial-population number-of-individuals) selection tournament-k elitism pc pm pa))

(define (generate-initial-population number-of-individuals)
  (let ((initial-population '()))
    (for ([i number-of-individuals])
      (set! initial-population (cons (list (generate-binary-path) (generate-random-position)) initial-population)))
    initial-population))

(define (generate-binary-path)
  (letrec ((rec (λ (n result)
                  (cond
                    ((zero? n) result)
                    (else (string-append (simple-binary-converter (random 8)) (rec (sub1 n) result)))))))
    (rec 64 "")))

(define (generate-random-position)
  (list (random 8) (random 8)))

(define (simple-binary-converter n)
  (cond
    ((string? n)
     (case n
       ('"000" 0)
       ('"001" 1)
       ('"010" 2)
       ('"011" 3)
       ('"100" 4)
       ('"101" 5)
       ('"110" 6)
       ('"111" 7)))
    (else
     (case n
       ('0 "000")
       ('1 "001")
       ('2 "010")
       ('3 "011")
       ('4 "100")
       ('5 "101")
       ('6 "110")
       ('7 "111")))))

(define (update-population initial-population selection tournament-k elitism pc pm pa)
  (let ((populations '())
        (actual-population initial-population))
    (letrec ((rec (λ (actual-generation)
                    (let* ((formated-population (add-fitness actual-population)))
                      (cond
                        ((path-found? formated-population)
                         (let ((result (reverse (cons formated-population populations))))
                           (values (best formated-population first) (best-for-generation result first) (med-for-generation result first))))
                        (else
                         (begin
                           (set! actual-population (compose-new-generation formated-population elitism selection tournament-k pc pm pa))
                           (set! populations (cons formated-population populations))
                           (rec (add1 actual-generation)))))))))
      (rec 0))))

(define (path-found? f-population)
  (findf (λ (x) (= 64 (first x))) f-population))

(define (best population key)
  (let ((ind (list-tail (first (sort population > #:key key)) 1)))
    (retrieve-path-from-string (second ind) (first ind))))

(define (best-for-generation populations key)
  (let ((result '()))
    (for ([i (length populations)]
          [population populations])
      (set! result (cons (list i (key (first (sort population > #:key key)))) result)))
    (reverse result)))

(define (med-for-generation populations key)
  (let* ((m-population (map (λ (x) (med x key)) populations))
         (result '()))
    (for ([i (length m-population)]
          [population m-population])
      (set! result (cons (list i population) result)))
    (reverse result)))

(define (med population pos)
  (/ (apply + (map (λ (x) (pos x)) population)) (length population)))

(define (add-fitness population)
  (map (λ (x) (cons (count-valid-moves (retrieve-path-from-string (second x) (first x))) x)) population))

(define (count-valid-moves ind)
  (letrec ((rec (λ (acc lst visited-lst)
                  (cond
                    ((or (empty? lst) (not (is-a-valid-move? (car lst) visited-lst))) acc)
                    (else (rec (add1 acc) (cdr lst) (cons (car lst) visited-lst)))))))
    (rec 0 ind '())))

(define (is-a-valid-move? pos lst-pos)
  (and (inside-board? pos)
       (not (member pos lst-pos))))

(define (inside-board? pos)
  (and (<= 0 (first pos) 7) (<= 0 (second pos) 7)))

(define (retrieve-path-from-string initial-pos str)
  (retrieve-path initial-pos (directions str)))

(define (directions str)
  (letrec ((rec (λ (str result)
                  (if (non-empty-string? str)
                      (rec (substring str 3) (cons (simple-binary-converter (substring str 0 3)) result))
                      (reverse result)))))
    (rec str '())))

(define (retrieve-path initial-pos directions)
  (letrec ((rec (λ (pos directions result)
                  (cond
                    ((empty? directions) (reverse result))
                    (else (rec (from-to pos (car directions)) (cdr directions) (cons pos result)))))))
    (rec initial-pos directions '())))

(define (from-to pos n)
  (let ((x (first pos))
        (y (second pos)))
    (case n
      ('0 (list (+ x 1) (- y 2)))
      ('1 (list (+ x 2) (- y 1)))
      ('2 (list (+ x 2) (+ y 1)))
      ('3 (list (+ x 1) (+ y 2)))
      ('4 (list (- x 1) (+ y 2)))
      ('5 (list (- x 2) (+ y 1)))
      ('6 (list (- x 2) (- y 1)))
      ('7 (list (- x 1) (- y 2))))))

(define (compose-new-generation f-population elitism selection tournament-k pc pm pa)
  (let ((new-generation (select-best elitism f-population))
        (population (case selection
                      ('roulette (add-cumulative-fitness f-population))
                      ('tournament f-population))))
    (for ([i (/ (- (length f-population) elitism) 2)])
      (let*-values ([(p1 p2) (select-parents population selection tournament-k)]
                    [(f1 f2) (crossover p1 p2 pc)]
                    [(m1 m2) (mutation f1 f2 pm)]
                    [(a1 a2) (adaptation m1 m2 pa)])
        (set! new-generation (cons a1 (cons a2 new-generation)))))
    new-generation))

(define (select-best k f-population)
  (map (λ (x) (list-tail x 1))
       (take (sort f-population > #:key first) k)))

(define (add-cumulative-fitness f-population)
  (let ((sum (apply + (map (λ (x) (first x)) f-population)))
        (acc 0))
    (map (λ (ind)
           (let ((aux acc))
             (set! acc (+ acc (/ (first ind) sum)))
             (cons aux (cons acc (list-tail ind 1)))))
         f-population)))

(define (select-parents population selection tournament-k)
  (case selection
    ('roulette (let* ((rnd1 (%RANDOM))
                      (rnd2 (%RANDOM))
                      (p1 (findf (λ (x) (search-proc x rnd1)) population))
                      (p2 (findf (λ (x) (search-proc x rnd2)) population)))
                 (values (list-tail p1 2) (list-tail p2 2))))
    ('tournament (let* ((r1 (list-tail (first (sort (select-random tournament-k population) > #:key first)) 1))
                        (r2 (list-tail (first (sort (select-random tournament-k population) > #:key first)) 1)))
                   (values r1 r2)))))

(define (search-proc x rnd)
  (and (< (first x) rnd) (> (second x) rnd)))

(define (select-random k population)
  (letrec ((select-random-aux (λ (result k population lt)
                                (cond
                                  ((zero? k) result)
                                  (else (select-random-aux (cons (list-ref population (random lt)) result)
                                                           (sub1 k)
                                                           population lt))))))
    (select-random-aux '() k population (length population))))

(define (crossover p1 p2 pc)
  (if (<= (%RANDOM) pc)
      (let* ((resolution (length p1))
             (index (random 192)))
        (single-point-crossover p1 p2 index))
      (values p1 p2)))

(define (two-point-crossover p1 p2 begin end)
  (let ((str1 (first p1))
        (str2 (first p2)))
    (values (list (string-append (substring str1 0 begin) (substring str2 begin end) (substring str1 end)) (second p1))
            (list (string-append (substring str2 0 begin) (substring str1 begin end) (substring str2 end)) (second p2)))))

(define (single-point-crossover p1 p2 begin)
  (let ((str1 (first p1))
        (str2 (first p2)))
    (values (list (string-append (substring str1 0 begin) (substring str2 begin)) (second p1))
            (list (string-append (substring str2 0 begin) (substring str1 begin)) (second p2)))))

(define (mutation i1 i2 pm)
  (values (single-mutation i1 pm) (single-mutation i2 pm)))

(define (single-mutation individual pm)
  (list (list->string
         (map (λ (y)
                (if (<= (%RANDOM) pm) (if (char=? y #\0) #\1 #\0) y))
              (string->list (first individual)))) (second individual)))

(define (adaptation i1 i2 pa)
  (values (single-adaptation i1 pa) (single-adaptation i2 pa)))

(define (single-adaptation ind pa)
  (if (<= (%RANDOM) pa)
      (adapt-until-failure (second ind) (first ind))
      ind))

(define (adapt-until-failure initial-pos str)
  (letrec ((rec (λ (pos rest-str result-str visited-lst)
                  (if (not (non-empty-string? rest-str))
                      (list result-str initial-pos)
                      (let ((new-pos (get-valid-position pos (simple-binary-converter (substring rest-str 0 3)) visited-lst)))
                        (rec
                            (from-to pos (simple-binary-converter (second new-pos)))
                          (substring rest-str 3)
                          (string-append result-str (second new-pos))
                          (cons pos visited-lst)))))))
  (rec initial-pos str "" '())))

(define (get-valid-position pos move-coord visited)
  (if (is-a-valid-move? (from-to pos move-coord) visited)
      (list pos (simple-binary-converter move-coord))
      (get-best-move-from pos visited move-coord)))

(define (get-best-move-from pos visited move-coord)
  (let ((all (get-all-valid-moves-from pos visited)))
    (if (empty? all) (list pos (simple-binary-converter move-coord))
        (list-tail (sort-first (map (λ (x) (add-valid-moves-count x visited)) all) #:key first) 1))))

(define (get-all-valid-moves-from pos visited)
  (letrec ((rec (λ (n result)
                  (cond
                    ((< n 0) result)
                    (else (let ((n-pos (from-to pos n)))
                            (rec (sub1 n) (if (is-a-valid-move? n-pos visited)
                                              (cons (list n-pos (simple-binary-converter n)) result)
                                              result))))))))
    (rec 7 '())))

(define (sort-first lst #:proc (proc <) #:key (key identity))
  (letrec ((rec (λ (lst min)
                  (cond
                    ((empty? lst) min)
                    (else (rec (cdr lst) (if (proc (key (car lst)) (key min))
                                             (car lst)
                                             min)))))))
    (rec (cdr lst) (car lst))))

(define (add-valid-moves-count valid-move visited)
  (let ((pos (first valid-move)))
    (letrec ((rec (λ (n acc)
                    (cond ((< n 0) (cons acc valid-move))
                          (else (let ((n-pos (from-to pos n)))
                                  (rec (sub1 n) (if (is-a-valid-move? n-pos visited)
                                                    (add1 acc)
                                                    acc))))))))
      (rec 7 0))))