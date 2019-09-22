;;;; tournament-elitism.lisp

(in-package #:tournament-elitism)

;;;; function crossover-probability mutation-probability last-generation number-of-individuals -> minimal-point
;;;; (genetic-algorithm fn pc pm mgen nind)
(defun genetic-algorithm (canvas performance-canvas x-entry y-entry agen-entry  &key
							       (fn *fn*)
							       (selection :roulette)
							       (tournament-k 4)
							       (resolution 10)
							       (elitism nil)
							       (elitism-k 2)
							       (range-min 0)
							       (range-max 512)
							       (pc 0.6)
							       (pm 0.01)
							       (mgen 70)
							       (nind 50))
  "Minimize the given function"
  (let* ((max-value (expt 2 resolution))
	 (range (- range-max range-min))
	 (coef (/ max-value range)))
    
    (do* ((agen 0 (1+ agen))
	  (population (generate-initial-population nind range-min max-value resolution coef)
		      (update-population nind pc pm fitness-population resolution coef selection tournament-k elitism elitism-k))
	  (fitness-population (fitness fn population)
			      (fitness fn population))
	  (points (plot-points fn canvas population nil)
		  (plot-points fn canvas population points))
	  (best (best fitness-population)
		(best fitness-population)))
	 ((= agen mgen) best)
      (plot-performance-point performance-canvas agen (funcall fn best))
      (plot-performance-point performance-canvas agen (/ (reduce #'+ fitness-population :key #'(lambda (x) (- (third x) 4))) nind) :color :blue)
      (setf (text x-entry) (float best)
	    (text y-entry) (funcall fn best)
	    (text agen-entry) agen))))

;;;; number-of-individuals -> population
;;;; (generate-initial-population nind)
(defun generate-initial-population (nind range-min max-value resolution precision)
  "Generates the initial population"
  (let (population)
    (dotimes (ind nind (nreverse population))
      (let ((value (+ range-min (/ (random max-value) precision))))
	(push (represent-individual value resolution precision) population)))))

;;;; number -> list (string number)
;;;; (represent-individual number)
(defun represent-individual (number resolution precision)
  "Represent an individual as a 10 bits string, from 0 to 511.5"
  (let* ((string (make-string resolution :initial-element #\0))
	 (convertion (write-to-string (round (* number precision)) :base 2))
	 (lt (length convertion)))
    (list (concatenate 'string (subseq string 0 (- resolution lt)) convertion) number)))

;;;; function population -> fitness-population
;;;; (fitness function population)
;;;; A population is a list of lists (string number)
;;;; A fitness-population is a list of lists (string number fi)
(defun fitness (fn population)
  "Associate a fitness value to each individual of a given population"
  (mapcar #'(lambda (ind)
	      (let* ((value (second ind)))
		(list (first ind)
		      value
		      (+ 4 (funcall fn value)))))
	  population))

;;;; fitness-probability-population -> fitness-cumulative-probability-population
;;;; (cumulative p-population)
;;;; A fitness-probability-population is a list of lists (string number pi)
;;;; A fitness-cumulative-probability-population is a list of lists (string number ci)
(defun cumulative (population)
  "Associate a cumulative value to each individual of a probability population"
  (let ((sum (reduce #'+ population :key #'third))
	(acc 0))
    (mapcar #'(lambda (ind)
		(let ((aux acc))
		  (setf acc (+ acc (/ (third ind) sum)))
		  (list (first ind)
			(second ind)
			aux acc)))
	    population)))

;;;; number-of-individuals fitness-cumulative-probability-population -> population
;;;; (select-individuals nind c-population)
;;;; A fitness-cumulative-probability-population is a list of lists (string number ci)
;;;; A population is a list of lists (string number)
(defun update-population (nind pc pm population resolution precision selection tournament-k elitism elitism-k)
  "Select best individuals according to the cumulative probability given to each one"
  (let ((c-population (if (eql selection :roulette) (cumulative population) population)) 
	(result (if elitism (select-best elitism-k population) nil)))
    
    (dotimes (i (/ (if elitism (- nind elitism-k) nind) 2) (nreverse result))
      (let* ((c1 (chosen-one c-population selection tournament-k))
	     (c2 (chosen-one c-population selection tournament-k))
	     
	     (cbin1 (first (copy-seq c1)))
	     (cbin2 (first (copy-seq c2))))
	
	(if (<= (funcall *rnd-fn*) pc)
	    
	    (let* ((index-c (random resolution))
		   (cc1 (mutation (concatenate 'string
					       (subseq cbin1 0 index-c)
					       (subseq cbin2 index-c resolution))
				  pm))
		   (cc2 (mutation (concatenate 'string
					       (subseq cbin2 0 index-c)
					       (subseq cbin1 index-c resolution))
				  pm)))
	      (push (list cc1 (value-from-string cc1 precision)) result)
	      (push (list cc2 (value-from-string cc2 precision)) result))
	    
	    (let ((cm1 (mutation cbin1 pm))
		  (cm2 (mutation cbin2 pm)))
	      (push (list cm1 (value-from-string cm1 precision)) result)
	      (push (list cm2 (value-from-string cm2 precision)) result)))))))

(defun select-best (n population)
  (mapcar #'(lambda (x) (list (first x) (second x)))
	  (subseq (sort (copy-seq population) #'> :key #'third) 0 n)))

;;;; fitness-cumulative-probability-population individual number -> selected-individual
;;;; (chosen-one c-population ind r)
;;;; A fitness-cumulative-probability-population is a list of lists (string number ci)
;;;; An individual is a list (string number ci)
;;;; A selected-individual is a list (string number)
(defun chosen-one (c-population selection-mode tournament-k)
  (case selection-mode
    (:roulette
     (let* ((rnd (funcall *rnd-fn*))
	    (chosen (find-if #'(lambda (x)
				 (and (< (third x) rnd) (< rnd (fourth x))))
			     c-population)))
       (list (first chosen) (second chosen))))
    (:tournament
     (car (sort (select-random tournament-k c-population) #'> :key #'third)))))

(defun select-random (k population)
  (let ((l (length population))
	result)
    (dotimes (i k (nreverse result))
      (push (nth (random l) population) result))))

;;;; mutation-probability population -> population
;;;; (mutation pm population)
;;;; A population is a list of lists (string number)
(defun mutation (individual pm)
  "Creates a new population, after apply the mutation operation"
  (map 'string
       #'(lambda (y)
	   (let ((rp (funcall *rnd-fn*)))
	     (if (<= rp pm) (if (eql y #\0) #\1 #\0) y)))
       individual))

;;;; string -> number
;;;; (value-from-string string)
(defun value-from-string (string precision)
  "Return the corresponding value to the 10 bits string"
  (/ (parse-integer string :radix 2) precision))

;;;; population -> number
;;;; (best population)
;;;; A population is a list of lists (string number)
(defun best (population)
  "Returns the minimal value of a population"
  (second (first (sort (copy-seq population) #'> :key #'third))))
