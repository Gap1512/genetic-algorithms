;;;; simple-example.lisp

(in-package #:simple-example)

(defparameter *rnd-precision* 1000000)
(defvar *rnd-fn* #'(lambda () (/ (random *rnd-precision*) *rnd-precision*)))
(defparameter *fn* #'(lambda (x) (+ 480 (* -1 (abs (* x (sin (sqrt (abs x)))))))))
(defparameter *rect-size* 2)
(defparameter *rect-color* :red)

;;;; Nothing -> Nothing
;;;; (main)
(defun main ()
  "Beginning point to the GA"
  (render))

;;;; Nothing -> Nothing
;;;; (render)
(defun render ()
  "Creates the interface and waits for the user input to start the GA"
  (with-ltk ()
    (let* ((main-frame (make-instance 'frame))
	   (title-frame (make-instance 'frame :master main-frame))
	   (info-frame (make-instance 'frame :master main-frame))
	   (view-frame (make-instance 'frame :master main-frame))

	   (screen-title (make-instance 'label :master title-frame))

	   (pc-frame (make-instance 'frame :master info-frame))
	   (pm-frame (make-instance 'frame :master info-frame))
	   (mgen-frame (make-instance 'frame :master info-frame))
	   (nind-frame (make-instance 'frame :master info-frame))
	   (x-frame (make-instance 'frame :master info-frame))
	   (y-frame (make-instance 'frame :master info-frame))
	   (agen-frame (make-instance 'frame :master info-frame))

	   (pc-label (make-instance 'label :master pc-frame))
	   (pm-label (make-instance 'label :master pm-frame))
	   (mgen-label (make-instance 'label :master mgen-frame))
	   (nind-label (make-instance 'label :master nind-frame))
	   (x-label (make-instance 'label :master x-frame))
	   (y-label (make-instance 'label :master y-frame))
	   (agen-label (make-instance 'label :master agen-frame))

	   (pc-entry (make-instance 'entry :master pc-frame))
	   (pm-entry (make-instance 'entry :master pm-frame))
	   (mgen-entry (make-instance 'entry :master mgen-frame))
	   (nind-entry (make-instance 'entry :master nind-frame))
	   (x-entry (make-instance 'entry :master x-frame))
	   (y-entry (make-instance 'entry :master y-frame))
	   (agen-entry (make-instance 'entry :master agen-frame))

	   (canvas (make-instance 'canvas :master view-frame))
	   (start-button (make-instance 'button :master view-frame :text "START"
					:command #'(lambda ()
						     (let ((mgen (text mgen-entry))
							   (nind (text nind-entry)))
						       (clear canvas)
						       (plot *fn* canvas)
					;(handler-case
						       (genetic-algorithm
							*fn*
							(with-input-from-string
							    (pc (text pc-entry))
							  (read pc))
							(with-input-from-string
							    (pm (text pm-entry))
							  (read pm))
							(parse-integer mgen)
							(parse-integer nind)
						        canvas x-entry y-entry agen-entry))))))
					;(error () (format t "Error"))))))))
      
      (pack main-frame :expand t :fill :both)
      (pack title-frame)
      (pack info-frame :side :left :anchor :w)
      (pack view-frame :side :right :anchor :se)

      (wm-title *tk* "GENETIC ALGORITHM")

      (start-label screen-title "SIMPLE EXAMPLE")
      (start-label pc-label "PC")
      (start-label pm-label "PM")
      (start-label mgen-label "LAST GENERATION")
      (start-label nind-label "NUMBER OF INDIVIDUALS")
      (start-label x-label "X")
      (start-label y-label "F(X)")
      (start-label agen-label "ACTUAL GENERATION")

      (pack-items pc-frame pm-frame mgen-frame nind-frame x-frame y-frame agen-frame
		  pc-entry pm-entry mgen-entry nind-entry x-entry y-entry agen-entry)

      (setf (text pc-entry) "0.6")
      (setf (text pm-entry) "0.01")
      (setf (text mgen-entry) "70")
      (setf (text nind-entry) "50")
      (configure x-entry :state :disable)
      (configure y-entry :state :disable)
      (configure agen-entry :state :disable)

      (pack (canvas canvas))
      (configure canvas :background :white :width 512 :height 512)
      (plot *fn* canvas)
      (pack start-button :fill :x))))

(defun start-label (label text)
  (pack label :anchor :w)
  (setf (text label) text))

(defun pack-items (&rest items)
  (loop for item in items do
       (pack item :anchor :w)))

(defun plot (fn canvas)
  (let* ((canvas-width (screen-width canvas))
	 (fn-v (create-line canvas (get-points fn :min 0 :max canvas-width :step 0.1))))
    fn-v))

(defun get-points (fn &key min max step)
    (do ((points nil) (i min (+ i step)))
	((>= i max) points)
      (progn
	(push (- 512 (funcall fn i)) points)
	(push i points))))

;;;; function crossover-probability mutation-probability last-generation number-of-individuals -> minimal-point
;;;; (genetic-algorithm fn pc pm mgen nind)
(defun genetic-algorithm (fn pc pm mgen nind canvas x-entry y-entry agen-entry)
  "Minimize the given function"
  (do* ((agen 0 (1+ agen))
	(population (generate-initial-population nind)
		    (mutation
		     pm (crossover
			 pc (select-individuals nind (cumulative (probability fitness-population))))))
	(fitness-population (fitness fn population :inverse t) (fitness fn population :inverse t))
	(points (plot-points fn canvas population nil) (plot-points fn canvas population points))
	(best (best fitness-population) (best fitness-population)))
       ((= agen mgen) best)
    (setf (text x-entry) (float best)
	  (text y-entry) (funcall fn best)
	  (text agen-entry) agen)))

;;;; number-of-individuals -> population
;;;; (generate-initial-population nind)
(defun generate-initial-population (nind)
  "Generates the initial population"
  (let (population)
    (dotimes (ind nind (nreverse population))
      (push (represent-individual (/ (random 1021) 2.0)) population))))

;;;; number -> list (string number)
;;;; (represent-individual number)
(defun represent-individual (number)
  "Represent an individual as a 10 bits string, from 0 to 511.5"
  (let* ((string (make-string 10 :initial-element #\0))
	 (convertion (write-to-string (ceiling (* 2 number)) :base 2))
	 (lt (length convertion)))
    (list (concatenate 'string (subseq string 0 (- 10 lt)) convertion) number)))

;;;; function canvas population points -> list (points)
;;;; (plot-points fn canvas population)
;;;; A population is a list of lists (string number)
(defun plot-points (fn canvas population points)
  "Plot a list of points from a population on a canvas"
  (mapcar #'(lambda (x) (itemdelete canvas x)) points)
  (let (points)
    (dolist (item population (nreverse points))
      (push (plot-point fn canvas (second item)) points))))

;;;; function canvas coordinate -> point
;;;; (plot-point fn canvas coord)
(defun plot-point (fn canvas coord)
  "Plot a point on a canvas"
  (let* ((fx (- 512 (funcall fn coord)))
	 (point (create-rectangle canvas
				  (- coord *rect-size*)
				  (- fx *rect-size*)
				  (+ coord *rect-size*)
				  (+ fx *rect-size*))))
    (itemconfigure canvas point :fill *rect-color*)
    point))

;;;; function population -> fitness-population
;;;; (fitness function population)
;;;; A population is a list of lists (string number)
;;;; A fitness-population is a list of lists (string number fi)
(defun fitness (fn population &key (inverse nil))
  "Associate a fitness value to each individual of a given population"
  (mapcar #'(lambda (ind)
	      (let* ((value (second ind))
		     (fx (funcall fn value)))
		(list (first ind)
		      value
		      (if inverse (/ 1 fx) fx))))
	  population))

;;;; fitness-population -> fitness-probability-population
;;;; (probability f-population)
;;;; A fitness-population is a list of lists (string number fi)
;;;; A fitness-probability-population is a list of lists (string number pi)
(defun probability (f-population)
  "Associate a probability value according to the sum of all individuals from a population"
  (let ((sum (reduce #'+ f-population :key #'third)))
    (mapcar #'(lambda (ind)
	        (list (first ind)
		      (second ind)
		      (/ (third ind) sum)))
	    f-population)))

;;;; fitness-probability-population -> fitness-cumulative-probability-population
;;;; (cumulative p-population)
;;;; A fitness-probability-population is a list of lists (string number pi)
;;;; A fitness-cumulative-probability-population is a list of lists (string number ci)
(defun cumulative (p-population)
  "Associate a cumulative value to each individual of a probability population"
  (let ((acc 0))
    (mapcar #'(lambda (ind)
		(let ((aux acc))
		  (setf acc (+ acc (third ind)))
		  (list (first ind)
			(second ind)
		        aux)))
		p-population)))

;;;; number-of-individuals fitness-cumulative-probability-population -> population
;;;; (select-individuals nind c-population)
;;;; A fitness-cumulative-probability-population is a list of lists (string number ci)
;;;; A population is a list of lists (string number)
(defun select-individuals (nind c-population)
  "Select best individuals according to the cumulative probability given to each one"
  (let (result)
    (dotimes (i nind (nreverse result))
      (let ((r (funcall *rnd-fn*)))
	(push (chosen-one (cdr c-population) (car c-population) r) result)))))

;;;; fitness-cumulative-probability-population individual number -> selected-individual
;;;; (chosen-one c-population ind r)
;;;; A fitness-cumulative-probability-population is a list of lists (string number ci)
;;;; An individual is a list (string number ci)
;;;; A selected-individual is a list (string number)
(defun chosen-one (c-population ind r)
  "Returns the chosen individual, according to the random number input"
  (let* ((new-ind (car c-population))
	 (si (first new-ind))
	 (vi (second new-ind))
	 (ci (third new-ind))
	 (si-1 (first ind))
	 (vi-1 (second ind))
	 (ci-1 (third ind)))
  (cond
    ((or (not c-population) (> ci r)) (list si-1 vi-1))
    ((and (< ci-1 r) (<= r ci)) (list si vi))
    (t (chosen-one (cdr c-population) new-ind r)))))

;;;; crossover-probability population -> population
;;;; (crossover pc population)
;;;; A population is a list of lists (string number)
(defun crossover (pc population)
  "Creates a new population, after apply the crossover operation"
  (let ((lt (length population)))
    (dotimes (i lt population)
      (let* ((nth1 (random lt))
	     (nth2 (random lt))
	     (i1 (first (nth nth1 population)))
	     (i2 (first (nth nth2 population)))
	     (rc (funcall *rnd-fn*))
	     (crossoverp (<= rc pc))
	     (index-c (random 10))
	     (ci1 (concatenate 'string (subseq i1 0 index-c) (subseq i2 index-c 10)))
	     (ci2 (concatenate 'string (subseq i2 0 index-c) (subseq i1 index-c 10))))
	(if crossoverp (setf (nth nth1 population) (list ci1 (value-from-string ci1))
			     (nth nth2 population) (list ci2 (value-from-string ci2))))))))

;;;; mutation-probability population -> population
;;;; (mutation pm population)
;;;; A population is a list of lists (string number)
(defun mutation (pm population)
  "Creates a new population, after apply the mutation operation"
  (mapcar #'(lambda (x) (let* ((m-string (map 'string
					 #'(lambda (y)
					     (let ((rp (funcall *rnd-fn*)))
					       (if (<= rp pm) (if (eql y #\0) #\1 #\0) y)))
					 (first x)))
			  (m-value (value-from-string m-string)))
		     (list m-string m-value)))
	  population))

;;;; string -> number
;;;; (value-from-string string)
(defun value-from-string (string)
  "Return the corresponding value to the 10 bits string"
  (/ (parse-integer string :radix 2) 2))

;;;; population -> number
;;;; (best population)
;;;; A population is a list of lists (string number)
(defun best (population)
  "Returns the minimal value of a population"
  (second (first (sort (copy-seq population) #'< :key #'third))))
