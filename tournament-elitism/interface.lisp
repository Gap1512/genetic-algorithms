;;;; interface.lisp

(in-package #:tournament-elitism)

(defparameter *canvas* 512)
(defparameter *offset* (/ *canvas* 2))
(defparameter *coef* 70)
(defparameter *p-coef* 8)
(defvar *selection-mode* :roulette)
(defvar *elitism* nil)
(defparameter *rect-size* 2)
(defparameter *rect-color* :red)

;;;; Nothing -> Nothing
;;;; (render)
(defun render ()
  "Creates the interface and waits for the user input to start the GA"
  (with-ltk ()
    (conf-tk)
    (let* ((main-frame (make-instance 'frame  :pack
				      '(:fill :both :expand :both)))
	   (info-frame (make-instance 'frame :master main-frame :pack
				      '(:side :left :fill :both :expand :both)))
	   (view-frame (make-instance 'frame :master main-frame :pack
				      '(:side :right :fill :both :expand :both)))

	   (selection-frame (make-instance 'frame :master info-frame))
	   (elitism-frame (make-instance 'frame :master info-frame))
	   (resolution-frame (make-instance 'frame :master info-frame))
	   (range-min-frame (make-instance 'frame :master info-frame))
	   (range-max-frame (make-instance 'frame :master info-frame))
	   (pc-frame (make-instance 'frame :master info-frame))
	   (pm-frame (make-instance 'frame :master info-frame))
	   (mgen-frame (make-instance 'frame :master info-frame))
	   (nind-frame (make-instance 'frame :master info-frame))
	   (x-frame (make-instance 'frame :master info-frame))
	   (y-frame (make-instance 'frame :master info-frame))
	   (agen-frame (make-instance 'frame :master info-frame))

	   (selection-label (make-instance 'label :master selection-frame :text "SELECTION"))
	   (resolution-label (make-instance 'label :master resolution-frame :text "RESOLUTION"))
	   (range-min-label (make-instance 'label :master range-min-frame :text "RANGE MIN"))
	   (range-max-label (make-instance 'label :master range-max-frame :text "RANGE MAX"))
	   (pc-label (make-instance 'label :master pc-frame :text "PC"))
	   (pm-label (make-instance 'label :master pm-frame :text "PM"))
	   (mgen-label (make-instance 'label :master mgen-frame :text "LAST GENERATION"))
	   (nind-label (make-instance 'label :master nind-frame :text "NUMBER OF INDIVIDUALS"))
	   (x-label (make-instance 'label :master x-frame :text "X"))
	   (y-label (make-instance 'label :master y-frame :text "F(X)"))
	   (agen-label (make-instance 'label :master agen-frame :text "ACTUAL GENERATION"))

	   (tournament-k-entry (make-instance 'entry :master selection-frame :text 4))
	   (elitism-k-entry (make-instance 'entry :master elitism-frame :text 2))
	   (resolution-entry (make-instance 'entry :master resolution-frame :text 10))
	   (range-min-entry (make-instance 'entry :master range-min-frame :text 0))
	   (range-max-entry (make-instance 'entry :master range-max-frame :text 3))
	   (pc-entry (make-instance 'entry :master pc-frame :text 0.6))
	   (pm-entry (make-instance 'entry :master pm-frame :text 0.01))
	   (mgen-entry (make-instance 'entry :master mgen-frame :text 70))
	   (nind-entry (make-instance 'entry :master nind-frame :text 50))
	   (x-entry (make-instance 'entry :master x-frame :state :disable))
	   (y-entry (make-instance 'entry :master y-frame :state :disable))
	   (agen-entry (make-instance 'entry :master agen-frame :state :disable))

	   (canvas (make-instance 'canvas :master view-frame :background :white
				  :height *canvas* :width *canvas*
				  :pack '(:side :left :fill :both :expand :both)))

	   (performance-canvas (make-instance 'canvas :master view-frame :background :white
					      :height *canvas* :width *canvas*
					      :pack '(:side :left :fill :both :expand :both)))

	   (roulette-check (make-instance 'check-button :master selection-frame :text "ROULETTE" :command #'(lambda (x) (setf *selection-mode* (if (zerop x) :tournament :roulette)))))
	   (tournament-check (make-instance 'check-button :master selection-frame :text "TOURNAMENT" :command #'(lambda (x) (setf *selection-mode* (if (zerop x) :roulette :tournament)))))
	   (elitism-check (make-instance 'check-button :master elitism-frame :text "ELITISM" :command #'(lambda (x) (setf *elitism* (not (zerop x))))))
	   
	   (start-button (make-instance 'button :master view-frame :text "START"
					:command #'(lambda ()
						     (let ((mgen (parse-integer (text mgen-entry)))
							   (nind (parse-integer (text nind-entry)))
							   (tournament-k (parse-integer (text tournament-k-entry)))
							   (elitism-k (parse-integer (text elitism-k-entry)))
							   (resolution (parse-integer (text resolution-entry)))
							   (range-min (parse-integer (text range-min-entry)))
							   (range-max (parse-integer (text range-max-entry))))
						       (clear canvas)
						       (plot *fn* canvas range-min range-max)
						       (prepare-perfomance-plot performance-canvas)
						       (handler-case
							   (genetic-algorithm canvas performance-canvas x-entry y-entry agen-entry
									      :resolution resolution :range-min range-min
									      :range-max range-max :mgen mgen :nind nind
									      :selection *selection-mode* :tournament-k tournament-k
									      :elitism *elitism* :elitism-k elitism-k
									      :pc (with-input-from-string
										      (pc (text pc-entry))
										    (read pc))
									      :pm (with-input-from-string
										      (pm (text pm-entry))
										    (read pm)))
							 
							 (error () (format t "Error"))))))))

      (pack-items selection-frame elitism-frame resolution-frame range-min-frame range-max-frame
		  pc-frame pm-frame mgen-frame nind-frame x-frame y-frame agen-frame
		  selection-label resolution-label range-min-label range-max-label pc-label
		  pm-label mgen-label nind-label x-label y-label agen-label
		  resolution-entry range-min-entry range-max-entry
		  pc-entry pm-entry mgen-entry nind-entry x-entry y-entry agen-entry
		  roulette-check tournament-check tournament-k-entry elitism-check elitism-k-entry start-button)
      
      (plot *fn* canvas (parse-integer (text range-min-entry))
	    (parse-integer (text range-max-entry))))))

(defun conf-tk ()
  (wm-title *tk* "GENETIC ALGORITHM"))

(defun pack-items (&rest items)
  (loop for item in items do
       (pack item
	     :side :top
	     :anchor :w
	     :fill :both
	     :expand :both)))

(defun plot (fn canvas range-min range-max)
  (create-line* canvas 0 (/ (window-width canvas) 2) (window-width canvas) (/ (window-width canvas) 2))
  (create-line canvas (get-points fn :min range-min :max range-max :step 0.001 :wd (window-width canvas))))

(defun get-points (fn &key min max step (coef *coef*) (wd *canvas*))
  (do ((points nil) (i min (+ i step)))
      ((>= i max) points)
    (progn
      (push (+ *offset* (* -1 (funcall fn i) coef)) points)
      (push (/ (* i wd) max) points))))

;;;; function canvas population points -> list (points)
;;;; (plot-points fn canvas population)
;;;; A population is a list of lists (string number)
(defun plot-points (fn canvas population points)
  "Plot a list of points from a population on a canvas"
  (mapcar #'(lambda (x) (itemdelete canvas x)) points)
  (let (points)
    (dolist (item population (nreverse points))
      (push (plot-point fn canvas (second item) :wd (window-width canvas)) points))))

;;;; function canvas coordinate -> point
;;;; (plot-point fn canvas coord)
(defun plot-point (fn canvas coord &key (coef *coef*) (x-max 3) (wd *canvas*))
  "Plot a point on a canvas"
  (let* ((x-coord (/ (* coord wd) x-max))
	 (fx (+ *offset* (* -1 (funcall fn coord) coef)))
	 (point (create-rectangle canvas
				  (- x-coord *rect-size*)
				  (- fx *rect-size*)
				  (+ x-coord *rect-size*)
				  (+ fx *rect-size*))))
    (itemconfigure canvas point :fill *rect-color*)
    point))

(defun prepare-perfomance-plot (performance-canvas)
  (clear performance-canvas)
  (create-line* performance-canvas 0 (/ (window-width performance-canvas) 2) (window-width performance-canvas) (/ (window-width performance-canvas) 2)))

(defun plot-performance-point (performance-canvas x y &key (coef *coef*) (x-max 70) (color :red))
  "Plot a point on a canvas"
  (let* ((x-coord (/ (* x (window-width performance-canvas)) x-max))
	 (y-coord (+ *offset* (* -1 coef y)))
	 (point (create-rectangle performance-canvas
				 (- x-coord *rect-size*)
				 (- y-coord *rect-size*)
				 (+ x-coord *rect-size*)
				 (+ y-coord *rect-size*))))
	(itemconfigure performance-canvas point :fill color)))
