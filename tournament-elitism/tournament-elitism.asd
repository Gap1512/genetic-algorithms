;;;; tournament-elitism.asd

(asdf:defsystem #:tournament-elitism
    :description "GUI Application For Simple Example From 'A Survey of Genetic Algorithms' By M. Tomassini"
    :author "Gustavo Alves Pacheco <gap1512@gmail.com>"
    :serial t
    :depends-on (#:ltk)
    :components ((:file "package")
		 (:file "shared" :depends-on ("package"))
		 (:file "interface" :depends-on ("package" "shared"))
		 (:file "tournament-elitism" :depends-on ("package" "interface" "shared")))
    :build-operation "asdf:program-op"
    :build-pathname "tournament-elitism"
    :entry-point "tournament-elitism:main")

;;sbcl --eval "(asdf:operate :build-op :tournament-elitism)"
