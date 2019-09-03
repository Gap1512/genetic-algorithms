;;;; simple-example-ltk.asd

(asdf:defsystem #:simple-example
    :description "GUI Application For Simple Example From 'A Survey of Genetic Algorithms' By M. Tomassini"
    :author "Gustavo Alves Pacheco <gap1512@gmail.com>"
    :serial t
    :depends-on (#:ltk)
    :components ((:file "package")
		 (:file "simple-example" :depends-on ("package")))
    :build-operation "asdf:program-op"
    :build-pathname "simple-example"
    :entry-point "simple-example:main")

;;sbcl --eval "(asdf:operate :build-op :simple-example)"
