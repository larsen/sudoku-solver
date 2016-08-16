;;;; sudoku.asd

(asdf:defsystem #:sudoku
  :description "Describe sudoku here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :serial t
  :depends-on (#:lispbuilder-sdl #:external-program)
  :components ((:file "package")
               (:file "sudoku")))

