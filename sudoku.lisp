;;;; sudoku.lisp

(in-package #:sudoku)

(defstruct cell
  (value 'empty)
  (exclusions '()))

(defun slurp-file (filename)
  (let ((in (open filename)))
    (when in
      (loop
         for line = (read-line in nil)
         while line
         collect line))))

(defun load-board (filename)
  (let ((board (make-array '(9 9) :initial-element 'empty))
        (lines (remove-if
                (lambda (l) (equal l ""))
                (slurp-file filename))))
    (loop
       for line in lines
       for y upfrom 0 do
         (loop
            for x upfrom 0 
            for c across (remove #\Space line)
            do (setf
                (aref board x y)
                (make-cell :value 
                           (if (equal c #\.)
                               'empty
                               (parse-integer (make-string 1 :initial-element c)))))))
    board))
