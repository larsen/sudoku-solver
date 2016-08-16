;;;; sudoku.lisp

(in-package #:sudoku)

(defstruct cell
  "A Sudoku problem is an array of cells.  Each cell contains a
value (most of them will be 'empty at the beginning) and a list of
exclusions: numbers that, depending on the values of other cells in
the board, are not possible values of the cell."
  (value 'empty)
  (exclusions '()))

(defun slurp-file (filename)
  "Given a filename, returns a list of strings, one for each of the
lines in the file."
  (let ((in (open filename)))
    (when in
      (loop
         for line = (read-line in nil)
         while line
         collect line))))

(defun make-cell-from-char (c)
  "Given a char, returns a new cell. The value it's the integer
corresponding to the digit represented by the character, or 'empty
if the char is '.' (representative of the empty cell value)."
  (make-cell :value (if (equal c #\.)
                        'empty
                        (parse-integer
                         (make-string 1 :initial-element c)))))

(defun load-board (filename)
  "Loads a sudoku problem from a file."
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
            do (setf (aref board x y) (make-cell-from-char c))))
    board))

(defun cell-representation (cell)
  "Translates a cell value into the corresponding character. Used for
printing the board."
  (if (equal (cell-value cell) 'empty)
              "."
              (cell-value cell)))

(defun print-cell (cell)
  (format t "~A (~A) "
          (cell-representation cell)
          (length (cell-exclusions cell))))

(defun print-board (board)
  (loop
     for i from 0 to 8
     do (progn (loop
                  for j from 0 to 8
                  do (print-cell (aref board j i)))
               (format t "~%"))))

(defun display-board-tex (board)
  "Display a sudoku problem. It uses /tmp/ directory for storing
intermediate files. It uses TeX and xdvi to produce and display a
graphical representation."
  (let ((filename "/tmp/board"))
    (progn
      (with-open-file (s (concatenate 'string filename ".tex")
                         :direction :output
                         :if-exists :supersede)
        (format s "
$$\\vbox{
    \\offinterlineskip
    \\halign{
        \\strut \\vrule # & \\vrule # & \\vrule # & \\vrule # & \\vrule # & \\vrule # & \\vrule # & \\vrule # & \\vrule # \\vrule \\cr
        \\noalign{\\hrule}
")
        (loop
           for i from 0 to 8
           do (progn (format s "\\noalign{\\hrule}~%")
                     (format s "~{~a~^ & ~} \\cr"
                             (loop
                                for j from 0 to 8
                                collect (cell-representation (aref board j i))))
                     (format s "~%")))
        (format s "\\noalign{\\hrule}
    }
}$$
\\bye
"))
      (external-program:run "/usr/bin/tex"
                            (list "-output-directory" "/tmp" filename)
                            :output *standard-output*)
      (external-program:run "/usr/bin/xdvi"
                            (list "-s" "5" filename)))))

(defun region-boundaries (x y)
  "Given the coordinates of a cell, returns a list representing (x1 x2
y1 y2) the boundaries of the 3x3 region where the cell resides."
  (list (* 3 (truncate (/ x 3)))
        (+ 2 (* 3 (truncate (/ x 3))))
        (* 3 (truncate (/ y 3)))
        (+ 2 (* 3 (truncate (/ y 3))))))

(defun empty-p (value)
  (equal value 'empty))

(defun extend-exclusions (cell board x y)
  "Given a board and a particular cell, returns a list of inferrable
exclusions for the cell."
  (remove-if
   #'empty-p
   (remove-duplicates
    (concatenate 'list
                 ;; Examine column
                 (loop
                    for i from 0 to 8
                    when (not (= i x))
                    collect (cell-value (aref board i y)))
                 ;; Examine row
                 (loop
                    for i from 0 to 8
                    when (not (= i y))
                    collect (cell-value (aref board x i)))
                 ;; Examine region
                 (destructuring-bind
                       (x1 x2 y1 y2) (region-boundaries x y)
                   (loop
                      for i from x1 to x2
                      for j from y1 to y2
                      when (not (and (= i x)
                                     (= j y)))
                      collect (cell-value (aref board i j))))))))

(defun extend-all-exclusions (board)
  "Destructively updates a board, computing exclusions for all the
cells in the board."
  (loop
     for i from 0 to 8
     do (progn (loop
                  for j from 0 to 8
                  do (setf (cell-exclusions (aref board i j))
                           (extend-exclusions (aref board i j) board i j))))))
