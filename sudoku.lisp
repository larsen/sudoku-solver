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

(defun extend-exclusions (board x y)
  "Given a board and a particular cell, returns a list of inferrable
exclusions for the cell."
  (remove-if
   #'empty-p
   (remove-duplicates
    (concatenate 'list
                 ;; Existing exclusions
                 (cell-exclusions (aref board x y))
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
     do (loop
           for j from 0 to 8
           do (setf (cell-exclusions (aref board i j))
                    (extend-exclusions board i j)))))

(defun number-excluded-p (n cell)
  (member n (cell-exclusions cell)))

(defun n-6th (n)
  (let ((cell-width (/ *window-width* 9)))
    (* n (truncate (/ cell-width 6)))))

(defun render-exclusions (cell x y)
  (let ((cell-width (/ *window-width* 9))
        (cell-height (/ *window-height* 9))
        (exclusions (sort (cell-exclusions cell) #'<)))
    (loop
       for n in exclusions
       for displacements in `((,(n-6th 1) ,(n-6th 1))
                              (,(n-6th 3) ,(n-6th 1))
                              (,(n-6th 5) ,(n-6th 1))
                              (,(n-6th 1) ,(n-6th 3))
                              (,(n-6th 5) ,(n-6th 3))
                              (,(n-6th 1) ,(n-6th 5))
                              (,(n-6th 3) ,(n-6th 5))
                              (,(n-6th 5) ,(n-6th 5)))
       do (sdl:draw-surface-at-*
           (sdl:render-string-shaded
            (format nil "~a" n)
            sdl:*black* sdl:*white*
            :cache t)
           (+ (* cell-width x) (car displacements))
           (+ (* cell-height y) (cadr displacements))))))

(defun render-cell (cell x y)
  (let ((cell-width (/ *window-width* 9))
        (cell-height (/ *window-height* 9))
        (cell-background (if (equal (cell-value cell) 'empty)
                             (if (= (length (cell-exclusions cell)) 8)
                                 sdl:*green*
                                 sdl:*white*)
                             sdl:*yellow*)))
    (fill-surface cell-background
     :template (rectangle :x (* cell-width x)
                          :y (* cell-height y)
                          :h cell-height
                          :w cell-width))
    (sdl:draw-surface-at-*
     (sdl:render-string-shaded
      (format nil "~a" (cell-representation cell))
      sdl:*red* cell-background
      :cache t)
     (+ (* cell-width x) (n-6th 3))
     (+ (* cell-height y) (n-6th 3)))
    (if (empty-p (cell-value cell))
        (render-exclusions cell x y))))

(defun render-grid ()
  (let ((cell-width (/ *window-width* 9))
        (cell-height (/ *window-height* 9)))
    (loop
       for i from 0 to 8
       do (draw-line (sdl:point :x (* cell-width i)
                                :y 0)
                     (sdl:point :x (* cell-width i)
                                :y *window-height*)
                     :color sdl:*black*))
    (loop
       for i from 0 to 8
       do (draw-line (sdl:point :x 0
                                :y (* cell-height i))
                     (sdl:point :x *window-width*
                                :y (* cell-height i))
                     :color sdl:*black*))))

(defun render-board (board)
  (loop
     for i from 0 to 8
     do (loop
           for j from 0 to 8
           do (render-cell (aref board i j) i j)))
  (render-grid)
  (update-display))

(defparameter *window-width* 900)
(defparameter *window-height* 900)
(defparameter *window* nil)

(defun display-board (board)
  (with-init ()
    (setf *window* (window *window-width* *window-height*))
    (sdl:initialise-default-font sdl:*font-9x18*)
    (clear-display sdl:*white*)
    (render-board board)
    (update-display)
    (with-events ()
      (:quit-event () t)
      (:key-down-event (:key key)
                       (case key
                         (:sdl-key-escape (push-quit-event)))))))
