(ql:quickload '(split-sequence))

(defparameter *input-file*
  (merge-pathnames #p"day4-input.txt"
                   (make-pathname :directory (sb-posix:getcwd))))

(defun read-numbers (line)
  (mapcar #'parse-integer
          (split-sequence::split-sequence #\, line)))

(defun read-row (row)
  (mapcar #'parse-integer
          (split-sequence::split-sequence #\Space
                                          row
                                          :remove-empty-subseqs t)))

(defun read-board (rows)
  (mapcar #'read-row rows))

(defun empty-string? (s)
  (= 0 (length s)))

(defun read-boards (lines)
  (mapcar #'read-board
          (split-sequence::split-sequence-if #'empty-string?
                                             lines
                                             :remove-empty-subseqs t)))

(defun read-lines (in)
  (loop for line = (read-line in nil)
        while line
        collect line))

(with-open-file (in *input-file*)
  (defparameter *numbers* (read-numbers (read-line in nil)))
  (defparameter *boards* (read-boards (read-lines in))))

(defun complete? (line)
  (every #'null line))

(defun get-columns (board)
  (loop for index from 0 below (length (first board))
        collect (mapcar (lambda (row) (elt row index)) board)))

(defun bingo (rows)
  (or (find-if #'complete? rows)
      (find-if #'complete? (get-columns rows))))

(defun mark (called-number boards)
  (dolist (board boards)
    (dolist (row board)
      (nsubstitute nil called-number row)))
  called-number)

(defun find-winning-board (boards numbers)
  (cond
   ((< (length numbers) 1) (format t "No numbers left~%"))
   (t (let* ((called-number (mark (pop numbers) boards))
             (winning-board (find-if #'bingo boards)))
        (if winning-board
            (values winning-board called-number)
            (find-winning-board boards numbers))))))

(defun board-score (board)
  (reduce #'+
          (mapcan (lambda (row) (remove nil row))
                  board)))

(multiple-value-bind (winning-board last-called-number)
                     (find-winning-board (copy-tree *boards*)
                                         (copy-list *numbers*))
  (let ((score (board-score winning-board)))
    (format t "Winning board score: ~A~%" score)
    (format t "Last called number: ~A~%" last-called-number)
    (format t "Final score: ~A~%" (* score last-called-number))))
