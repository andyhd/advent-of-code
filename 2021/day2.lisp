(ql:quickload '(split-sequence))


(defparameter *input-file*
  (merge-pathnames #p"day2-input.txt"
                   (make-pathname :directory (sb-posix:getcwd))))


(defun parse-command (command)
  (destructuring-bind (command arg) command
    `(,(intern (string-upcase command)) ,(parse-integer arg))))


(defun parse (input)
  (with-open-file (in input)
    (loop :for line := (read-line in nil)
          :while line
          :collect (parse-command (split-sequence::split-sequence #\Space line)))))


(defclass submarine ()
  ((pos :initform 0 :accessor pos)
   (depth :initform 0 :accessor depth)
   (aim :initform 0 :accessor aim)))


(defmethod forward ((sub submarine) value)
  (incf (pos sub) value))


(defmethod down ((sub submarine) value)
  (incf (depth sub) value))


(defmethod up ((sub submarine) value)
  (decf (depth sub) value))


(defmethod execute ((sub submarine) commands)
  (dolist (command commands)
    (destructuring-bind (command arg) command
      (funcall command sub arg))))


(defmethod down ((sub submarine) value)
  (incf (aim sub) value))


(defmethod up ((sub submarine) value)
  (decf (aim sub) value))


(defmethod forward ((sub submarine) value)
  (incf (pos sub) value)
  (incf (depth sub) (* (aim sub) value)))


(defparameter *sub* (make-instance 'submarine))


(execute *sub* (parse *input-file*))


(pos *sub*)

(depth *sub*)

(* (pos *sub*) (depth *sub*))
