(defparameter *input-file*
  (merge-pathnames #p"day3-test-input.txt"
                   (make-pathname :directory (sb-posix:getcwd))))

*input-file*

(defun parse (input)
  (with-open-file (in input)
    (loop :for line := (read-line in nil)
          :while line
          :collect (map 'list #'digit-char-p line))))

(parse *input-file*)

(defun sum-lists (a b)
  (loop for x in a
        for y in b
        collect (+ x y)))

(defun count-set-bits (entries)
  (reduce #'sum-lists
          entries
          :initial-value (make-list (length (first entries)) :initial-element 0)))

(count-set-bits (parse *input-file*))

(defun most-common-value (entries index)
  (if (>= (elt (count-set-bits entries) index) (round (length entries) 2)) 1 0))

(most-common-value (parse *input-file*) 1)


