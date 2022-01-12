(defparameter *input-file*
  (merge-pathnames #p"day3-input.txt"
                   (make-pathname :directory (sb-posix:getcwd))))

(defun parse (input)
  (with-open-file (in input)
    (loop for line = (read-line in nil)
          while line
          collect (map 'list #'digit-char-p line))))

(defparameter *log* (parse *input-file*))

(defun sum-lists (a b)
  (loop for x in a
        for y in b
        collect (+ x y)))

(defun count-set-bits (entries)
  (reduce #'sum-lists
          entries
          :initial-value (make-list (length (first entries)) :initial-element 0)))

(defun most-common-value (entries index)
  (if (>= (elt (count-set-bits entries) index)
          (ceiling (length entries) 2))
      1
      0))

(defun least-common-value (entries index)
  (- 1 (most-common-value entries index)))

(defun gamma-rate (entries)
  (loop for index from 0 to (1- (length (first entries)))
        collect (most-common-value entries index)))

(defun bit-list-to-int (value)
  (reduce (lambda (a b) (+ (* 2 a) b))
          value
          :initial-value 0))

(defun epsilon-rate (entries)
  (loop for index from 0 to (1- (length (first entries)))
        collect (least-common-value entries index)))

(format t "power consumption = ~A" (* (bit-list-to-int (gamma-rate *log*))
                                      (bit-list-to-int (epsilon-rate *log*))))

(defun range (max &key (min 0) (step 1))
  (loop for i from min below max by step
        collect i))

(defun find-by-bit-criteria (entries criteria)
  (first
    (reduce
      (lambda (candidates index)
        (if (>= 1 (length candidates))
            candidates
            (let ((bit-criteria (funcall (symbol-function criteria) candidates index)))
              (remove-if-not
                (lambda (candidate)
                  (= (elt candidate index) bit-criteria))
                candidates))))
      (range (length (first entries)))
      :initial-value (copy-list entries))))

(defun oxygen-generator-rating (entries)
  (find-by-bit-criteria entries 'most-common-value))

(defun co2-scrubber-rating (entries)
  (find-by-bit-criteria entries 'least-common-value))

(format t "life support rating = ~A~%"
        (* (bit-list-to-int (oxygen-generator-rating *log*))
           (bit-list-to-int (co2-scrubber-rating *log*))))
