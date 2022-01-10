(ql:quickload '(folio2))


(defparameter *input-file*
  (merge-pathnames #p"day1-input.txt"
                   (make-pathname :directory (sb-posix:getcwd))))


(defparameter *readings*
  (with-open-file (input *input-file*)
    (folio2::image #'parse-integer (folio2::lines input))))


(defun pairs (series)
  (folio2::take-while #'folio2::second
    (folio2::take-by 2 1 (folio2::add-first nil series))))


(defun increasing? (pair)
  (let ((prev (folio2::first pair))
        (curr (folio2::second pair)))
    (and
      (not (null prev))
      (> curr prev))))


(defun count-series (series)
  (let ((head (folio2::head series))
        (tail (folio2::tail series)))
    (cond
      ((null head) 0)
      (t (+ 1 (count-series tail))))))


(defun count-increasing (series)
  (count-series (folio2::filter #'increasing? (pairs series))))


(count-increasing *readings*)


(defun sliding-windows (series)
  (folio2::take-while (lambda (window) (folio2::element window 2))
    (folio2::take-by 3 1 series)))


(defun sum-series (series)
  (let ((head (folio2::head series))
        (tail (folio2::tail series)))
    (cond
      ((null head) 0)
      ((null (folio2::head tail)) head)
      (t (+ head (sum-series tail))))))


(count-increasing (folio2::image #'sum-series (sliding-windows *readings*)))
