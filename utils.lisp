(in-package :vampyre.utils)

;; Send to utils!
(defun display-pairs (pairs)
  (loop for pair in pairs do
       (format t "~a: ~a~%" (car pair) (cdr pair))))

(defun get-values (alist)
  (loop for i in alist collect (cdr i)))

(defun random-name (len)
  (let ((char-list (string "ABCDEFGHIJKLMNOPQRSTUVWXYZ")))
    (coerce
     (loop for i upto (- len 1) collect
	  (char char-list (random (length char-list))))
'string)))
