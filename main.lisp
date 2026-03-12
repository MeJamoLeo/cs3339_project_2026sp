;; adder
(defun adder (a b)
  (+ a b))

;; Read input file
(defun assembler ()
  (with-open-file (input-stream "input")
	(loop for line = (read-line input-stream nil)
		  while line
		  do (format t "~A~%" line))))
