;; adder
(defun adder (a b)
  (+ a b))

;; Read input file
;; TODO: devide this in two functions; one is read assembly, the other is instruction encoder
(defun assembler ()
  (with-open-file (input-stream "input")
	(loop for line = (read-line input-stream nil)
		  while line
		  do (format t "~A~%" line))))
