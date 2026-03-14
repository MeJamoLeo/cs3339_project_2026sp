;; adder
(defun adder (a b)
  (+ a b))

;; Read input file
;; TODO: devide this in two functions; one is read assembly, the other is instruction encoder
(defun read-assembly (path)
  (with-open-file (input-stream path)
	(loop for line = (read-line input-stream nil)
		  while line
		  collect line)))

(map nil #'print (read-assembly "./input"))
