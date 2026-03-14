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

;; rmeove conmma from string
(format t (remove #\, "add $s0, $t0, $t1"))

(position #\Space "add $s0 $t0 $t1" :start 4)
