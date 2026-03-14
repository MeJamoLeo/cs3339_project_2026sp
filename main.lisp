;; adder
(defun adder (a b)
  (+ a b))

;; Read input file
;; TODO: devide this in two functions; one is read assembly, the other is instruction encoder
(defun read-assembly (path)
  (with-open-file (input-stream path)
	(loop for line = (read-line input-stream nil)
		  while line
		  collect (remove #\, line))))

(defun split-by-spaces (str)
  (let ((trimmed (string-trim " " str)))
	(if (string= trimmed "")
		nil
		(let ((pos (position #\Space trimmed)))
		  (if pos
			  (cons (subseq trimmed 0 pos)
					(split-by-spaces (subseq trimmed (1+ pos))))
			  (list trimmed))))))

(defun parse-assembly (path)
  (mapcar #'split-by-spaces (read-assembly path)))

(defparameter *reg* (make-hash-table))
(loop for (name num) in
	  '(($zero 0) ($at 1) ($v0 2) ($v1 3) ($a0 4) ($a1 5) ($a2 6) ($a3 7)
				  ($t0 8) ($t1 9) ($t2 10) ($t3 11) ($t4 12) ($t5 13) ($t6 14) ($t7 15)
				  ($s0 16) ($s1 17) ($s2 18) ($s3 19) ($s4 20) ($s5 21) ($s6 22) ($s7 23)
				  ($t8 24) ($t9 25) ($k0 26) ($k1 27) ($gp 28) ($sp 29) ($fp 30) ($ra 31))
	  do (setf (gethash name *reg*) num))

(format t "~A~%" (hash-table-count *reg*))
(format t "~B~%" (gethash '$zero *reg*))
(format t "~B~%" (gethash '$s3 *reg*))
