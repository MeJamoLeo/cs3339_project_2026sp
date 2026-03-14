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
(setf (gethash '$zero *reg*) 0)
(setf (gethash '$at *reg*) 1)
(setf (gethash '$v0 *reg*) 2)
(setf (gethash '$v1 *reg*) 3)
(setf (gethash '$a0 *reg*) 4)
(setf (gethash '$a1 *reg*) 5)
(setf (gethash '$a2 *reg*) 6)
(setf (gethash '$a3 *reg*) 7)

(setf (gethash '$t0 *reg*) 8)
(setf (gethash '$t1 *reg*) 9)
(setf (gethash '$t2 *reg*) 10)
(setf (gethash '$t3 *reg*) 11)
(setf (gethash '$t4 *reg*) 12)
(setf (gethash '$t5 *reg*) 13)
(setf (gethash '$t6 *reg*) 14)
(setf (gethash '$t7 *reg*) 15)

(setf (gethash '$s0 *reg*) 16)
(setf (gethash '$s1 *reg*) 17)
(setf (gethash '$s2 *reg*) 18)
(setf (gethash '$s3 *reg*) 19)
(setf (gethash '$s4 *reg*) 20)
(setf (gethash '$s5 *reg*) 21)
(setf (gethash '$s6 *reg*) 22)
(setf (gethash '$s7 *reg*) 23)

(setf (gethash '$t8 *reg*) 24)
(setf (gethash '$t9 *reg*) 25)
(setf (gethash '$k0 *reg*) 26)
(setf (gethash '$k1 *reg*) 27)
(setf (gethash '$gp *reg*) 28)
(setf (gethash '$sp *reg*) 29)
(setf (gethash '$fp *reg*) 30)
(setf (gethash '$ra *reg*) 31)

(format t "~A~%" (hash-table-count *reg*))
(format t "~B~%" (gethash '$zero *reg*))
(format t "~B~%" (gethash '$s3 *reg*))
