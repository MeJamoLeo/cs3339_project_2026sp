(defun adder (a b)
  (+ a b))

(defun read-assembly (path)
  (with-open-file (input-stream path)
	(loop for line = (read-line input-stream nil)
		  while line
		  collect (remove #\, (substitute #\Space #\) (substitute #\Space #\( line))))))

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

(defparameter *register-table* (make-hash-table))
(loop for (name num) in
	  '(("$zero" 0) ("$at" 1) ("$v0" 2) ("$v1" 3) ("$a0" 4) ("$a1" 5) ("$a2" 6) ("$a3" 7)
					("$t0" 8) ("$t1" 9) ("$t2" 10) ("$t3" 11) ("$t4" 12) ("$t5" 13) ("$t6" 14) ("$t7" 15)
					("$s0" 16) ("$s1" 17) ("$s2" 18) ("$s3" 19) ("$s4" 20) ("$s5" 21) ("$s6" 22) ("$s7" 23)
					("$t8" 24) ("$t9" 25) ("$k0" 26) ("$k1" 27) ("$gp" 28) ("$sp" 29) ("$fp" 30) ("$ra" 31))
	  do (setf (gethash name *reg*) num))

(defparameter *instruction-memory-table* (make-hash-table))
(loop for (name inst_type num) in
	  '(("add" :r #b100000) ;;32, signed integer addition
		("addi" :i #b001000) ;; 8, add immediate
		("sub" :r #b100010) ;; 34, signed integer subtraction
		("mult" :r #b011000) ;; 24, integer multiplication
		("and" :r #b100100) ;; 36, bitwise and operation
		("or" :r #b100101) ;; 37, bitwise or operation
		("sll" :r #b000000) ;; 0, shift left logical
		("srl" :r #b000010) ;; 2, shift right logical
		("lw" :i #b100011) ;; 35, load word
		("sw" :i #b101011) ;; 43, store word
		("beq" :i #b000100) ;; 4, branch if equal to
		("j" :j #b000010) ;; 2, jump
		("nop" :r #b000000)) ;; 0, branch if equal to
	  do (setf (gethash name *instruction-memory-table*) (list inst_type num)))

(format t "~A~%" (hash-table-count *instruction_memory*))
(format t "~B~%" (gethash 'add *instruction_memory*))
(format t "~A~%" (gethash 'add *instruction_memory*))
(format t "~B~%" (gethash 'lw *instruction_memory*))
(format t "~A~%" (gethash 'lw *instruction_memory*))
