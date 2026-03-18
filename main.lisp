(defparameter *register-table* (let ((ht (make-hash-table :test #'equal)))
									 (loop for (name num) in
										   '(("$zero" 0) ("$at" 1) ("$v0" 2) ("$v1" 3)
											 ("$a0" 4) ("$a1" 5) ("$a2" 6) ("$a3" 7)
											 ("$t0" 8) ("$t1" 9) ("$t2" 10) ("$t3" 11)
											 ("$t4" 12) ("$t5" 13) ("$t6" 14) ("$t7" 15)
											 ("$s0" 16) ("$s1" 17) ("$s2" 18) ("$s3" 19)
											 ("$s4" 20) ("$s5" 21) ("$s6" 22) ("$s7" 23)
											 ("$t8" 24) ("$t9" 25) ("$k0" 26) ("$k1" 27)
											 ("$gp" 28) ("$sp" 29) ("$fp" 30) ("$ra" 31))
										   do (setf (gethash name ht) num))
									 ht))

(defparameter *instruction-table* (let ((ht (make-hash-table :test #'equal)))
										   (loop for (name inst-type opcode-or-funct order) in
												 '(("add" :r #b100000 (rd rs rt)) ;;32, signed integer addition
												   ("addi" :i #b001000 (rt rs imm)) ;; 8, add immediate
												   ("sub" :r #b100010 (rd rs rt)) ;; 34, signed integer subtraction
												   ;("mult" :r #b011000 ()) ;; 24, integer multiplication
												   ("and" :r #b100100 (rd rs rt)) ;; 36, bitwise and operation
												   ("or" :r #b100101 (rd rs rt)) ;; 37, bitwise or operation
												   ("sll" :r #b000000 (rd rt shamt)) ;; 0, shift left logical
												   ("srl" :r #b000010 (rd rt shamt)) ;; 2, shift right logical
												   ("lw" :i #b100011 (rt imm rs)) ;; 35, load word
												   ("sw" :i #b101011 (rt imm rs)) ;; 43, store word
												   ("beq" :i #b000100 (rs rt imm)) ;; 4, branch if equal to
												   ("j" :j #b000010 (addr)) ;; 2, jump
												   ("nop" :r #b000000)) ;; 0, branch if equal to
												 do (setf (gethash name ht) (list inst-type opcode-or-funct order)))
										   ht))

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

(defun get-field (field tokens order)
  (let ((pos (position field order)))
	(if pos
		(nth (1+ pos) tokens)
		nil)))

(defun encode (line)
  (labels ((to-num (s)
			 (if s (parse-integer s) 0))
		   (reg (field order)
			 (or (gethash (get-field field line order) *register-table*) 0)))
	(let* ((instruction (gethash (car line) *instruction-table*))
		   (inst-type  (first instruction))
		   (opcode-or-funct  (second instruction))
		   (order  (third instruction)))
	  (cond ((eq :r inst-type)
			 (logior (ash 0 26)
					 (ash (reg 'rs order) 21)
					 (ash (reg 'rt order) 16)
					 (ash (reg 'rd order) 11)
					 (ash (to-num (get-field 'shamt line order)) 6)
					 opcode-or-funct))
			((eq :i inst-type)
			 (logior (ash opcode-or-funct 26)
					 (ash (reg 'rs order) 21)
					 (ash (reg 'rt order) 16)
					 (logand #b1111111111111111 (to-num (get-field 'imm line order)))))
			((eq :j inst-type)
			 (logior (ash opcode-or-funct 26)
					 (logand #b11111111111111111111111111 (to-num (get-field 'addr line order)))))))))

(mapcar (lambda (line)
			   (format t "~32,'0B~%" (encode line)))
		(parse-assembly "./input"))

