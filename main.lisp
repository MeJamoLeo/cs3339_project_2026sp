;; ====================================
;; Assemblar
;; ====================================

;; make a list of lines from input file
(defun read-lines (path)
  (with-open-file (input-stream path)
	(loop for line = (read-line input-stream nil)
		  while line
		  collect line)))

;; remove comma, and brackets
(defun normalize (line)
  (remove #\, (substitute #\Space #\) (substitute #\Space #\( line))))

;; read a file in asesmbly
;; - depends on
;;     - normalize
;;     - read-lines
(defun read-assembly (path)
  (mapcar #'normalize (read-lines path)))

;; make a list form string with spliting by space
(defun split-by-spaces (str)
  (let ((trimmed (string-trim " " str)))
	(if (string= trimmed "")
		nil
		(let ((pos (position #\Space trimmed)))
		  (if pos
			  (cons (subseq trimmed 0 pos)
					(split-by-spaces (subseq trimmed (1+ pos))))
			  (list trimmed))))))

;; make 2D list of tokens from a assembly file
;; - depends on
;;     - read-assembly
;;     - split-by-spaces
(defun parse-assembly (path)
  (mapcar #'split-by-spaces (read-assembly path)))

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
									(loop for (name inst-type opcode-or-funct fields) in
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
											("nop" :r #b000000)) ;; 0, no operation
										  do (setf (gethash name ht) (list :type inst-type :code opcode-or-funct :fields fields)))
									ht))

(defun get-operand (field tokens layout)
  (let ((pos (position field layout)))
	(if pos
		(nth (1+ pos) tokens)
		nil)))

(defun encode (line)
  (labels ((to-num (s)
			 (if s (parse-integer s) 0))
		   (reg (field fields)
			 (or (gethash (get-operand field line fields) *register-table*) 0)))
	(let* ((instruction (gethash (car line) *instruction-table*))
		   (inst-type  (getf instruction :type))
		   (opcode-or-funct (getf instruction :code))
		   (fields  (getf instruction :fields)))
	  (cond ((eq :r inst-type)
			 (logior (ash 0 26)
					 (ash (reg 'rs fields) 21)
					 (ash (reg 'rt fields) 16)
					 (ash (reg 'rd fields) 11)
					 (ash (to-num (get-operand 'shamt line fields)) 6)
					 opcode-or-funct))
			((eq :i inst-type)
			 (logior (ash opcode-or-funct 26)
					 (ash (reg 'rs fields) 21)
					 (ash (reg 'rt fields) 16)
					 (logand #b1111111111111111 (to-num (get-operand 'imm line fields)))))
			((eq :j inst-type)
			 (logior (ash opcode-or-funct 26)
					 (logand #b11111111111111111111111111 (to-num (get-operand 'addr line fields)))))))))


;; ====================================
;; CPU
;; ====================================
;; program counter
(defparameter *pc* 0)

(defun adder (a b)
  (+ a b))

(defun decode (value)
  (let ((inst25-0 (logand #b1111111111111111111111111 value)) ;; addr
		(inst10-6 (logand #b11111 (ash value -6))) ;; shamt
		(inst31-26 (ash value -26)) ;; opcode to Controller
		(inst25-21 (logand #b11111 (ash value -21))) ;; rs to Register Read1
		(inst20-16 (logand #b11111 (ash value -16))) ;; rt to Register Read2
		(inst15-11 (logand #b11111 (ash value -11))) ;; rd to Mux_RegDst
		(inst15-0 (logand #b1111111111111111 value)) ;; imm to SignExtend
		(inst5-0 (logand #b111111 value))) ;; funct to ALU Controller
	(format t "opcode\(~A\):~C~6,'0B~%" inst31-26 #\Tab inst31-26)
	(format t "rs\(~A\)~C~5,'0B~%" inst25-21 #\Tab inst25-21)
	(format t "rt\(~A\)~C~5,'0B~%" inst20-16 #\Tab inst20-16)
	(format t "rd\(~A\)~C~5,'0B~%" inst15-11 #\Tab inst15-11)
	(format t "shamt\(~A\)~C~5,'0B~%" inst10-6 #\Tab inst10-6)
	(format t "funct\(~A\)~C~6,'0B~%" inst5-0 #\Tab inst5-0)
	(format t "imm\(~A\)~C~16,'0B~%"  inst15-0 #\Tab inst15-0)
	(format t "addr\(~A\)~C~26,'0B~%"  inst25-0 #\Tab inst25-0)))


;; ====================================
;; Debug
;; ====================================
(mapcar (lambda (line)
		  (format t "~A~%" "=======================================")
		  (format t "~A~%" line)
		  (format t "~A~%" "---------------------------------------")
		  (decode (encode line))
		  (format t "~A~%~%" "======================================="))
		(parse-assembly "./input"))

