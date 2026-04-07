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
									(loop for (name inst-type opcode-or-funct layout) in
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
										  do (setf (gethash name ht) (list :type inst-type :code opcode-or-funct :layout layout)))
									ht))

(defun get-operand (field tokens layout)
  (let ((pos (position field layout)))
	(if pos
		(nth (1+ pos) tokens)
		nil)))

(defun encode-r (rs rt rd shamt funct)
  (logior (ash rs 21)
		  (ash rt 16)
		  (ash rd 11)
		  (ash shamt 6)
		  funct))

(defun encode-i (opcode rs rt imm)
  (logior  (ash opcode 26)
		   (ash rs 21)
		   (ash rt 16)
		   (logand #b1111111111111111 imm)))

(defun encode-j (opcode addr)
  (logior (ash opcode 26)
		  (logand #b11111111111111111111111111 addr)))

(defun to-num (s)
  (if s (parse-integer s) 0))

;; convert tokens to binary
;; sample input: ("add" "$s0" "$t0" "$t1")
;; sample output: 00000001000010011000000000100000
(defun encode (tokens)
  (let* ((instruction (gethash (car tokens) *instruction-table*)) ;; e.g.) "add"
		 (inst-type  (getf instruction :type)) ;; e.g.) :r
		 (opcode-or-funct (getf instruction :code)) ;; e.g.) #b100000 which is 32
		 (layout  (getf instruction :layout))) ;; e.g.) (rd rs rt)
	(labels ((reg (field)
			   (or (gethash (get-operand field tokens layout) *register-table*) 0)))
	  (cond ((eq :r inst-type)
			 (encode-r (reg 'rs)
					   (reg 'rt)
					   (reg 'rd)
					   (to-num (get-operand 'shamt tokens layout))
					   opcode-or-funct))
			((eq :i inst-type)
			 (encode-i opcode-or-funct
					   (reg 'rs)
					   (reg 'rt)
					   (to-num (get-operand 'imm tokens layout))))
			((eq :j inst-type)
			 (encode-j opcode-or-funct
					   (to-num (get-operand 'addr tokens layout))))))))

;; ====================================
;; CPU
;; ====================================
(defun adder (a b)
  (+ a b))

(defun decode (value)
  (list :opcode (ash value -26) ;; inst[31-26]
		:rs (logand #b11111 (ash value -21)) ;; inst[25-21]
		:rt (logand #b11111 (ash value -16)) ;; inst[20-16]
		:rd (logand #b11111 (ash value -11)) ;; inst[15-11]
		:shamt (logand #b11111 (ash value -6)) ;; inst[10-6]
		:funct (logand #b111111 value) ;; inst[5-0]
		:imm (logand #b1111111111111111 value) ;; inst[15-0]
		:addr (logand #b11111111111111111111111111 value))) ;; inst[25-0]

(defun sign-extend (inst15-0)
  (if (logbitp 15 inst15-0)
	  (logior #xFFFF0000 inst15-0)
	  inst15-0))

;; controller
(defun control (opcode)
  (case opcode
	(#b000000 ;; R-type
	 `(:reg-dst		1
	   :alu-src		0
	   :mem-to-reg	0
	   :reg-write	1
	   :mem-read	0
	   :mem-write	0
	   :branch		0
	   :alu-op		#b10))
	(#b001000 ;; addi
	 `(:reg-dst		0
	   :alu-src		1
	   :mem-to-reg	0
	   :reg-write	1
	   :mem-read	0
	   :mem-write	0
	   :branch		0
	   :alu-op		#b00))
	(#b100011 ;; lw
	 `(:reg-dst		0
	   :alu-src		1
	   :mem-to-reg	1
	   :reg-write	1
	   :mem-read	1
	   :mem-write	0
	   :branch		0
	   :alu-op		#b00))
	(#b101011 ;; sw
	 `(:reg-dst		nil
	   :alu-src		1
	   :mem-to-reg	nil
	   :reg-write	0
	   :mem-read	0
	   :mem-write	1
	   :branch		0
	   :alu-op		#b00))
	(#b000100 ;; beq
	 `(:reg-dst		nil
	   :alu-src		nil
	   :mem-to-reg	nil
	   :reg-write	0
	   :mem-read	0
	   :mem-write	0
	   :branch		1
	   :alu-op		#b00))
	(#b000010 ;; j
	 `(:reg-dst		nil
	   :alu-src		0
	   :mem-to-reg	nil
	   :reg-write	0
	   :mem-read	0
	   :mem-write	0
	   :branch		1
	   :alu-op		#b01))))

;; alu-control
(defun alu-control (alu-op funct)
  (cond ((= alu-op #b00) #b0010) ;; lw or sw -> add
		((logbitp 0 alu-op) #b0110) ;; ALUOp0=1 -> beq -> sub
		((logbitp 1 alu-op) ;; ALUOp1=1 -> r-type
		 (case (logand #b1111 funct) ;; care only funct[3-0]
		   (#b0000 #b0010)
		   (#b0010 #b0110)
		   (#b0100 #b0000)
		   (#b0101 #b0001)
		   (#b1010 #b0111)))))

;; alu
;; TODO: carry-out, overflow but not now
(defun alu (in1 in2 alu-op)
  (let ((result (case alu-op
				  (#b0000 (logand in1 in2))
				  (#b0001 (logior in1 in2))
				  (#b0010 (+ in1 in2))
				  (#b0110 (- in1 in2))
				  (#b0111 (if (< in1 in2) 1 0))
				  (#b1100 (logand #xFFFFFFFF (lognot (logior in1 in2))))))) ;; NOR
  (list result (if (zerop result) 1 0))))


;; register
(defparameter *register* (make-array 32 :initial-element 0))

(defun read-register (num)
  (aref *register* num))

(defun write-register (num value)
  (unless (zerop num)
	(setf (aref *register* num) value)))

;; data-memory
(defparameter *data-memory* (make-array 1024 :initial-element 0))

(defun read-data-memory (num)
  (aref *data-memory* num))

(defun write-data-memory (num value)
  (setf (aref *data-memory* num) value))

;; program counter
(defparameter *pc* 0)


(defun print-decoded (decoded)
  (format t "~%===========================")
  (format t "~%~A~6,'0B"	"opcode: " (getf decoded :opcode))
  (format t "~%~A~5,'0B"	"rs:     " (getf decoded :rs))
  (format t "~%~A~5,'0B"	"rt:     " (getf decoded :rt))
  (format t "~%~A~5,'0B"	"rd:     " (getf decoded :rd))
  (format t "~%~A~5,'0B"	"shamt:  " (getf decoded :shamt))
  (format t "~%~A~6,'0B"	"funct:  " (getf decoded :funct))
  (format t "~%~A~16,'0B"	"imm:    " (getf decoded :imm))
  (format t "~%~A~26,'0B"	"addr:   " (getf decoded :addr))
  (format t "~%==========================="))

(defun execute-one-cycle (line)
  (let* ((encoded (encode (split-by-spaces (normalize "addi $t0, $zero, 5"))))
		 (decoded (decode encoded))
		 ;; decode
		 (opcode (getf decoded :opcode))
		 (rs (getf decoded :rs))
		 (rt (getf decoded :rt))
		 (rd (getf decoded :rd))
		 (shamt (getf decoded :shamt))
		 (funct (getf decoded :funct))
		 (imm (getf decoded :imm))
		 (addr (getf decoded :addr))
		 ;; control
		 (control-signals (control opcode))
		 ;; register read
		 (data1 (read-register rs))
		 (data2 (read-register rt))

		 ;; ALU
		 (alu-operation (alu-control (getf control-signals :alu-op) funct))
		 (alu-output (alu data1
						   (if (= (getf control-signals :alu-src) 1)
									 (sign-extend (getf decoded :imm))
									 data2)
						   alu-operation))
		 (alu-result (car alu-output))
		 (alu-zero (cdar alu-output))

		 ;; Memory
		 (mem-data (if (= (getf control-signals :mem-read) 1)
					   (read-data-memory alu-result)
					   0))

		 ;; Write Back
		 (write-data (if (= (getf control-signals :mem-to-reg) 1)
						 memdata
						 alu-result))
		 (write-reg (if (= (getf control-signals :reg-dst) 1)
						rd
						rt))
		 )
	))

(defun main ()
  (let ((instructions (mapcar #'encode (parse-assembly "./input"))))
	(loop while (< (/ *pc* 4) (length instructions))
		  for instruction = (nth (/ *pc* 4) instructions)
		  for decoded = (decode instruction)
		  do
		  (print-decoded decoded)
		  (setf *pc* (+ *pc* 4)) ; for ending loop
		  )))
(main)
