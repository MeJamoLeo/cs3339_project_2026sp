;(load "./debug.lisp")

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
											("mul" :r #b011000 (rd rs rt)) ;; 24, integer multiplication (low 32 bits)
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
	   :jump		0
	   :alu-op		#b10))
	(#b001000 ;; addi
	 `(:reg-dst		0
	   :alu-src		1
	   :mem-to-reg	0
	   :reg-write	1
	   :mem-read	0
	   :mem-write	0
	   :branch		0
	   :jump		0
	   :alu-op		#b00))
	(#b100011 ;; lw
	 `(:reg-dst		0
	   :alu-src		1
	   :mem-to-reg	1
	   :reg-write	1
	   :mem-read	1
	   :mem-write	0
	   :branch		0
	   :jump		0
	   :alu-op		#b00))
	(#b101011 ;; sw
	 `(:reg-dst		nil
	   :alu-src		1
	   :mem-to-reg	nil
	   :reg-write	0
	   :mem-read	0
	   :mem-write	1
	   :branch		0
	   :jump		0
	   :alu-op		#b00))
	(#b000100 ;; beq
	 `(:reg-dst		nil
	   :alu-src		0
	   :mem-to-reg	nil
	   :reg-write	0
	   :mem-read	0
	   :mem-write	0
	   :branch		1
	   :jump		0
	   :alu-op		#b01))
	(#b000010 ;; j
	 `(:reg-dst		nil
	   :alu-src		0
	   :mem-to-reg	nil
	   :reg-write	0
	   :mem-read	0
	   :mem-write	0
	   :branch		1
	   :jump		1
	   :alu-op		#b01))))

;; alu-control
(defun alu-control (alu-op funct)
  (cond ((= alu-op #b00) '(:alu-operation #b0010 :alu-in1-src 0)) ;; lw or sw -> add
		((logbitp 0 alu-op) '(:alu-operation #b0110 :alu-in1-src 0)) ;; ALUOp0=1 -> beq -> sub
		((logbitp 1 alu-op) ;; ALUOp1=1 -> r-type
		 (case funct
		   (#b100000 '(:alu-operation #b0010 :alu-in1-src 0)) ;; add
		   (#b100010 '(:alu-operation #b0110 :alu-in1-src 0)) ;; sub
		   (#b100100 '(:alu-operation #b0000 :alu-in1-src 0)) ;; and
		   (#b100101 '(:alu-operation #b0001 :alu-in1-src 0)) ;; or
		   (#b011000 '(:alu-operation #b0011 :alu-in1-src 0)) ;; mul (low 32 bits)
		   (#b101010 '(:alu-operation #b0111 :alu-in1-src 0)) ;; set on less than
		   (#b000000 '(:alu-operation #b1110 :alu-in1-src 1)) ;; sll, shift left logical
		   (#b000010 '(:alu-operation #b1111 :alu-in1-src 1)) ;; srl, shift right logical
		   ))))

;; alu
;; TODO: carry-out, overflow but not now
(defun alu (in1 in2 alu-op)
  (let ((result (case alu-op
				  (#b0000 (logand in1 in2))
				  (#b0001 (logior in1 in2))
				  (#b0010 (+ in1 in2))
				  (#b0011 (logand #xFFFFFFFF (* in1 in2))) ;; mul, low 32 bits
				  (#b0110 (- in1 in2))
				  (#b0111 (if (< in1 in2) 1 0))
				  (#b1100 (logand #xFFFFFFFF (lognot (logior in1 in2)))) ;; NOR
				  (#b1110 (ash in2 in1)) ;; sll, shift left logical
				  (#b1111 (ash in2 (- in1))) ;; srl, shift right logical
				  )))
	(list :result result
		  :zero (if (zerop result) 1 0))))


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

(defun execute-one-cycle (line)
  (let* ((encoded (encode (split-by-spaces (normalize line))))
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
		 (data-reg-read1 (read-register rs))
		 (data-reg-read2 (read-register rt))

		 ;; ALU
		 (alu-ctrl-signals (alu-control (getf control-signals :alu-op) funct))
		 (alu-output (alu (if (= (getf alu-ctrl-signals :alu-in1-src) 1)
							  shamt
							  data-reg-read1)
						  (if (= (getf control-signals :alu-src) 1)
							  (sign-extend imm)
							  data-reg-read2)
						  (getf alu-ctrl-signals :alu-operation)))
		 (alu-result (getf alu-output :result))
		 (alu-zero (getf alu-output :zero))

		 ;; Memory
		 (mem-data (if (= (getf control-signals :mem-read) 1)
					   (read-data-memory alu-result)
					   0))

		 ;; Write Back
		 (write-data (if (eql (getf control-signals :mem-to-reg) 1)
						 mem-data
						 alu-result))
		 (write-reg (if (eql (getf control-signals :reg-dst) 1)
						rd
						rt))
		 ;; Program Coutner
		 (pc+4 (+ *pc* 4))
		 (branch-target (+ pc+4 (ash (sign-extend imm) 2)))
		 (branch-result (if (and (= (getf control-signals :branch) 1) 
								 (= alu-zero 1))
							branch-target
							pc+4))
		 (jump-target (logior (ash addr 2)
							  (logand pc+4 #xF0000000)))
		 (next-pc (if (= (getf control-signals :jump) 1)
					  jump-target
					  branch-result)))
	;; State change (in other words, side effect)
	(when (= (getf control-signals :reg-write) 1)
	  (write-register write-reg write-data))
	(when (= (getf control-signals :mem-write) 1)
	  (write-data-memory alu-result data-reg-read2))
	(setf *pc* next-pc)
	))

;(execute-one-cycle "addi $t0, $zero, 5")

(defun stage-if (pc)
  (let* ((instruction (aref *instruction-memory*  (/ pc 4)))
		  (pc+4 (+ pc 4)))
	(list :instruction instruction :pc+4 pc+4)))

(defun stage-id (if-id)
  (let* ((decoded  (decode (getf if-id :instruction)))
		 (opcode (getf decoded :opcode))
		 ;; control
		 (control-signals (control opcode))
		 (rs (getf decoded :rs))
		 (rt (getf decoded :rt))
		 (rd (getf decoded :rd))
		 (shamt (getf decoded :shamt))
		 (funct (getf decoded :funct))
		 (sign-extended (sign-extend(getf decoded :imm)))
		 (addr (getf decoded :addr))
		 ;; register read
		 (data-reg-read1 (read-register rs))
		 (data-reg-read2 (read-register rt)))
	(list :control-signals 	control-signals
		  :pc+4				(getf if-id :pc+4)
		  :data-reg-read1	data-reg-read1
		  :data-reg-read2	data-reg-read2
		  :sign-extended	sign-extended
		  :rt				rt
		  :rd				rd
		  :shamt			shamt
		  :funct			funct
		  :addr				addr)))

(defun stage-ex (id-exec)
  (let* ((control-signals  (getf id-exec :control-signals))
		 (alu-ctrl-signals (alu-control (getf control-signals :alu-op)
										(getf id-exec :funct)))
		 (data-reg-read2   (getf id-exec :data-reg-read2))
		 (alu-in1 (if (= (getf alu-ctrl-signals :alu-in1-src) 1) ;; MUX for shifter, sll/srl
					  (getf id-exec :shamt)
					  (getf id-exec :data-reg-read1)))
		 (alu-in2 (if (= (getf control-signals :alu-src) 1) ;; MUX for ALUSrc
					  (getf id-exec :sign-extended)
					  data-reg-read2))
		 (alu-output (alu alu-in1 alu-in2 (getf alu-ctrl-signals :alu-operation)))
		 (branch-target (adder (getf id-exec :pc+4)
							   (ash (getf id-exec :sign-extended) 2))) ;; shift left 2
		 (write-reg (if (eql (getf control-signals :reg-dst) 1) ;; MUX for RegDst (eql: reg-dst may be nil for SW/BEQ/J)
						(getf id-exec :rd)
						(getf id-exec :rt))))
	(list :control-signals control-signals
		  :branch-target   branch-target
		  :alu-zero        (getf alu-output :zero)
		  :alu-result      (getf alu-output :result)
		  :data-mem-write  data-reg-read2
		  :write-reg       write-reg)))

(defun stage-mem (ex-mem)
  (let* ((control-signals (getf ex-mem :control-signals))
		 (alu-result      (getf ex-mem :alu-result))
		 (mem-data (if (= (getf control-signals :mem-read) 1)
					   (read-data-memory alu-result)
					   0)))
	(when (= (getf control-signals :mem-write) 1)
	  (write-data-memory alu-result (getf ex-mem :data-mem-write)))
	(list :control-signals control-signals
		  :mem-data        mem-data
		  :alu-result      alu-result
		  :write-reg       (getf ex-mem :write-reg))))

(defun stage-wb (mem-wb)
  (let ((control-signals (getf mem-wb :control-signals)))
	(when (= (getf control-signals :reg-write) 1)
	  (let ((write-data (if (= (getf control-signals :mem-to-reg) 1) ;; MUX for MemToReg
							(getf mem-wb :mem-data)
							(getf mem-wb :alu-result))))
		(write-register (getf mem-wb :write-reg) write-data)))))

(defparameter *instruction-memory* #())

;; ====================================
;; Pipeline driver (double-buffered)
;; ====================================
;; Each pipeline register holds the value written at the end of the
;; previous cycle. A cycle runs in two phases:
;;   Phase 1 : every stage reads the old register values and computes
;;             its new output into a local binding (no pipeline register
;;             is overwritten). Register-file and data-memory writes
;;             happen here; they model MIPS half-cycle semantics.
;;   Phase 2 : all four pipeline registers and the PC are committed at
;;             the same time. This mimics the clock edge in hardware.

(defparameter *if-id*  nil)
(defparameter *id-ex*  nil)
(defparameter *ex-mem* nil)
(defparameter *mem-wb* nil)

(defun pc-in-range-p (pc)
  (< (/ pc 4) (length *instruction-memory*)))

(defun compute-next-pc (current-pc new-ex-mem new-id-ex)
  (cond
	;; branch taken (resolved in EX this cycle)
	((and new-ex-mem
		  (= (getf (getf new-ex-mem :control-signals) :branch) 1)
		  (= (getf new-ex-mem :alu-zero) 1))
	 (getf new-ex-mem :branch-target))
	;; jump (resolved in ID this cycle)
	((and new-id-ex
		  (= (getf (getf new-id-ex :control-signals) :jump) 1))
	 (logior (ash (getf new-id-ex :addr) 2)
			 (logand (+ current-pc 4) #xF0000000)))
	(t (+ current-pc 4))))

(defun pipeline-cycle ()
  ;; Phase 1a: commit the register-file write from the previous MEM/WB.
  ;; Running stage-wb before stage-id models MIPS half-cycle semantics
  ;; (WB writes in the first half, ID reads in the second half, so ID
  ;; sees WB this cycle).
  (when *mem-wb* (stage-wb *mem-wb*))
  ;; Phase 1b: compute new pipeline register values from the old ones
  (let* ((new-mem-wb (when *ex-mem* (stage-mem *ex-mem*)))
		 (new-ex-mem (when *id-ex* (stage-ex *id-ex*)))
		 (new-id-ex  (when *if-id* (stage-id *if-id*)))
		 (fetched    (when (pc-in-range-p *pc*) (stage-if *pc*)))
		 (next-pc    (compute-next-pc *pc* new-ex-mem new-id-ex)))
	;; Phase 2: commit everything at once (the clock edge)
	(setf *if-id*  fetched
		  *id-ex*  new-id-ex
		  *ex-mem* new-ex-mem
		  *mem-wb* new-mem-wb
		  *pc*     next-pc)))

(defun pipeline-drained-p ()
  (and (not (pc-in-range-p *pc*))
	   (null *if-id*)
	   (null *id-ex*)
	   (null *ex-mem*)
	   (null *mem-wb*)))

(defun run-pipeline ()
  (loop until (pipeline-drained-p)
		do (pipeline-cycle)))

