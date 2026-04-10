(load "main.lisp")

;; ====================================
;; Assemblar
;; ====================================

;; ------------------------------------ normalize
(assert (equal (normalize "addi $t0, $zero, 5") "addi $t0 $zero 5"))
(assert (equal (normalize "lw $t2, 0($sp)") "lw $t2 0 $sp "))
(assert (equal (normalize "nop") "nop"))

;; ------------------------------------ split-by-spaces
(assert (equal (split-by-spaces "I have a pen.")
			   '("I" "have" "a" "pen.")))
(assert (equal (split-by-spaces "addi $t0 $zero 5")
			   '("addi" "$t0" "$zero" "5")))

;; ------------------------------------ get-operand
;; I-type: addi $t0, $zero, 5 → layout = (rt rs imm)
(assert (equal (get-operand 'rt  '("addi" "$t0" "$zero" "5") '(rt rs imm))
			   "$t0"))
(assert (equal (get-operand 'rs  '("addi" "$t0" "$zero" "5") '(rt rs imm))
			   "$zero"))
(assert (equal (get-operand 'imm '("addi" "$t0" "$zero" "5") '(rt rs imm))
			   "5"))
(assert (equal (get-operand 'rd  '("addi" "$t0" "$zero" "5") '(rt rs imm))
			   nil))
;; R-type: add $s0, $t0, $t1 → layout = (rd rs rt)
(assert (equal (get-operand 'rd '("add" "$s0" "$t0" "$t1") '(rd rs rt))
			   "$s0"))
(assert (equal (get-operand 'rs '("add" "$s0" "$t0" "$t1") '(rd rs rt))
			   "$t0"))
(assert (equal (get-operand 'rt '("add" "$s0" "$t0" "$t1") '(rd rs rt))
			   "$t1"))
;; J-type: j 0 → layout = (addr)
(assert (equal (get-operand 'addr '("j" "0") '(addr))
			   "0"))

;; ------------------------------------ encode-r
;; add $s0, $t0, $t1
;; -> opcode=0, rs=$t0=8, rt=$t1=9, rd=$s0=16, shamt=0, funct=32
;;  opcode   rs     rt     rd    shamt   funct
;; [000000][01000][01001][10000][00000][100000]
(assert (= (encode-r 8 9 16 0 #b100000) #b00000001000010011000000000100000))

;; ------------------------------------ encode-i
;; addi $t1, $zero, 10
;; -> opcode=8, rs=$zero=0, rt=$t1=9, imm=10
;;  opcode   rs     rt          imm
;; [001000][00000][01001][0000000000001010]
(assert (= (encode-i #b001000 0 9 10) #b00100000000010010000000000001010))

;; sw $s0, 0($sp)
;; -> opcode=43, rs=$sp=29, rt=$s0=16, imm=0
;;  opcode   rs     rt          imm
;; [101011][11101][10000][0000000000000000]
(assert (= (encode-i #b101011 29 16 0) #b10101111101100000000000000000000))

;; ------------------------------------ encode-j
;; j 32
;; -> opcode=2, addr=32
;;  opcode           addr
;; [000010][00000000000000000000100000]
(assert (= (encode-j #b000010 32) #b00001000000000000000000000100000))

;; ------------------------------------ to-num
(assert (= (to-num nil) 0))
(assert (= (to-num "1") 1))

;; ====================================
;; CPU
;; ====================================
;; ------------------------------------ adder
(assert (= (adder 0 4) 4))

;; ------------------------------------ decode
;; add $s0, $t0, $t1
;; R) 000000 01000 01001 10000 00000 100000
;; I) 000000 01000 01001 1000000000100000
;; J) 000000 01000010011000000000100000
(assert (equal (decode #b00000001000010011000000000100000)
			   '(:opcode #b000000
				 :rs #b01000
				 :rt #b01001
				 :rd #b10000
				 :shamt #b00000
				 :funct #b100000
				 :imm #b1000000000100000
				 :addr #b01000010011000000000100000)))

;; addi $t1, $zero, 10
;; R) 001000 00000 01001 00000 00000 001010
;; I) 001000 00000 01001 0000000000001010
;; J) 001000 00000010010000000000001010
(assert (equal (decode #b00100000000010010000000000001010)
			   '(:opcode #b001000
				 :rs #b00000
				 :rt #b01001
				 :rd #b00000
				 :shamt #b00000
				 :funct #b001010
				 :imm #b0000000000001010
				 :addr #b00000010010000000000001010)))

;; sw $s0, 0($sp)
;; R) 101011 11101 10000 00000 00000 000000
;; I) 101011 11101 10000 0000000000000000
;; J) 101011 11101100000000000000000000
(assert (equal (decode #b10101111101100000000000000000000)
			   '(:opcode #b101011
				 :rs #b11101
				 :rt #b10000
				 :rd #b00000
				 :shamt #b00000
				 :funct #b000000
				 :imm #b0000000000000000
				 :addr #b11101100000000000000000000)))

;; j 32
;; R) 000010 00000 00000 00000 00000 100000
;; I) 000010 00000 00000 0000000000100000
;; J) 000010 00000000000000000000100000
(assert (equal (decode #b00001000000000000000000000100000)
			   '(:opcode #b000010
				 :rs #b00000
				 :rt #b00000
				 :rd #b00000
				 :shamt #b00000
				 :funct #b100000
				 :imm #b0000000000100000
				 :addr #b00000000000000000000100000)))

;; ------------------------------------ sign-extend
(assert (= (sign-extend #b1000000000100000)
		   #b11111111111111111000000000100000))
(assert (= (sign-extend #b0000000000001010)
		   #b00000000000000000000000000001010))

;; ------------------------------------ register
(assert (= (read-register 0) 0))
(assert (= (read-register 31) 0))
(write-register 0 31)
(assert (= (read-register 0) 0))
(write-register 31 9999)
(assert (= (read-register 31) 9999))

;; ------------------------------------ control-signals
;; R-type (opcode=0)
(let ((ctrl (control #b000000)))
  (assert (= (getf ctrl :reg-dst) 1))
  (assert (= (getf ctrl :alu-src) 0))
  (assert (= (getf ctrl :reg-write) 1))
  (assert (= (getf ctrl :mem-read) 0))
  (assert (= (getf ctrl :mem-write) 0))
  (assert (= (getf ctrl :branch) 0))
  (assert (= (getf ctrl :alu-op) #b10)))

;; lw (opcode=35)
(let ((ctrl (control #b100011)))
  (assert (= (getf ctrl :mem-read) 1))
  (assert (= (getf ctrl :reg-write) 1))
  (assert (= (getf ctrl :alu-src) 1))
  (assert (= (getf ctrl :alu-op) #b00)))

;; sw (opcode=43)
(let ((ctrl (control #b101011)))
  (assert (= (getf ctrl :mem-write) 1))
  (assert (= (getf ctrl :reg-write) 0)))

;; beq (opcode=4)
(let ((ctrl (control #b000100)))
  (assert (= (getf ctrl :branch) 1))
  (assert (= (getf ctrl :reg-write) 0)))

;; ------------------------------------ alu-control
;; ALUOp=00 (lw/sw) → add
(assert (= (getf (alu-control #b00 #b000000) :alu-operation) #b0010))
(assert (= (getf (alu-control #b00 #b000000) :alu-in1-src) 0))

;; ALUOp=01 (beq) → sub
(assert (= (getf (alu-control #b01 #b000000) :alu-operation) #b0110))
(assert (= (getf (alu-control #b01 #b000000) :alu-in1-src) 0))

;; ALUOp=10 (R-type) → depends on funct
(assert (= (getf (alu-control #b10 #b100000) :alu-operation) #b0010)) ;; add  (funct=32)
(assert (= (getf (alu-control #b10 #b100010) :alu-operation) #b0110)) ;; sub  (funct=34)
(assert (= (getf (alu-control #b10 #b100100) :alu-operation) #b0000)) ;; and  (funct=36)
(assert (= (getf (alu-control #b10 #b100101) :alu-operation) #b0001)) ;; or   (funct=37)
(assert (= (getf (alu-control #b10 #b101010) :alu-operation) #b0111)) ;; slt  (funct=42)

;; sll/srl → alu-in1-src=1 (use data2 instead of data1)
(assert (= (getf (alu-control #b10 #b000000) :alu-operation) #b1110)) ;; sll  (funct=0)
(assert (= (getf (alu-control #b10 #b000000) :alu-in1-src) 1))
(assert (= (getf (alu-control #b10 #b000010) :alu-operation) #b1111)) ;; srl  (funct=2)
(assert (= (getf (alu-control #b10 #b000010) :alu-in1-src) 1))

;; ------------------------------------ alu
;; AND ALUOp=0000
(assert (equal (alu 1 1 #b0000) '(1 0)))
(assert (equal (alu 1 0 #b0000) '(0 1)))
(assert (equal (alu 0 1 #b0000) '(0 1)))
(assert (equal (alu 0 0 #b0000) '(0 1)))
(assert (equal (alu #b10001010 #b01111010  #b0000) '(#b00001010 0)))
(assert (equal (alu #b10001010 #b01110101  #b0000) '(#b00000000 1)))
;; OR ALUOp=0001
(assert (equal (alu 1 1 #b0001) '(1 0)))
(assert (equal (alu 1 0 #b0001) '(1 0)))
(assert (equal (alu 0 1 #b0001) '(1 0)))
(assert (equal (alu 0 0 #b0001) '(0 1)))
(assert (equal (alu #b10001010 #b01111010  #b0001) '(#b11111010 0)))
(assert (equal (alu #b10001010 #b01110101  #b0001) '(#b11111111 0)))
;; Add ALUOp=0001
(assert (equal (alu 1 1 #b0010) '(2 0)))
(assert (equal (alu 1 0 #b0010) '(1 0)))
(assert (equal (alu 0 1 #b0010) '(1 0)))
(assert (equal (alu 0 0 #b0010) '(0 1)))
(assert (equal (alu 32 40  #b0010) '(72 0)))
(assert (equal (alu -32 40  #b0010) '(8 0)))
;; Sub ALUOp=0110
(assert (equal (alu 1 1 #b0110) '(0 1)))
(assert (equal (alu 1 0 #b0110) '(1 0)))
(assert (equal (alu 0 1 #b0110) '(-1 0)))
(assert (equal (alu 0 0 #b0110) '(0 1)))
(assert (equal (alu 32 40  #b0110) '(-8 0)))
(assert (equal (alu -32 40  #b0110) '(-72 0)))
;; set on less than ALUOp=0111
(assert (equal (alu 1 1 #b0111) '(0 1)))
(assert (equal (alu 1 0 #b0111) '(0 1)))
(assert (equal (alu 1 99 #b0111) '(1 0)))
;; NOR ALUOp=1100
(assert (equal (alu 0 0 #b1100) '(#xFFFFFFFF 0))) ;; 0=#x00000000
(assert (equal (alu #x00000000 #x00000000 #b1100) '(#xFFFFFFFF 0)))
(assert (equal (alu 1 0 #b1100) '(#xFFFFFFFE 0))) ;; 1=#x00000001
(assert (equal (alu 1 1 #b1100) '(#xFFFFFFFE 0)))
(assert (equal (alu #xFFFFFFFF #xFFFFFFFF #b1100) '(#x00000000 1)))
(assert (equal (alu #xFFFFFFFF #x00000000 #b1100) '(#x00000000 1)))

;; ------------------------------------ register
(assert (= (read-data-memory 0) 0))
(assert (= (read-data-memory 31) 0))
(write-data-memory 0 31)
(assert (= (read-data-memory 0) 31))
(write-data-memory 31 9999)
(assert (= (read-data-memory 31) 9999))

(format t "~%✅ All unit test passed!!")

;; ------------------------------------ encode
(assert (= (encode '("nop")) #b00000000000000000000000000000000))
(assert (= (encode '("add" "$s0" "$t0" "$t1")) #b00000001000010011000000000100000))
(assert (= (encode '("addi" "$t1" "$zero" "10")) #b00100000000010010000000000001010))
(assert (= (encode '("sw" "$s0" "0" "$sp")) #b10101111101100000000000000000000))
(assert (= (encode '("j" "32")) #b00001000000000000000000000100000))
(assert (= (encode '("sll" "$s4" "$s0" "2")) #b00000000000100001010000010000000))

;; ------------------------------------ encode, decode, check ctrl-signals
(let* ((encoded (encode (split-by-spaces (normalize "addi $t0, $zero, 5"))))
	   (decoded (decode encoded))
	   (ctrl    (control (getf decoded :opcode))))
  (assert (= (getf ctrl :alu-src) 1))
  (assert (= (getf ctrl :reg-write) 1)))

;; ------------------------------------ execute one cycle

;; reset register and program counter
(setf *pc* 0)
(fill *register* 0)

(execute-one-cycle "addi $t0, $zero, 5") ;; $t0 supposed to be 5
(assert (= (read-register 8) 5)) ;; $t0 = reg[8]
(assert (= *pc* 4))

(execute-one-cycle "addi $t1, $zero, 10")
(assert (= (read-register 9) 10)) ;; $t1 = reg[9]
(assert (= *pc* 8))

(execute-one-cycle "add $s0, $t0, $t1")
(assert (= (read-register 16) 15)) ;; $s0 = reg[16]
(assert (= *pc* 12))

(execute-one-cycle "sub $s1, $s0, $t0")
(assert (= (read-register 17) (read-register 9))) ;; $s1 = reg[17]
(assert (= *pc* 16))

(execute-one-cycle "and $s1, $s0, $t0")
(assert (= (read-register 17) #b0101)) ;; $s1 = reg[17]
(assert (= *pc* 20))

(execute-one-cycle "or $s1, $s0, $t0")
(assert (= (read-register 17) #b1111)) ;; $s1 = reg[17]
(assert (= *pc* 24))

(execute-one-cycle "sll $s4, $s0, 2")
(assert (= (read-register 20) #b111100)) ;; $s4 = reg[20]
(assert (= *pc* 28))

(execute-one-cycle "srl $s4, $s0, 2")
(assert (= (read-register 20) #b11)) ;; $s4 = reg[20]
(assert (= *pc* 32))


(execute-one-cycle "sw $s0, 0($zero)")
(assert (= (read-data-memory 0) (read-register 16))) ;; $s0 = reg[16]
(assert (= *pc* 36))

(execute-one-cycle "lw $s2, 0($zero)")
(assert (= (read-register 18) 15)) ;; $s2 = reg[18]
(assert (= *pc* 40))

(execute-one-cycle "beq $t0, $t1, 100")
(assert (= *pc* 44))

(execute-one-cycle "beq $t0, $t0, 100")
(assert (= *pc* 448))

(execute-one-cycle "j 0")
(assert (= *pc* 0))

;; ------------------------------------ pipeline
;; reset register and program counter
(setf *pc* 0)
(fill *register* 0)
(setf *instruction-memory*
	  (vector (encode (split-by-spaces (normalize "addi $t0, $zero, 5")))
			  (encode (split-by-spaces (normalize "addi $t1, $zero, 10")))
			  (encode (split-by-spaces (normalize "add $s0, $t0, $t1")))))

;; "addi $t0, $zero, 5"
(assert (equal (stage-if *pc*)
			   (list :instruction #b00100000000010000000000000000101
					 :pc+4 4)))

(setf *pc* (+ *pc* 4))

 ;; "addi $t0, $zero, 5"
(assert (equal (stage-if *pc*)
			   (list :instruction #b00100000000010010000000000001010
					 :pc+4 8)))

(format t "~%✅ All Integration test passed!!~%")
