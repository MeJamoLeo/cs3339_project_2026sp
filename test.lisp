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
			   '(:addr #b01000010011000000000100000
				 :opcode #b000000
				 :rs #b01000
				 :rt #b01001
				 :rd #b10000
				 :imm #b1000000000100000
				 :shamt #b00000
				 :funct #b100000)))

;; addi $t1, $zero, 10
;; R) 001000 00000 01001 00000 00000 001010
;; I) 001000 00000 01001 0000000000001010
;; J) 001000 00000010010000000000001010
(assert (equal (decode #b00100000000010010000000000001010)
			   '(:addr #b00000010010000000000001010
				 :opcode #b001000
				 :rs #b00000
				 :rt #b01001
				 :rd #b00000
				 :imm #b0000000000001010
				 :shamt #b00000
				 :funct #b001010)))

;; sw $s0, 0($sp)
;; R) 101011 11101 10000 00000 00000 000000
;; I) 101011 11101 10000 0000000000000000
;; J) 101011 11101100000000000000000000
(assert (equal (decode #b10101111101100000000000000000000)
			   '(:addr #b11101100000000000000000000
				 :opcode #b101011
				 :rs #b11101
				 :rt #b10000
				 :rd #b00000
				 :imm #b0000000000000000
				 :shamt #b00000
				 :funct #b000000)))

;; j 32
;; R) 000010 00000 00000 00000 00000 100000
;; I) 000010 00000 00000 0000000000100000
;; J) 000010 00000000000000000000100000
(assert (equal (decode #b00001000000000000000000000100000)
			   '(:addr #b00000000000000000000100000
				 :opcode #b000010
				 :rs #b00000
				 :rt #b00000
				 :rd #b00000
				 :imm #b0000000000100000
				 :shamt #b00000
				 :funct #b100000)))

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
  (assert (= (getf ctrl :branch) 0)))

;; lw (opcode=35)
(let ((ctrl (control #b100011)))
  (assert (= (getf ctrl :mem-read) 1))
  (assert (= (getf ctrl :reg-write) 1))
  (assert (= (getf ctrl :alu-src) 1)))

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
(assert (= (alu-control #b00 #b000000) #b0010))

;; ALUOp=01 (beq) → sub
(assert (= (alu-control #b01 #b000000) #b0110))

;; ALUOp=10 (R-type) → depends on funct
(assert (= (alu-control #b10 #b100000) #b0010)) ;; add  (funct=32)
(assert (= (alu-control #b10 #b100010) #b0110)) ;; sub  (funct=34)
(assert (= (alu-control #b10 #b100100) #b0000)) ;; and  (funct=36)
(assert (= (alu-control #b10 #b100101) #b0001)) ;; or   (funct=37)
(assert (= (alu-control #b10 #b101010) #b0111)) ;; slt  (funct=42)

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

(format t "~%✅ All unit test passed!!")

;; ------------------------------------ encode
(assert (= (encode '("nop")) #b00000000000000000000000000000000))
(assert (= (encode '("add" "$s0" "$t0" "$t1")) #b00000001000010011000000000100000))
(assert (= (encode '("addi" "$t1" "$zero" "10")) #b00100000000010010000000000001010))
(assert (= (encode '("sw" "$s0" "0" "$sp")) #b10101111101100000000000000000000))
(assert (= (encode '("j" "32")) #b00001000000000000000000000100000))
(assert (= (encode '("sll" "$s4" "$s0" "2")) #b00000000000100001010000010000000))


(format t "~%✅ All Integration test passed!!~%")
