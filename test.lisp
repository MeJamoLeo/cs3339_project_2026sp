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
;; ====================================
;; CPU
;; ====================================
;; ------------------------------------ adder
(assert (= (adder 0 4) 4))






(format t "✅ All test passed!!")
