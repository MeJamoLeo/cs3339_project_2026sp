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

;; ====================================
;; CPU
;; ====================================
;; ------------------------------------ adder
(assert (= (adder 0 4) 4))






(format t "✅ All test passed!!")
