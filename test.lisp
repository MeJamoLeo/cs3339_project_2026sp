(load "main.lisp")

;; ====================================
;; Assemblar
;; ====================================

;; ------------------------------------ normalize
(assert (equal (normalize "addi $t0, $zero, 5") "addi $t0 $zero 5"))
(assert (equal (normalize "lw $t2, 0($sp)") "lw $t2 0 $sp "))
(assert (equal (normalize "nop") "nop"))

;; ====================================
;; CPU
;; ====================================
;; ------------------------------------ adder
(assert (= (adder 0 4) 4))

