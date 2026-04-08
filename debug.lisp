(load "main.lisp")

(defun snapshot ()
  (list :pc *pc*
		:registers (copy-seq *register*)
		:memory (copy-seq *data-memory*)))

(defun diff-state (before after)
  ;; PC
  (unless (= (getf before :pc) (getf after :pc))
	(format t "~%PC: ~A → ~A" (getf before :pc) (getf after :pc)))
  ;; Registers
  (loop for i from 0 to 31
		for old = (aref (getf before :registers) i)
		for new = (aref (getf after :registers) i)
		unless (= old new)
		do (format t "~%REG[~A]: ~A → ~A" i old new))
  ;; Memory
  (loop for i from 0 below 1024
		for old = (aref (getf before :memory) i)
		for new = (aref (getf after :memory) i)
		unless (= old new)
		do (format t "~%MEM[~A]: ~A → ~A" i old new)))

(let ((before (snapshot)))
  (execute-one-cycle "addi $t0, $zero, 5")
  (diff-state before (snapshot)))
