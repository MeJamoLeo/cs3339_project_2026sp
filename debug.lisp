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

