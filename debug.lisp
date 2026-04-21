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

;; --- cycle-level display for the pipeline demo ---

(defun format-control-signals (cs)
  (if cs
	  (format nil "regW=~A memR=~A memW=~A br=~A jmp=~A memToReg=~A aluOp=~A"
			  (getf cs :reg-write)
			  (getf cs :mem-read)
			  (getf cs :mem-write)
			  (getf cs :branch)
			  (getf cs :jump)
			  (getf cs :mem-to-reg)
			  (getf cs :alu-op))
	  "-"))

(defun print-pipeline-state ()
  (format t "~&  IF/ID : ~A"
		  (if *if-id*
			  (format nil "instr=#x~8,'0X  pc+4=~A"
					  (getf *if-id* :instruction)
					  (getf *if-id* :pc+4))
			  "(bubble)"))
  (format t "~&  ID/EX : ~A"
		  (if *id-ex*
			  (format nil "rs=~A rt=~A signExt=~A shamt=~A funct=~A | ~A"
					  (getf *id-ex* :data-reg-read1)
					  (getf *id-ex* :data-reg-read2)
					  (getf *id-ex* :sign-extended)
					  (getf *id-ex* :shamt)
					  (getf *id-ex* :funct)
					  (format-control-signals (getf *id-ex* :control-signals)))
			  "(bubble)"))
  (format t "~&  EX/MEM: ~A"
		  (if *ex-mem*
			  (format nil "aluResult=~A zero=~A brTarget=~A writeReg=~A | ~A"
					  (getf *ex-mem* :alu-result)
					  (getf *ex-mem* :alu-zero)
					  (getf *ex-mem* :branch-target)
					  (getf *ex-mem* :write-reg)
					  (format-control-signals (getf *ex-mem* :control-signals)))
			  "(bubble)"))
  (format t "~&  MEM/WB: ~A"
		  (if *mem-wb*
			  (format nil "memData=~A aluResult=~A writeReg=~A | ~A"
					  (getf *mem-wb* :mem-data)
					  (getf *mem-wb* :alu-result)
					  (getf *mem-wb* :write-reg)
					  (format-control-signals (getf *mem-wb* :control-signals)))
			  "(bubble)")))

(defun print-cycle-header (cycle)
  (format t "~%~%===== Cycle ~A  (PC=~A) =====" cycle *pc*))

(defun print-registers ()
  (format t "~&Registers (non-zero):")
  (let ((any nil))
	(loop for i from 0 to 31
		  for v = (aref *register* i)
		  unless (zerop v)
		  do (setf any t)
			 (format t "~&  reg[~2D] = ~A" i v))
	(unless any (format t "~&  (all zero)"))))

(defun print-memory-nonzero ()
  (format t "~&Data memory (non-zero):")
  (let ((any nil))
	(loop for i from 0 below (length *data-memory*)
		  for v = (aref *data-memory* i)
		  unless (zerop v)
		  do (setf any t)
			 (format t "~&  mem[~4D] = ~A" i v))
	(unless any (format t "~&  (all zero)"))))

(defun print-final-state ()
  (format t "~%~%===== Final State =====")
  (format t "~&PC = ~A" *pc*)
  (print-registers)
  (print-memory-nonzero)
  (format t "~%"))

