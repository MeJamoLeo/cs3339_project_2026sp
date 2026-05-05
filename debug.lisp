;; Reverse lookup: register number -> symbolic name (e.g. 8 -> "$t0").
(defparameter *register-names*
  (let ((names (make-array 32 :initial-element nil)))
	(maphash (lambda (name num) (setf (aref names num) name))
			 *register-table*)
	names))

(defun reg-name (num)
  (or (aref *register-names* num) (format nil "$~A" num)))

;; Side table holding the original assembly source for each instruction.
;; Indexed by inst-id (= PC/4) so the debugger can recover the mnemonic
;; without polluting the pipeline with strings.
(defparameter *instruction-source* #())

(defun source-of (inst-id)
  (if (and inst-id (< inst-id (length *instruction-source*)))
	  (aref *instruction-source* inst-id)
	  "?"))

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

(defun stage-tag (preg)
  "Format the [#id source] tag for a pipeline register."
  (let ((id (getf preg :inst-id)))
	(format nil "[#~A ~A]" id (source-of id))))

(defun print-pipeline-state ()
  (format t "~&  IF/ID : ~A"
		  (if *if-id*
			  (format nil "~A  instr=#x~8,'0X  pc+4=~A"
					  (stage-tag *if-id*)
					  (getf *if-id* :instruction)
					  (getf *if-id* :pc+4))
			  "(bubble)"))
  (format t "~&  ID/EX : ~A"
		  (if *id-ex*
			  (format nil "~A  rs=~A rt=~A signExt=~A shamt=~A funct=~A | ~A"
					  (stage-tag *id-ex*)
					  (getf *id-ex* :data-reg-read1)
					  (getf *id-ex* :data-reg-read2)
					  (getf *id-ex* :sign-extended)
					  (getf *id-ex* :shamt)
					  (getf *id-ex* :funct)
					  (format-control-signals (getf *id-ex* :control-signals)))
			  "(bubble)"))
  (format t "~&  EX/MEM: ~A"
		  (if *ex-mem*
			  (format nil "~A  aluResult=~A zero=~A brTarget=~A writeReg=~A | ~A"
					  (stage-tag *ex-mem*)
					  (getf *ex-mem* :alu-result)
					  (getf *ex-mem* :alu-zero)
					  (getf *ex-mem* :branch-target)
					  (getf *ex-mem* :write-reg)
					  (format-control-signals (getf *ex-mem* :control-signals)))
			  "(bubble)"))
  (format t "~&  MEM/WB: ~A"
		  (if *mem-wb*
			  (format nil "~A  memData=~A aluResult=~A writeReg=~A | ~A"
					  (stage-tag *mem-wb*)
					  (getf *mem-wb* :mem-data)
					  (getf *mem-wb* :alu-result)
					  (getf *mem-wb* :write-reg)
					  (format-control-signals (getf *mem-wb* :control-signals)))
			  "(bubble)")))

(defun print-cycle-header (cycle fetch-pc)
  ;; fetch-pc is the PC used for IF this cycle (captured before the clock
  ;; edge), not the post-edge *pc* which is already the next cycle's PC.
  (format t "~%~%===== Cycle ~A  (fetch PC=#x~X) =====" cycle fetch-pc))

(defun snapshot-registers ()
  "Return a fresh copy of the register file for diffing across cycles."
  (copy-seq *register*))

(defun print-registers-grid (&optional prev)
  "Print all 32 registers in an 8-column grid. If PREV is supplied,
mark registers whose value changed since the last snapshot with '*'."
  (format t "~&  Registers:")
  (loop for row from 0 below 4 do
		(format t "~&   ")
		(loop for col from 0 below 8
			  for i = (+ (* row 8) col)
			  for v = (aref *register* i)
			  for changed = (and prev (/= v (aref prev i)))
			  do (format t " ~6A=~5D~A"
						 (reg-name i) v
						 (if changed "*" " ")))))

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

