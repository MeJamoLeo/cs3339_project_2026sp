;; Entry point for the class demo.
;;
;; Usage from the shell:
;;   sbcl --non-interactive --load demo.lisp --eval '(demo "./input")'
;;
;; Usage from the REPL:
;;   (load "demo.lisp")
;;   (demo "./input")          ;; per-cycle debug + final state
;;   (demo "./input" :debug nil)  ;; final state only

(load "main.lisp")
(load "debug.lisp")

(defun reset-all ()
  "Reset every piece of simulator state so a new program starts clean."
  (setf *pc* 0
		*if-id* nil *id-ex* nil *ex-mem* nil *mem-wb* nil)
  (fill *register* 0)
  (fill *data-memory* 0))

(defun load-assembly-file (path)
  "Parse an assembly file and load both the encoded instructions and the
original source text. The source vector is parallel to the instruction
memory and used by the debug layer for human-readable trace output."
  (let* ((lines  (read-assembly path))
		 (parsed (mapcar #'split-by-spaces lines)))
	(setf *instruction-memory*
		  (coerce (mapcar #'encode parsed) 'vector))
	(setf *instruction-source*
		  (coerce (mapcar (lambda (s) (string-trim " " s)) lines)
				  'vector))))

(defun demo (path &key (debug nil))
  "Run an assembly file through the pipelined simulator.
When :debug is true, print the state of all pipeline registers
after every cycle. Always print the final register and memory
state at the end."
  (reset-all)
  (load-assembly-file path)
  (loop for cycle from 1
		until (pipeline-drained-p)
		for fetch-pc = *pc*
		do (pipeline-cycle)
		   (when debug
			 (print-cycle-header cycle fetch-pc)
			 (print-pipeline-state)))
  (print-final-state))
