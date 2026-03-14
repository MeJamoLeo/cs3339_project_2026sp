;; adder
(defun adder (a b)
  (+ a b))

;; Read input file
;; TODO: devide this in two functions; one is read assembly, the other is instruction encoder
(defun read-assembly (path)
  (with-open-file (input-stream path)
	(loop for line = (read-line input-stream nil)
		  while line
		  collect line)))

(defun split-by-spaces (line)
    (loop for i = 0 then (1+ j)
          as j = (position #\Space line :start i)
          as token = (subseq line i j)
		  when (string/= token "")
		  collect token
          while j))

(pprint(split-by-one-space "add   $s0 $t0 $t1"))
