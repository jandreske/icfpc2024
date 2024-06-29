(defpackage :icfpc
  (:use :cl :icfplang :lambdaman)
  (:export :process))

(in-package :icfpc)

(defun usage ()
  (format t "Usage: icfpc <command> [arg...]

Possible commands:
  eval-file <file>
  solve-lambdaman <file>
"))

(defun process (args)
  (cond ((null args) (usage))
	((string= (car args) "eval-file")
	 (let* ((program (alexandria:read-file-into-string (cadr args)))
		(parsed (parse-program program)))
	   (format t "PROGRAM: ~S~%~%PARSED: ~S~%"
		   program parsed)
	   (multiple-value-bind (value reductions) (eval-icfp parsed)
	     (let ((value (if (stringp value) (icfp->ascii value) value)))
	       (format t "~%VALUE: ~S~%REDUCTIONS: ~S~%" value reductions)))))
	((string= (car args) "solve-lambdaman")
	 (let ((grid (read-grid (cadr args))))
	   ;; (format t "Map size: ~Ax~A~%" (grid-width grid) (grid-height grid))
	   (format t "~A~%" (path-to-pills grid))))
	(t (usage))))
