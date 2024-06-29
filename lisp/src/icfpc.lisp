(defpackage :icfpc
  (:use :cl :icfplang :lambdaman)
  (:import-from :quri :make-uri)
  
  (:export :process))

(in-package :icfpc)

(defparameter *verbose* t)

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
	   (multiple-value-bind (value reductions)
	       (eval-icfp parsed)
	     (format t "~%VALUE: ~S~%REDUCTIONS: ~S~%" (icfp->ascii value) reductions))))
	((string= (car args) "solve-lambdaman")
	 (let* ((problem-number (cadr args))
		(problem-file (concatenate 'string (uiop:getenv "ICFPC_HOME")
					   "/courses/lambdaman/problems/lambdaman"
					   problem-number
					   ".txt"))
		(grid (read-grid problem-file))
		(moves (path-to-pills grid)))
	   (format t "Map size: ~Ax~A~%" (grid-width grid) (grid-height grid))
	   (format t "Moves: ~A~%" (length moves))
	   (send (list (ascii->icfp
			(concatenate 'string "solve lambdaman" problem-number " " moves))))))
	(t (usage))))


(defun send (icfp-program)
  (let ((uri (make-uri :defaults "https://boundvariable.space/communicate"))
	(headers `(("Authorization" . ,(concatenate 'string "Bearer " (uiop:getenv "ICFPC_TOKEN")))))
	(body (icfplang:encode icfp-program)))
    (declare (type string body))
    (multiple-value-bind (text http-code http-headers quri x)
	(dex:post uri :headers headers :content body)
      (declare (ignore http-headers quri x))
      (when *verbose*
	(format t "SENT: ~S~%RECV: ~S~%" body text))
      (multiple-value-bind (value reductions)
	  (eval-icfp (parse-program text))
	(when *verbose*
	  (format t "EVAL: ~S~%REDUCTIONS: ~A~%" (icfp->ascii value) reductions))
	value))))
