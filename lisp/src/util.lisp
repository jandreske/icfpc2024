(defpackage :util
  (:use :cl)
  (:export :find-repetitions))

(in-package :util)


(defun find-repetitions (seq)
  (labels ((reps1 (count item s)
	     (cond ((null s) (list (cons count item)))
		   ((equal item (car s)) (reps1 (+ count 1) item (cdr s)))
		   (t (cons (cons count item) (reps1 1 (car s) (cdr s))))))
	   (reps (s)
	     (if (null s)
		 nil
		 (reps1 1 (car s) (cdr s)))))
    (reps (coerce seq 'list))))


	 
