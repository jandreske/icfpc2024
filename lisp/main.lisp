(defpackage :main
  (:use :cl :icfpc))

(in-package :main)

(defun main (args)
  (icfpc:process args))

(defun start () (main (uiop:command-line-arguments)))

