(defpackage :icfplang-tests
  (:use :cl :fiveam :icfplang))

(in-package :icfplang-tests)

(def-suite icfplang-tests)
(in-suite icfplang-tests)

(test parse
      (is (equal '(apply
		   (lambda 2 (+ (icfplang::var 2) (icfplang::var 2)))
		   3)
		 (parse-program "B$ L# B+ v# v# I$"))))

(test flatten
      (is (equal '(and (or 0 1) (or 11 22) 3 4 5)
		 (icfplang::flatten-app '(and (and (and (and (or 0 1) (or 11 22)) 3) 4) 5)))))

(test join-lambdas
      (is (equal '(lambda (3 1 2) (icfplang::var 1))
		 (icfplang::join-lambdas
		  '(lambda 3 (lambda 1 (lambda 2 (icfplang::var 1))))))))
