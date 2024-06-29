(defpackage :binary-heap-tests
  (:use :cl :fiveam :binary-heap))

(in-package :binary-heap-tests)

(def-suite binary-heap-tests)
(in-suite binary-heap-tests)

(test basics
  (let ((heap (make-binary-heap #'<)))
    (is (= 0 (binary-heap-size heap)))
    (binary-heap-insert heap 5)
    (binary-heap-insert heap 1)
    (binary-heap-insert heap 2)
    (binary-heap-insert heap 7)
    (binary-heap-insert heap 6)
    (is (= 5 (binary-heap-size heap)))
    (is (= 1 (binary-heap-take-min heap)))
    (is (= 4 (binary-heap-size heap)))    
    (is (= 2 (binary-heap-take-min heap)))
    (is (= 5 (binary-heap-take-min heap)))
    (is (= 2 (binary-heap-size heap)))
    ))


 
