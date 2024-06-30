(defpackage :spaceship-tests
  (:use :cl :fiveam :spaceship))

(in-package :spaceship-tests)

(def-suite spaceship-tests)
(in-suite spaceship-tests)

(test keys
      (is (char= #\9 (spaceship::get-key 1 1)))
      (is (char= #\7 (spaceship::get-key -1 1)))
      (is (char= #\2 (spaceship::get-key 0 -1))))

(test accels
      (is (equal '(1 -1) (spaceship::path 5 2))))
