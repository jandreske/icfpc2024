
(declaim (inline rlogbitp sat-eff7))
(defun rlogbitp (n i) (logbitp i n))

(defun sat-eff7 (n)
  (declare (type (unsigned-byte 48) n)
	   (optimize speed))
  (AND	       
   (OR
    (NOT
     (rlogbitp n (- 18 1)))
    (NOT
     (rlogbitp n (- 15 1))))
   (OR
    (NOT
     (rlogbitp n (- 14 1)))
    (NOT
     (rlogbitp n (- 9 1))))
   (OR
    (NOT
     (rlogbitp n (- 19 1)))
    (rlogbitp n (- 37 1))
    (rlogbitp n (- 12 1)))
   (NOT
    (rlogbitp n (- 39 1)))
   (OR
    (NOT
     (rlogbitp n (- 20 1)))
    (rlogbitp n (- 18 1)))
   (OR
    (NOT
     (rlogbitp n (- 8 1)))
    (rlogbitp n (- 16 1))
    (NOT
     (rlogbitp n (- 24 1))))
   (OR
    (NOT
     (rlogbitp n (- 29 1)))
    (NOT
     (rlogbitp n (- 39 1))))
   (OR
    (NOT
     (rlogbitp n (- 2 1)))
    (rlogbitp n (- 19 1)))
   (rlogbitp n (- 15 1))
   (OR
    (NOT
     (rlogbitp n (- 37 1)))
    (rlogbitp n (- 19 1))
    (NOT
     (rlogbitp n (- 6 1))))
   (OR
    (rlogbitp n (- 25 1))
    (NOT
     (rlogbitp n (- 23 1))))
   (OR
    (NOT
     (rlogbitp n (- 17 1)))
    (rlogbitp n (- 40 1))
    (rlogbitp n (- 21 1)))
   (OR
    (NOT
     (rlogbitp n (- 23 1)))
    (rlogbitp n (- 35 1))
    (rlogbitp n (- 24 1)))
   (OR
    (NOT
     (rlogbitp n (- 30 1)))
    (NOT
     (rlogbitp n (- 28 1))))
   (rlogbitp n (- 15 1))
   (OR
    (NOT
     (rlogbitp n (- 37 1)))
    (rlogbitp n (- 19 1))
    (rlogbitp n (- 6 1)))
   (OR
    (NOT
     (rlogbitp n (- 3 1)))
    (NOT
     (rlogbitp n (- 11 1))))
   (OR
    (NOT
     (rlogbitp n (- 35 1)))
    (NOT
     (rlogbitp n (- 3 1))))
   (OR
    (NOT
     (rlogbitp n (- 29 1)))
    (rlogbitp n (- 39 1))
    (rlogbitp n (- 22 1)))
   (OR
    (NOT
     (rlogbitp n (- 27 1)))
    (NOT
     (rlogbitp n (- 10 1))))
   (OR
    (rlogbitp n (- 28 1))
    (NOT
     (rlogbitp n (- 8 1))))
   (OR
    (NOT
     (rlogbitp n (- 4 1)))
    (rlogbitp n (- 39 1)))
   (OR
    (rlogbitp n (- 10 1))
    (rlogbitp n (- 26 1)))
   (OR
    (rlogbitp n (- 22 1))
    (rlogbitp n (- 14 1))
    (rlogbitp n (- 15 1)))
   (NOT
    (rlogbitp n (- 13 1)))
   (OR
    (rlogbitp n (- 36 1))
    (rlogbitp n (- 28 1))
    (NOT
     (rlogbitp n (- 35 1))))
   (OR
    (NOT
     (rlogbitp n (- 8 1)))
    (rlogbitp n (- 16 1))
    (rlogbitp n (- 24 1)))
   (OR
    (NOT
     (rlogbitp n (- 7 1)))
    (rlogbitp n (- 3 1))
    (NOT
     (rlogbitp n (- 40 1))))
   (OR
    (rlogbitp n (- 22 1))
    (rlogbitp n (- 14 1))
    (rlogbitp n (- 15 1)))
   (NOT
    (rlogbitp n (- 13 1)))
   (OR
    (NOT
     (rlogbitp n (- 8 1)))
    (rlogbitp n (- 16 1))
    (rlogbitp n (- 24 1)))
   (OR
    (NOT
     (rlogbitp n (- 19 1)))
    (rlogbitp n (- 37 1))
    (rlogbitp n (- 12 1)))
   (OR
    (rlogbitp n (- 10 1))
    (NOT
     (rlogbitp n (- 26 1))))
   (OR
    (NOT
     (rlogbitp n (- 20 1)))
    (NOT
     (rlogbitp n (- 18 1))))
   (OR
    (NOT
     (rlogbitp n (- 8 1)))
    (NOT
     (rlogbitp n (- 16 1))))
   (OR
    (NOT
     (rlogbitp n (- 3 1)))
    (rlogbitp n (- 11 1))
    (NOT
     (rlogbitp n (- 23 1))))
   (OR
    (NOT
     (rlogbitp n (- 16 1)))
    (rlogbitp n (- 37 1)))
   (OR
    (NOT
     (rlogbitp n (- 38 1)))
    (NOT
     (rlogbitp n (- 11 1))))
   (OR
    (NOT
     (rlogbitp n (- 31 1)))
    (rlogbitp n (- 13 1))
    (rlogbitp n (- 14 1)))
   (OR
    (NOT
     (rlogbitp n (- 33 1)))
    (rlogbitp n (- 19 1)))
   (OR
    (NOT
     (rlogbitp n (- 14 1)))
    (rlogbitp n (- 9 1))
    (NOT
     (rlogbitp n (- 29 1))))
   (OR
    (rlogbitp n (- 6 1))
    (rlogbitp n (- 16 1)))
   (OR
    (rlogbitp n (- 36 1))
    (rlogbitp n (- 28 1))
    (rlogbitp n (- 35 1)))
   (OR
    (NOT
     (rlogbitp n (- 19 1)))
    (NOT
     (rlogbitp n (- 37 1))))
   (OR
    (NOT
     (rlogbitp n (- 5 1)))
    (rlogbitp n (- 6 1)))
   (OR
    (rlogbitp n (- 10 1))
    (rlogbitp n (- 26 1)))
   (OR
    (NOT
     (rlogbitp n (- 3 1)))
    (rlogbitp n (- 11 1))
    (rlogbitp n (- 23 1)))
   (OR
    (rlogbitp n (- 26 1))
    (rlogbitp n (- 37 1))
    (NOT
     (rlogbitp n (- 16 1))))
   (OR
    (rlogbitp n (- 22 1))
    (rlogbitp n (- 14 1))
    (NOT
     (rlogbitp n (- 15 1))))
   (OR
    (rlogbitp n (- 25 1))
    (rlogbitp n (- 23 1))
    (rlogbitp n (- 38 1)))
   (OR
    (rlogbitp n (- 36 1))
    (NOT
     (rlogbitp n (- 28 1))))
   (OR
    (NOT
     (rlogbitp n (- 35 1)))
    (rlogbitp n (- 3 1)))
   (OR
    (NOT
     (rlogbitp n (- 34 1)))
    (rlogbitp n (- 35 1))
    (NOT
     (rlogbitp n (- 37 1))))
   (OR
    (NOT
     (rlogbitp n (- 5 1)))
    (NOT
     (rlogbitp n (- 6 1))))
   (OR
    (NOT
     (rlogbitp n (- 33 1)))
    (NOT
     (rlogbitp n (- 19 1))))
   (OR
    (NOT
     (rlogbitp n (- 17 1)))
    (rlogbitp n (- 40 1))
    (rlogbitp n (- 21 1)))
   (OR
    (NOT
     (rlogbitp n (- 30 1)))
    (rlogbitp n (- 28 1))
    (rlogbitp n (- 4 1)))
   (OR
    (NOT
     (rlogbitp n (- 34 1)))
    (rlogbitp n (- 35 1))
    (rlogbitp n (- 37 1)))
   (OR
    (rlogbitp n (- 28 1))
    (rlogbitp n (- 8 1))
    (rlogbitp n (- 29 1)))
   (NOT
    (rlogbitp n (- 21 1)))
   (OR
    (NOT
     (rlogbitp n (- 18 1)))
    (rlogbitp n (- 15 1))
    (rlogbitp n (- 39 1)))
   (rlogbitp n (- 40 1))
   (OR
    (NOT
     (rlogbitp n (- 3 1)))
    (rlogbitp n (- 11 1))
    (rlogbitp n (- 23 1)))
   (OR
    (NOT
     (rlogbitp n (- 17 1)))
    (NOT
     (rlogbitp n (- 40 1))))
   (OR
    (NOT
     (rlogbitp n (- 35 1)))
    (rlogbitp n (- 3 1)))
   (OR
    (NOT
     (rlogbitp n (- 33 1)))
    (rlogbitp n (- 19 1)))
   (OR
    (rlogbitp n (- 25 1))
    (rlogbitp n (- 23 1))
    (NOT
     (rlogbitp n (- 38 1))))
   (OR
    (NOT
     (rlogbitp n (- 27 1)))
    (rlogbitp n (- 10 1)))
   (OR
    (rlogbitp n (- 36 1))
    (rlogbitp n (- 28 1))
    (rlogbitp n (- 35 1)))
   (OR
    (NOT
     (rlogbitp n (- 7 1)))
    (rlogbitp n (- 3 1))
    (rlogbitp n (- 40 1)))
   (OR
    (NOT
     (rlogbitp n (- 11 1)))
    (rlogbitp n (- 35 1)))
   (OR
    (rlogbitp n (- 6 1))
    (rlogbitp n (- 16 1)))
   (OR
    (NOT
     (rlogbitp n (- 30 1)))
    (rlogbitp n (- 28 1))
    (rlogbitp n (- 4 1)))
   (OR
    (NOT
     (rlogbitp n (- 11 1)))
    (NOT
     (rlogbitp n (- 35 1))))
   (OR
    (NOT
     (rlogbitp n (- 32 1)))
    (rlogbitp n (- 19 1)))
   (OR
    (NOT
     (rlogbitp n (- 37 1)))
    (NOT
     (rlogbitp n (- 19 1))))
   (OR
    (rlogbitp n (- 26 1))
    (NOT
     (rlogbitp n (- 37 1))))
   (OR
    (NOT
     (rlogbitp n (- 30 1)))
    (rlogbitp n (- 28 1))
    (NOT
     (rlogbitp n (- 4 1))))
   (NOT
    (rlogbitp n (- 21 1)))
   (OR
    (NOT
     (rlogbitp n (- 20 1)))
    (rlogbitp n (- 18 1)))
   (OR
    (NOT
     (rlogbitp n (- 9 1)))
    (rlogbitp n (- 31 1)))
   (OR
    (rlogbitp n (- 28 1))
    (rlogbitp n (- 8 1))
    (rlogbitp n (- 29 1)))
   (OR
    (NOT
     (rlogbitp n (- 31 1)))
    (rlogbitp n (- 13 1))
    (NOT
     (rlogbitp n (- 14 1))))
   (OR
    (NOT
     (rlogbitp n (- 24 1)))
    (rlogbitp n (- 32 1))
    (rlogbitp n (- 35 1)))
   (OR
    (rlogbitp n (- 12 1))
    (rlogbitp n (- 16 1)))
   (OR
    (NOT
     (rlogbitp n (- 29 1)))
    (rlogbitp n (- 39 1))
    (rlogbitp n (- 22 1)))
   (OR
    (NOT
     (rlogbitp n (- 14 1)))
    (rlogbitp n (- 9 1))
    (rlogbitp n (- 29 1)))
   (NOT
    (rlogbitp n (- 39 1)))
   (OR
    (NOT
     (rlogbitp n (- 4 1)))
    (NOT
     (rlogbitp n (- 39 1))))
   (OR
    (NOT
     (rlogbitp n (- 37 1)))
    (rlogbitp n (- 19 1))
    (rlogbitp n (- 6 1)))
   (OR
    (NOT
     (rlogbitp n (- 23 1)))
    (NOT
     (rlogbitp n (- 35 1))))
   (OR
    (NOT
     (rlogbitp n (- 29 1)))
    (rlogbitp n (- 39 1))
    (NOT
     (rlogbitp n (- 22 1))))
   (OR
    (NOT
     (rlogbitp n (- 5 1)))
    (rlogbitp n (- 6 1)))
   (rlogbitp n (- 40 1))
   (OR
    (rlogbitp n (- 25 1))
    (rlogbitp n (- 23 1))
    (rlogbitp n (- 38 1)))
   (OR
    (NOT
     (rlogbitp n (- 2 1)))
    (rlogbitp n (- 19 1)))
   (OR
    (rlogbitp n (- 12 1))
    (rlogbitp n (- 16 1)))
   (OR
    (NOT
     (rlogbitp n (- 23 1)))
    (rlogbitp n (- 35 1))
    (rlogbitp n (- 24 1)))
   (OR
    (NOT
     (rlogbitp n (- 31 1)))
    (NOT
     (rlogbitp n (- 13 1))))
   (OR
    (NOT
     (rlogbitp n (- 34 1)))
    (NOT
     (rlogbitp n (- 35 1))))
   (OR
    (NOT
     (rlogbitp n (- 32 1)))
    (rlogbitp n (- 19 1)))
   (OR
    (NOT
     (rlogbitp n (- 24 1)))
    (rlogbitp n (- 32 1))
    (rlogbitp n (- 35 1)))
   (OR
    (rlogbitp n (- 26 1))
    (rlogbitp n (- 37 1))
    (rlogbitp n (- 16 1)))
   (OR
    (NOT
     (rlogbitp n (- 38 1)))
    (rlogbitp n (- 11 1)))
   (OR
    (NOT
     (rlogbitp n (- 16 1)))
    (rlogbitp n (- 37 1)))
   (OR
    (NOT
     (rlogbitp n (- 7 1)))
    (rlogbitp n (- 3 1))
    (rlogbitp n (- 40 1)))
   (OR
    (NOT
     (rlogbitp n (- 4 1)))
    (rlogbitp n (- 39 1)))
   (OR
    (NOT
     (rlogbitp n (- 7 1)))
    (NOT
     (rlogbitp n (- 3 1))))
   (OR
    (NOT
     (rlogbitp n (- 24 1)))
    (rlogbitp n (- 32 1))
    (NOT
     (rlogbitp n (- 35 1))))
   (OR
    (NOT
     (rlogbitp n (- 34 1)))
    (rlogbitp n (- 35 1))
    (rlogbitp n (- 37 1)))
   (OR
    (NOT
     (rlogbitp n (- 11 1)))
    (rlogbitp n (- 35 1)))
   (rlogbitp n (- 1 1))
   (OR
    (NOT
     (rlogbitp n (- 27 1)))
    (rlogbitp n (- 10 1)))
   (OR
    (NOT
     (rlogbitp n (- 23 1)))
    (rlogbitp n (- 35 1))
    (NOT
     (rlogbitp n (- 24 1))))
   (OR
    (NOT
     (rlogbitp n (- 18 1)))
    (rlogbitp n (- 15 1))
    (NOT
     (rlogbitp n (- 39 1))))
   (OR
    (NOT
     (rlogbitp n (- 19 1)))
    (rlogbitp n (- 37 1))
    (NOT
     (rlogbitp n (- 12 1))))
   (OR
    (rlogbitp n (- 26 1))
    (rlogbitp n (- 37 1))
    (rlogbitp n (- 16 1)))
   (OR
    (rlogbitp n (- 6 1))
    (NOT
     (rlogbitp n (- 16 1))))
   (OR
    (NOT
     (rlogbitp n (- 17 1)))
    (rlogbitp n (- 40 1))
    (NOT
     (rlogbitp n (- 21 1))))
   (OR
    (NOT
     (rlogbitp n (- 9 1)))
    (NOT
     (rlogbitp n (- 31 1))))
   (OR
    (NOT
     (rlogbitp n (- 32 1)))
    (NOT
     (rlogbitp n (- 19 1))))
   (OR
    (NOT
     (rlogbitp n (- 38 1)))
    (rlogbitp n (- 11 1)))
   (OR
    (NOT
     (rlogbitp n (- 9 1)))
    (rlogbitp n (- 31 1)))
   (OR
    (NOT
     (rlogbitp n (- 14 1)))
    (rlogbitp n (- 9 1))
    (rlogbitp n (- 29 1)))
   (OR
    (NOT
     (rlogbitp n (- 18 1)))
    (rlogbitp n (- 15 1))
    (rlogbitp n (- 39 1)))
   (OR
    (rlogbitp n (- 12 1))
    (NOT
     (rlogbitp n (- 16 1))))
   (OR
    (NOT
     (rlogbitp n (- 16 1)))
    (NOT
     (rlogbitp n (- 37 1))))
   (OR
    (rlogbitp n (- 22 1))
    (NOT
     (rlogbitp n (- 14 1))))
   (OR
    (rlogbitp n (- 28 1))
    (rlogbitp n (- 8 1))
    (NOT
     (rlogbitp n (- 29 1))))
   (rlogbitp n (- 1 1))
   (OR
    (NOT
     (rlogbitp n (- 24 1)))
    (NOT
     (rlogbitp n (- 32 1))))
   (OR
    (NOT
     (rlogbitp n (- 2 1)))
    (NOT
     (rlogbitp n (- 19 1))))
   (OR
    (NOT
     (rlogbitp n (- 31 1)))
    (rlogbitp n (- 13 1))
    (rlogbitp n (- 14 1)))))

(defun eff7 (start)
  (declare (optimize speed))
  (declare (type (unsigned-byte 48) start))
  (do ((i start (+ i 1)))
      ((sat-eff7 i) i)))

