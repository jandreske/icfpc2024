(defvar sat-result
  '(1 -2 3 4 5 6 7 8 -9 -10 11 -12 13 -14 -15 -16 -17 18 19 -20 -21 22 -23 -24 -25 26 -27 -28 -29 -30 -31 -32 -33 34 35 36 -37 38 -39 -40 -41 -42 -43 -44 -45 -46 47 48))
  

(defun to-num (s n)
  (if (null s)
      n
      (if (< (car s) 0)
	  (to-num (cdr s) n)
	  (to-num (cdr s) (+ n (ash 1 (car s)))))))


(defun sat-eff8 (n)
  (declare (type fixnum n)
	   (optimize speed))
  (AND
   (OR (NOT (LOGBITP 2 N)) (LOGBITP 35 N) (LOGBITP 6 N))
   (OR (NOT (LOGBITP 2 N)) (NOT (LOGBITP 41 N)) (NOT (LOGBITP 47 N)))
   (OR (NOT (LOGBITP 48 N)) (NOT (LOGBITP 46 N)) (NOT (LOGBITP 40 N)))
   (OR (LOGBITP 7 N) (NOT (LOGBITP 39 N)) (LOGBITP 16 N))
   (OR (NOT (LOGBITP 20 N)) (NOT (LOGBITP 30 N)) (NOT (LOGBITP 38 N)))
   (OR (LOGBITP 35 N) (NOT (LOGBITP 21 N)) (LOGBITP 48 N))
   (OR (LOGBITP 26 N) (LOGBITP 37 N) (LOGBITP 13 N))
   (OR (LOGBITP 14 N) (NOT (LOGBITP 17 N)) (LOGBITP 5 N))
   (OR (LOGBITP 5 N) (LOGBITP 6 N) (NOT (LOGBITP 42 N)))
   (OR (LOGBITP 33 N) (NOT (LOGBITP 6 N)) (LOGBITP 22 N))
   (OR (LOGBITP 1 N) (LOGBITP 13 N) (NOT (LOGBITP 12 N)))
   (OR (LOGBITP 1 N) (LOGBITP 46 N) (NOT (LOGBITP 41 N)))
   (OR (NOT (LOGBITP 32 N)) (NOT (LOGBITP 34 N)) (LOGBITP 2 N))
   (OR (LOGBITP 43 N) (LOGBITP 39 N) (LOGBITP 48 N))
   (OR (LOGBITP 49 N) (LOGBITP 35 N) (LOGBITP 30 N))
   (OR (NOT (LOGBITP 35 N)) (NOT (LOGBITP 2 N)) (NOT (LOGBITP 36 N)))
   (OR (LOGBITP 25 N) (NOT (LOGBITP 28 N)) (LOGBITP 42 N))
   (OR (LOGBITP 14 N) (LOGBITP 28 N) (NOT (LOGBITP 44 N)))
   (OR (LOGBITP 23 N) (NOT (LOGBITP 10 N)) (LOGBITP 17 N))
   (OR (NOT (LOGBITP 46 N)) (NOT (LOGBITP 25 N)) (LOGBITP 5 N))
   (OR (NOT (LOGBITP 49 N)) (NOT (LOGBITP 32 N)) (NOT (LOGBITP 9 N)))
   (OR (LOGBITP 31 N) (LOGBITP 5 N) (LOGBITP 15 N))
   (OR (NOT (LOGBITP 33 N)) (LOGBITP 36 N) (LOGBITP 40 N))
   (OR (LOGBITP 6 N) (NOT (LOGBITP 27 N)) (NOT (LOGBITP 16 N)))
   (OR (NOT (LOGBITP 43 N)) (LOGBITP 45 N) (LOGBITP 18 N))
   (OR (LOGBITP 6 N) (LOGBITP 21 N) (NOT (LOGBITP 47 N)))
   (OR (LOGBITP 2 N) (LOGBITP 38 N) (LOGBITP 33 N))
   (OR (LOGBITP 30 N) (LOGBITP 45 N) (NOT (LOGBITP 42 N)))
   (OR (NOT (LOGBITP 26 N)) (LOGBITP 31 N) (LOGBITP 22 N))
   (OR (LOGBITP 36 N) (NOT (LOGBITP 49 N)) (NOT (LOGBITP 17 N)))
   (OR (LOGBITP 19 N) (LOGBITP 4 N) (LOGBITP 10 N))
   (OR (NOT (LOGBITP 44 N)) (NOT (LOGBITP 23 N)) (LOGBITP 5 N))
   (OR (NOT (LOGBITP 33 N)) (NOT (LOGBITP 22 N)) (NOT (LOGBITP 13 N)))
   (OR (NOT (LOGBITP 21 N)) (LOGBITP 20 N) (LOGBITP 19 N))
   (OR (NOT (LOGBITP 16 N)) (LOGBITP 49 N) (LOGBITP 23 N))
   (OR (NOT (LOGBITP 24 N)) (NOT (LOGBITP 23 N)) (NOT (LOGBITP 26 N)))
   (OR (LOGBITP 2 N) (LOGBITP 34 N) (LOGBITP 20 N))
   (OR (NOT (LOGBITP 25 N)) (LOGBITP 46 N) (NOT (LOGBITP 35 N)))
   (OR (NOT (LOGBITP 27 N)) (NOT (LOGBITP 44 N)) (LOGBITP 48 N))
   (OR (NOT (LOGBITP 20 N)) (NOT (LOGBITP 5 N)) (LOGBITP 11 N))
   (OR (NOT (LOGBITP 16 N)) (NOT (LOGBITP 14 N)) (NOT (LOGBITP 38 N)))
   (OR (LOGBITP 40 N) (LOGBITP 1 N) (NOT (LOGBITP 13 N)))
   (OR (LOGBITP 24 N) (LOGBITP 35 N) (NOT (LOGBITP 22 N)))
   (OR (NOT (LOGBITP 38 N)) (NOT (LOGBITP 2 N)) (NOT (LOGBITP 39 N)))
   (OR (LOGBITP 49 N) (LOGBITP 19 N) (LOGBITP 34 N))
   (OR (LOGBITP 26 N) (LOGBITP 30 N) (NOT (LOGBITP 38 N)))
   (OR (LOGBITP 44 N) (NOT (LOGBITP 14 N)) (NOT (LOGBITP 39 N)))
   (OR (LOGBITP 33 N) (LOGBITP 49 N) (LOGBITP 34 N))
   (OR (NOT (LOGBITP 0 N)) (NOT (LOGBITP 47 N)) (LOGBITP 11 N))
   (OR (LOGBITP 17 N) (NOT (LOGBITP 34 N)) (NOT (LOGBITP 29 N)))
   (OR (LOGBITP 26 N) (NOT (LOGBITP 23 N)) (NOT (LOGBITP 24 N)))
   (OR (NOT (LOGBITP 3 N)) (NOT (LOGBITP 32 N)) (NOT (LOGBITP 11 N)))
   (OR (NOT (LOGBITP 42 N)) (NOT (LOGBITP 23 N)) (NOT (LOGBITP 36 N)))
   (OR (NOT (LOGBITP 36 N)) (LOGBITP 30 N) (NOT (LOGBITP 43 N)))
   (OR (NOT (LOGBITP 8 N)) (NOT (LOGBITP 37 N)) (LOGBITP 13 N))
   (OR (LOGBITP 32 N) (NOT (LOGBITP 15 N)) (LOGBITP 33 N))
   (OR (LOGBITP 3 N) (NOT (LOGBITP 34 N)) (NOT (LOGBITP 4 N)))
   (OR (NOT (LOGBITP 2 N)) (NOT (LOGBITP 20 N)) (NOT (LOGBITP 18 N)))
   (OR (NOT (LOGBITP 34 N)) (NOT (LOGBITP 35 N)) (NOT (LOGBITP 28 N)))
   (OR (LOGBITP 6 N) (NOT (LOGBITP 42 N)) (LOGBITP 35 N))
   (OR (LOGBITP 29 N) (LOGBITP 13 N) (LOGBITP 40 N))
   (OR (NOT (LOGBITP 34 N)) (NOT (LOGBITP 23 N)) (NOT (LOGBITP 6 N)))
   (OR (LOGBITP 34 N) (NOT (LOGBITP 41 N)) (LOGBITP 5 N))
   (OR (NOT (LOGBITP 0 N)) (NOT (LOGBITP 14 N)) (LOGBITP 38 N))
   (OR (LOGBITP 26 N) (LOGBITP 48 N) (NOT (LOGBITP 15 N)))
   (OR (NOT (LOGBITP 36 N)) (LOGBITP 48 N) (NOT (LOGBITP 9 N)))
   (OR (LOGBITP 49 N) (NOT (LOGBITP 45 N)) (NOT (LOGBITP 2 N)))
   (OR (NOT (LOGBITP 40 N)) (LOGBITP 19 N) (LOGBITP 33 N))
   (OR (NOT (LOGBITP 0 N)) (LOGBITP 22 N) (LOGBITP 27 N))
   (OR (NOT (LOGBITP 11 N)) (NOT (LOGBITP 29 N)) (NOT (LOGBITP 19 N)))
   (OR (NOT (LOGBITP 23 N)) (LOGBITP 28 N) (NOT (LOGBITP 36 N)))
   (OR (LOGBITP 11 N) (LOGBITP 4 N) (NOT (LOGBITP 43 N)))
   (OR (NOT (LOGBITP 5 N)) (NOT (LOGBITP 1 N)) (LOGBITP 47 N))
   (OR (NOT (LOGBITP 1 N)) (NOT (LOGBITP 48 N)) (NOT (LOGBITP 42 N)))
   (OR (LOGBITP 0 N) (NOT (LOGBITP 49 N)) (LOGBITP 23 N))
   (OR (NOT (LOGBITP 6 N)) (NOT (LOGBITP 49 N)) (NOT (LOGBITP 43 N)))
   (OR (NOT (LOGBITP 40 N)) (LOGBITP 42 N) (LOGBITP 3 N))
   (OR (LOGBITP 12 N) (LOGBITP 14 N) (NOT (LOGBITP 10 N)))
   (OR (NOT (LOGBITP 2 N)) (NOT (LOGBITP 10 N)) (LOGBITP 22 N))
   (OR (LOGBITP 32 N) (LOGBITP 47 N) (LOGBITP 40 N))
   (OR (LOGBITP 8 N) (LOGBITP 22 N) (NOT (LOGBITP 48 N)))
   (OR (NOT (LOGBITP 42 N)) (LOGBITP 46 N) (LOGBITP 0 N))
   (OR (NOT (LOGBITP 39 N)) (LOGBITP 15 N) (NOT (LOGBITP 28 N)))
   (OR (LOGBITP 29 N) (LOGBITP 18 N) (LOGBITP 2 N))
   (OR (LOGBITP 18 N) (NOT (LOGBITP 33 N)) (LOGBITP 47 N))
   (OR (NOT (LOGBITP 15 N)) (NOT (LOGBITP 43 N)) (LOGBITP 13 N))
   (OR (LOGBITP 37 N) (NOT (LOGBITP 44 N)) (NOT (LOGBITP 11 N)))
   (OR (NOT (LOGBITP 3 N)) (NOT (LOGBITP 13 N)) (NOT (LOGBITP 30 N)))
   (OR (NOT (LOGBITP 47 N)) (LOGBITP 34 N) (NOT (LOGBITP 0 N)))
   (OR (LOGBITP 44 N) (NOT (LOGBITP 12 N)) (LOGBITP 18 N))
   (OR (LOGBITP 8 N) (LOGBITP 41 N) (NOT (LOGBITP 6 N)))
   (OR (NOT (LOGBITP 0 N)) (NOT (LOGBITP 14 N)) (LOGBITP 7 N))
   (OR (NOT (LOGBITP 12 N)) (NOT (LOGBITP 43 N)) (NOT (LOGBITP 13 N)))
   (OR (NOT (LOGBITP 42 N)) (NOT (LOGBITP 36 N)) (NOT (LOGBITP 30 N)))
   (OR (NOT (LOGBITP 26 N)) (NOT (LOGBITP 28 N)) (LOGBITP 46 N))
   (OR (LOGBITP 6 N) (LOGBITP 3 N) (LOGBITP 16 N))
   (OR (LOGBITP 6 N) (LOGBITP 9 N) (LOGBITP 34 N))
   (OR (NOT (LOGBITP 24 N)) (LOGBITP 19 N) (LOGBITP 16 N))
   (OR (LOGBITP 34 N) (NOT (LOGBITP 4 N)) (NOT (LOGBITP 41 N)))
   (OR (NOT (LOGBITP 49 N)) (LOGBITP 23 N) (NOT (LOGBITP 4 N)))
   (OR (NOT (LOGBITP 20 N)) (NOT (LOGBITP 25 N)) (LOGBITP 1 N))
   (OR (NOT (LOGBITP 7 N)) (LOGBITP 44 N) (NOT (LOGBITP 20 N)))
   (OR (NOT (LOGBITP 15 N)) (LOGBITP 32 N) (LOGBITP 48 N))
   (OR (NOT (LOGBITP 37 N)) (LOGBITP 5 N) (LOGBITP 15 N))
   (OR (LOGBITP 4 N) (LOGBITP 20 N) (LOGBITP 36 N))
   (OR (LOGBITP 7 N) (LOGBITP 37 N) (LOGBITP 30 N))
   (OR (NOT (LOGBITP 20 N)) (LOGBITP 32 N) (LOGBITP 13 N))
   (OR (LOGBITP 19 N) (LOGBITP 39 N) (NOT (LOGBITP 4 N)))
   (OR (NOT (LOGBITP 28 N)) (NOT (LOGBITP 8 N)) (LOGBITP 30 N))
   (OR (NOT (LOGBITP 6 N)) (LOGBITP 41 N) (NOT (LOGBITP 21 N)))
   (OR (NOT (LOGBITP 47 N)) (LOGBITP 7 N) (LOGBITP 25 N))
   (OR (LOGBITP 47 N) (NOT (LOGBITP 37 N)) (LOGBITP 32 N))
   (OR (NOT (LOGBITP 33 N)) (LOGBITP 48 N) (LOGBITP 45 N))
   (OR (NOT (LOGBITP 13 N)) (NOT (LOGBITP 45 N)) (LOGBITP 24 N))
   (OR (NOT (LOGBITP 45 N)) (LOGBITP 3 N) (LOGBITP 17 N))
   (OR (LOGBITP 35 N) (NOT (LOGBITP 11 N)) (NOT (LOGBITP 30 N)))
   (OR (LOGBITP 11 N) (NOT (LOGBITP 17 N)) (LOGBITP 13 N))
   (OR (NOT (LOGBITP 6 N)) (LOGBITP 45 N) (NOT (LOGBITP 15 N)))
   (OR (LOGBITP 8 N) (NOT (LOGBITP 7 N)) (LOGBITP 6 N))
   (OR (LOGBITP 48 N) (NOT (LOGBITP 41 N)) (NOT (LOGBITP 21 N)))
   (OR (LOGBITP 21 N) (NOT (LOGBITP 14 N)) (LOGBITP 37 N))
   (OR (LOGBITP 33 N) (NOT (LOGBITP 40 N)) (LOGBITP 46 N))
   (OR (LOGBITP 21 N) (NOT (LOGBITP 25 N)) (LOGBITP 31 N))
   (OR (NOT (LOGBITP 24 N)) (NOT (LOGBITP 44 N)) (NOT (LOGBITP 20 N)))
   (OR (NOT (LOGBITP 25 N)) (LOGBITP 31 N) (NOT (LOGBITP 10 N)))
   (OR (LOGBITP 14 N) (LOGBITP 25 N) (NOT (LOGBITP 24 N)))
   (OR (NOT (LOGBITP 0 N)) (LOGBITP 45 N) (LOGBITP 24 N))
   (OR (NOT (LOGBITP 13 N)) (NOT (LOGBITP 30 N)) (LOGBITP 29 N))
   (OR (NOT (LOGBITP 8 N)) (NOT (LOGBITP 21 N)) (LOGBITP 11 N))
   (OR (NOT (LOGBITP 17 N)) (LOGBITP 25 N) (NOT (LOGBITP 34 N)))
   (OR (NOT (LOGBITP 15 N)) (NOT (LOGBITP 31 N)) (NOT (LOGBITP 20 N)))
   (OR (LOGBITP 30 N) (NOT (LOGBITP 48 N)) (NOT (LOGBITP 20 N)))
   (OR (LOGBITP 10 N) (LOGBITP 8 N) (LOGBITP 40 N))
   (OR (NOT (LOGBITP 12 N)) (NOT (LOGBITP 29 N)) (LOGBITP 18 N))
   (OR (NOT (LOGBITP 9 N)) (LOGBITP 3 N) (LOGBITP 5 N))
   (OR (NOT (LOGBITP 3 N)) (LOGBITP 2 N) (NOT (LOGBITP 21 N)))
   (OR (NOT (LOGBITP 24 N)) (NOT (LOGBITP 49 N)) (NOT (LOGBITP 17 N)))
   (OR (NOT (LOGBITP 39 N)) (LOGBITP 3 N) (LOGBITP 8 N))
   (OR (LOGBITP 36 N) (LOGBITP 19 N) (LOGBITP 45 N))
   (OR (NOT (LOGBITP 26 N)) (LOGBITP 21 N) (NOT (LOGBITP 28 N)))
   (OR (LOGBITP 33 N) (LOGBITP 13 N) (LOGBITP 2 N))
   (OR (LOGBITP 2 N) (NOT (LOGBITP 30 N)) (LOGBITP 19 N))
   (OR (NOT (LOGBITP 49 N)) (LOGBITP 1 N) (NOT (LOGBITP 25 N)))
   (OR (LOGBITP 16 N) (NOT (LOGBITP 28 N)) (LOGBITP 37 N))
   (OR (NOT (LOGBITP 48 N)) (LOGBITP 11 N) (NOT (LOGBITP 40 N)))
   (OR (LOGBITP 14 N) (NOT (LOGBITP 34 N)) (NOT (LOGBITP 42 N)))
   (OR (NOT (LOGBITP 21 N)) (NOT (LOGBITP 22 N)) (NOT (LOGBITP 48 N)))
   (OR (NOT (LOGBITP 8 N)) (LOGBITP 32 N) (LOGBITP 47 N))
   (OR (LOGBITP 25 N) (LOGBITP 28 N) (LOGBITP 34 N))
   (OR (LOGBITP 26 N) (NOT (LOGBITP 49 N)) (LOGBITP 36 N))
   (OR (NOT (LOGBITP 6 N)) (LOGBITP 45 N) (NOT (LOGBITP 42 N)))
   (OR (NOT (LOGBITP 45 N)) (NOT (LOGBITP 36 N)) (NOT (LOGBITP 7 N)))
   (OR (NOT (LOGBITP 39 N)) (LOGBITP 35 N) (NOT (LOGBITP 23 N)))
   (OR (NOT (LOGBITP 43 N)) (LOGBITP 45 N) (LOGBITP 14 N))
   (OR (NOT (LOGBITP 2 N)) (LOGBITP 35 N) (NOT (LOGBITP 15 N)))
   (OR (NOT (LOGBITP 47 N)) (LOGBITP 8 N) (LOGBITP 42 N))
   (OR (NOT (LOGBITP 24 N)) (NOT (LOGBITP 3 N)) (LOGBITP 43 N))
   (OR (NOT (LOGBITP 21 N)) (LOGBITP 36 N) (NOT (LOGBITP 6 N)))
   (OR (NOT (LOGBITP 30 N)) (NOT (LOGBITP 16 N)) (NOT (LOGBITP 21 N)))
   (OR (NOT (LOGBITP 10 N)) (NOT (LOGBITP 47 N)) (LOGBITP 16 N))
   (OR (LOGBITP 22 N) (LOGBITP 33 N) (NOT (LOGBITP 27 N)))
   (OR (LOGBITP 22 N) (NOT (LOGBITP 47 N)) (NOT (LOGBITP 38 N)))
   (OR (NOT (LOGBITP 36 N)) (NOT (LOGBITP 0 N)) (NOT (LOGBITP 22 N)))
   (OR (NOT (LOGBITP 18 N)) (LOGBITP 26 N) (LOGBITP 13 N))
   (OR (NOT (LOGBITP 21 N)) (LOGBITP 32 N) (NOT (LOGBITP 5 N)))
   (OR (NOT (LOGBITP 5 N)) (NOT (LOGBITP 31 N)) (NOT (LOGBITP 25 N)))
   (OR (LOGBITP 17 N) (NOT (LOGBITP 19 N)) (NOT (LOGBITP 45 N)))
   (OR (LOGBITP 42 N) (LOGBITP 21 N) (LOGBITP 26 N))
   (OR (NOT (LOGBITP 12 N)) (LOGBITP 33 N) (LOGBITP 48 N))
   (OR (NOT (LOGBITP 34 N)) (NOT (LOGBITP 45 N)) (LOGBITP 2 N))
   (OR (LOGBITP 31 N) (LOGBITP 38 N) (NOT (LOGBITP 42 N)))
   (OR (LOGBITP 5 N) (NOT (LOGBITP 38 N)) (NOT (LOGBITP 8 N)))
   (OR (LOGBITP 26 N) (LOGBITP 38 N) (NOT (LOGBITP 15 N)))
   (OR (LOGBITP 24 N) (NOT (LOGBITP 16 N)) (NOT (LOGBITP 14 N)))
   (OR (NOT (LOGBITP 42 N)) (LOGBITP 26 N) (LOGBITP 33 N))
   (OR (NOT (LOGBITP 5 N)) (LOGBITP 48 N) (LOGBITP 4 N))
   (OR (NOT (LOGBITP 37 N)) (LOGBITP 10 N) (LOGBITP 13 N))
   (OR (LOGBITP 39 N) (NOT (LOGBITP 37 N)) (LOGBITP 46 N))
   (OR (LOGBITP 36 N) (NOT (LOGBITP 13 N)) (LOGBITP 16 N))
   (OR (LOGBITP 38 N) (LOGBITP 28 N) (LOGBITP 35 N))
   (OR (NOT (LOGBITP 38 N)) (NOT (LOGBITP 27 N)) (LOGBITP 0 N))
   (OR (NOT (LOGBITP 17 N)) (LOGBITP 13 N) (NOT (LOGBITP 15 N)))
   (OR (NOT (LOGBITP 39 N)) (LOGBITP 49 N) (LOGBITP 14 N))
   (OR (LOGBITP 36 N) (NOT (LOGBITP 41 N)) (LOGBITP 17 N))
   (OR (NOT (LOGBITP 12 N)) (LOGBITP 30 N) (LOGBITP 32 N))
   (OR (LOGBITP 1 N) (NOT (LOGBITP 41 N)) (LOGBITP 32 N))
   (OR (LOGBITP 7 N) (NOT (LOGBITP 2 N)) (NOT (LOGBITP 21 N)))
   (OR (LOGBITP 0 N) (LOGBITP 22 N) (NOT (LOGBITP 30 N)))
   (OR (NOT (LOGBITP 19 N)) (NOT (LOGBITP 44 N)) (LOGBITP 25 N))
   (OR (LOGBITP 41 N) (LOGBITP 10 N) (LOGBITP 48 N))
   (OR (LOGBITP 28 N) (LOGBITP 10 N) (NOT (LOGBITP 42 N)))
   (OR (NOT (LOGBITP 19 N)) (NOT (LOGBITP 20 N)) (LOGBITP 29 N))
   (OR (LOGBITP 22 N) (LOGBITP 44 N) (NOT (LOGBITP 34 N)))
   (OR (LOGBITP 37 N) (NOT (LOGBITP 29 N)) (NOT (LOGBITP 13 N)))
   (OR (NOT (LOGBITP 8 N)) (LOGBITP 47 N) (NOT (LOGBITP 28 N)))
   (OR (LOGBITP 10 N) (NOT (LOGBITP 17 N)) (NOT (LOGBITP 22 N)))
   (OR (NOT (LOGBITP 40 N)) (NOT (LOGBITP 0 N)) (NOT (LOGBITP 28 N)))
   (OR (LOGBITP 4 N) (LOGBITP 40 N) (LOGBITP 25 N))
   (OR (LOGBITP 43 N) (NOT (LOGBITP 29 N)) (NOT (LOGBITP 6 N)))
   (OR (LOGBITP 37 N) (NOT (LOGBITP 5 N)) (NOT (LOGBITP 40 N)))
   (OR (LOGBITP 45 N) (LOGBITP 47 N) (NOT (LOGBITP 14 N)))
   (OR (NOT (LOGBITP 17 N)) (NOT (LOGBITP 9 N)) (NOT (LOGBITP 46 N)))
   (OR (LOGBITP 37 N) (LOGBITP 45 N) (NOT (LOGBITP 31 N)))
   (OR (NOT (LOGBITP 31 N)) (LOGBITP 45 N) (LOGBITP 11 N))
   (OR (LOGBITP 30 N) (LOGBITP 39 N) (LOGBITP 13 N))
   (OR (NOT (LOGBITP 17 N)) (LOGBITP 1 N) (LOGBITP 48 N))
   (OR (LOGBITP 27 N) (NOT (LOGBITP 37 N)) (LOGBITP 26 N))
   (OR (NOT (LOGBITP 15 N)) (NOT (LOGBITP 20 N)) (LOGBITP 13 N))
   (OR (NOT (LOGBITP 28 N)) (LOGBITP 14 N) (LOGBITP 11 N))
   (OR (LOGBITP 48 N) (LOGBITP 33 N) (LOGBITP 4 N))
   (OR (LOGBITP 13 N) (LOGBITP 21 N) (NOT (LOGBITP 11 N)))
   (OR (LOGBITP 29 N) (LOGBITP 32 N) (LOGBITP 19 N))
   (OR (NOT (LOGBITP 23 N)) (LOGBITP 21 N) (LOGBITP 24 N))
   (OR (LOGBITP 3 N) (NOT (LOGBITP 47 N)) (NOT (LOGBITP 22 N)))
   (OR (NOT (LOGBITP 29 N)) (NOT (LOGBITP 35 N)) (LOGBITP 8 N))
   (OR (LOGBITP 43 N) (LOGBITP 11 N) (NOT (LOGBITP 34 N)))
   (OR (LOGBITP 37 N) (LOGBITP 2 N) (NOT (LOGBITP 20 N)))
   (OR (NOT (LOGBITP 10 N)) (LOGBITP 32 N) (LOGBITP 48 N))))


(defun eff8 (start)
  (declare (optimize speed))
  (declare (type fixnum start))
  (do ((i start (+ i 1)))
      ((sat-eff8 i) i)))



(defun rewrite (e)
  (if (consp e)
      (cons (car e) (mapcar #'rewrite (cdr e)))
      (let ((vnum (read-from-string (subseq (symbol-name e) 1))))
	`(logbitp ,(- vnum 1) n))))

(defparameter sat-eff8-expr
  '(AND
    (OR
     (NOT
      V3)
     V36
     V7)
    (OR
     (NOT
      V3)
     (NOT
      V42)
     (NOT
      V48))
    (OR
     (NOT
      V49)
     (NOT
      V47)
     (NOT
      V41))
    (OR
     V8
     (NOT
      V40)
     V17)
    (OR
     (NOT
      V21)
     (NOT
      V31)
     (NOT
      V39))
    (OR
     V36
     (NOT
      V22)
     V49)
    (OR
     V27
     V38
     V14)
    (OR
     V15
     (NOT
      V18)
     V6)
    (OR
     V6
     V7
     (NOT
      V43))
    (OR
     V34
     (NOT
      V7)
     V23)
    (OR
     V2
     V14
     (NOT
      V13))
    (OR
     V2
     V47
     (NOT
      V42))
    (OR
     (NOT
      V33)
     (NOT
      V35)
     V3)
    (OR
     V44
     V40
     V49)
    (OR
     V50
     V36
     V31)
    (OR
     (NOT
      V36)
     (NOT
      V3)
     (NOT
      V37))
    (OR
     V26
     (NOT
      V29)
     V43)
    (OR
     V15
     V29
     (NOT
      V45))
    (OR
     V24
     (NOT
      V11)
     V18)
    (OR
     (NOT
      V47)
     (NOT
      V26)
     V6)
    (OR
     (NOT
      V50)
     (NOT
      V33)
     (NOT
      V10))
    (OR
     V32
     V6
     V16)
    (OR
     (NOT
      V34)
     V37
     V41)
    (OR
     V7
     (NOT
      V28)
     (NOT
      V17))
    (OR
     (NOT
      V44)
     V46
     V19)
    (OR
     V7
     V22
     (NOT
      V48))
    (OR
     V3
     V39
     V34)
    (OR
     V31
     V46
     (NOT
      V43))
    (OR
     (NOT
      V27)
     V32
     V23)
    (OR
     V37
     (NOT
      V50)
     (NOT
      V18))
    (OR
     V20
     V5
     V11)
    (OR
     (NOT
      V45)
     (NOT
      V24)
     V6)
    (OR
     (NOT
      V34)
     (NOT
      V23)
     (NOT
      V14))
    (OR
     (NOT
      V22)
     V21
     V20)
    (OR
     (NOT
      V17)
     V50
     V24)
    (OR
     (NOT
      V25)
     (NOT
      V24)
     (NOT
      V27))
    (OR
     V3
     V35
     V21)
    (OR
     (NOT
      V26)
     V47
     (NOT
      V36))
    (OR
     (NOT
      V28)
     (NOT
      V45)
     V49)
    (OR
     (NOT
      V21)
     (NOT
      V6)
     V12)
    (OR
     (NOT
      V17)
     (NOT
      V15)
     (NOT
      V39))
    (OR
     V41
     V2
     (NOT
      V14))
    (OR
     V25
     V36
     (NOT
      V23))
    (OR
     (NOT
      V39)
     (NOT
      V3)
     (NOT
      V40))
    (OR
     V50
     V20
     V35)
    (OR
     V27
     V31
     (NOT
      V39))
    (OR
     V45
     (NOT
      V15)
     (NOT
      V40))
    (OR
     V34
     V50
     V35)
    (OR
     (NOT
      V1)
     (NOT
      V48)
     V12)
    (OR
     V18
     (NOT
      V35)
     (NOT
      V30))
    (OR
     V27
     (NOT
      V24)
     (NOT
      V25))
    (OR
     (NOT
      V4)
     (NOT
      V33)
     (NOT
      V12))
    (OR
     (NOT
      V43)
     (NOT
      V24)
     (NOT
      V37))
    (OR
     (NOT
      V37)
     V31
     (NOT
      V44))
    (OR
     (NOT
      V9)
     (NOT
      V38)
     V14)
    (OR
     V33
     (NOT
      V16)
     V34)
    (OR
     V4
     (NOT
      V35)
     (NOT
      V5))
    (OR
     (NOT
      V3)
     (NOT
      V21)
     (NOT
      V19))
    (OR
     (NOT
      V35)
     (NOT
      V36)
     (NOT
      V29))
    (OR
     V7
     (NOT
      V43)
     V36)
    (OR
     V30
     V14
     V41)
    (OR
     (NOT
      V35)
     (NOT
      V24)
     (NOT
      V7))
    (OR
     V35
     (NOT
      V42)
     V6)
    (OR
     (NOT
      V1)
     (NOT
      V15)
     V39)
    (OR
     V27
     V49
     (NOT
      V16))
    (OR
     (NOT
      V37)
     V49
     (NOT
      V10))
    (OR
     V50
     (NOT
      V46)
     (NOT
      V3))
    (OR
     (NOT
      V41)
     V20
     V34)
    (OR
     (NOT
      V1)
     V23
     V28)
    (OR
     (NOT
      V12)
     (NOT
      V30)
     (NOT
      V20))
    (OR
     (NOT
      V24)
     V29
     (NOT
      V37))
    (OR
     V12
     V5
     (NOT
      V44))
    (OR
     (NOT
      V6)
     (NOT
      V2)
     V48)
    (OR
     (NOT
      V2)
     (NOT
      V49)
     (NOT
      V43))
    (OR
     V1
     (NOT
      V50)
     V24)
    (OR
     (NOT
      V7)
     (NOT
      V50)
     (NOT
      V44))
    (OR
     (NOT
      V41)
     V43
     V4)
    (OR
     V13
     V15
     (NOT
      V11))
    (OR
     (NOT
      V3)
     (NOT
      V11)
     V23)
    (OR
     V33
     V48
     V41)
    (OR
     V9
     V23
     (NOT
      V49))
    (OR
     (NOT
      V43)
     V47
     V1)
    (OR
     (NOT
      V40)
     V16
     (NOT
      V29))
    (OR
     V30
     V19
     V3)
    (OR
     V19
     (NOT
      V34)
     V48)
    (OR
     (NOT
      V16)
     (NOT
      V44)
     V14)
    (OR
     V38
     (NOT
      V45)
     (NOT
      V12))
    (OR
     (NOT
      V4)
     (NOT
      V14)
     (NOT
      V31))
    (OR
     (NOT
      V48)
     V35
     (NOT
      V1))
    (OR
     V45
     (NOT
      V13)
     V19)
    (OR
     V9
     V42
     (NOT
      V7))
    (OR
     (NOT
      V1)
     (NOT
      V15)
     V8)
    (OR
     (NOT
      V13)
     (NOT
      V44)
     (NOT
      V14))
    (OR
     (NOT
      V43)
     (NOT
      V37)
     (NOT
      V31))
    (OR
     (NOT
      V27)
     (NOT
      V29)
     V47)
    (OR
     V7
     V4
     V17)
    (OR
     V7
     V10
     V35)
    (OR
     (NOT
      V25)
     V20
     V17)
    (OR
     V35
     (NOT
      V5)
     (NOT
      V42))
    (OR
     (NOT
      V50)
     V24
     (NOT
      V5))
    (OR
     (NOT
      V21)
     (NOT
      V26)
     V2)
    (OR
     (NOT
      V8)
     V45
     (NOT
      V21))
    (OR
     (NOT
      V16)
     V33
     V49)
    (OR
     (NOT
      V38)
     V6
     V16)
    (OR
     V5
     V21
     V37)
    (OR
     V8
     V38
     V31)
    (OR
     (NOT
      V21)
     V33
     V14)
    (OR
     V20
     V40
     (NOT
      V5))
    (OR
     (NOT
      V29)
     (NOT
      V9)
     V31)
    (OR
     (NOT
      V7)
     V42
     (NOT
      V22))
    (OR
     (NOT
      V48)
     V8
     V26)
    (OR
     V48
     (NOT
      V38)
     V33)
    (OR
     (NOT
      V34)
     V49
     V46)
    (OR
     (NOT
      V14)
     (NOT
      V46)
     V25)
    (OR
     (NOT
      V46)
     V4
     V18)
    (OR
     V36
     (NOT
      V12)
     (NOT
      V31))
    (OR
     V12
     (NOT
      V18)
     V14)
    (OR
     (NOT
      V7)
     V46
     (NOT
      V16))
    (OR
     V9
     (NOT
      V8)
     V7)
    (OR
     V49
     (NOT
      V42)
     (NOT
      V22))
    (OR
     V22
     (NOT
      V15)
     V38)
    (OR
     V34
     (NOT
      V41)
     V47)
    (OR
     V22
     (NOT
      V26)
     V32)
    (OR
     (NOT
      V25)
     (NOT
      V45)
     (NOT
      V21))
    (OR
     (NOT
      V26)
     V32
     (NOT
      V11))
    (OR
     V15
     V26
     (NOT
      V25))
    (OR
     (NOT
      V1)
     V46
     V25)
    (OR
     (NOT
      V14)
     (NOT
      V31)
     V30)
    (OR
     (NOT
      V9)
     (NOT
      V22)
     V12)
    (OR
     (NOT
      V18)
     V26
     (NOT
      V35))
    (OR
     (NOT
      V16)
     (NOT
      V32)
     (NOT
      V21))
    (OR
     V31
     (NOT
      V49)
     (NOT
      V21))
    (OR
     V11
     V9
     V41)
    (OR
     (NOT
      V13)
     (NOT
      V30)
     V19)
    (OR
     (NOT
      V10)
     V4
     V6)
    (OR
     (NOT
      V4)
     V3
     (NOT
      V22))
    (OR
     (NOT
      V25)
     (NOT
      V50)
     (NOT
      V18))
    (OR
     (NOT
      V40)
     V4
     V9)
    (OR
     V37
     V20
     V46)
    (OR
     (NOT
      V27)
     V22
     (NOT
      V29))
    (OR
     V34
     V14
     V3)
    (OR
     V3
     (NOT
      V31)
     V20)
    (OR
     (NOT
      V50)
     V2
     (NOT
      V26))
    (OR
     V17
     (NOT
      V29)
     V38)
    (OR
     (NOT
      V49)
     V12
     (NOT
      V41))
    (OR
     V15
     (NOT
      V35)
     (NOT
      V43))
    (OR
     (NOT
      V22)
     (NOT
      V23)
     (NOT
      V49))
    (OR
     (NOT
      V9)
     V33
     V48)
    (OR
     V26
     V29
     V35)
    (OR
     V27
     (NOT
      V50)
     V37)
    (OR
     (NOT
      V7)
     V46
     (NOT
      V43))
    (OR
     (NOT
      V46)
     (NOT
      V37)
     (NOT
      V8))
    (OR
     (NOT
      V40)
     V36
     (NOT
      V24))
    (OR
     (NOT
      V44)
     V46
     V15)
    (OR
     (NOT
      V3)
     V36
     (NOT
      V16))
    (OR
     (NOT
      V48)
     V9
     V43)
    (OR
     (NOT
      V25)
     (NOT
      V4)
     V44)
    (OR
     (NOT
      V22)
     V37
     (NOT
      V7))
    (OR
     (NOT
      V31)
     (NOT
      V17)
     (NOT
      V22))
    (OR
     (NOT
      V11)
     (NOT
      V48)
     V17)
    (OR
     V23
     V34
     (NOT
      V28))
    (OR
     V23
     (NOT
      V48)
     (NOT
      V39))
    (OR
     (NOT
      V37)
     (NOT
      V1)
     (NOT
      V23))
    (OR
     (NOT
      V19)
     V27
     V14)
    (OR
     (NOT
      V22)
     V33
     (NOT
      V6))
    (OR
     (NOT
      V6)
     (NOT
      V32)
     (NOT
      V26))
    (OR
     V18
     (NOT
      V20)
     (NOT
      V46))
    (OR
     V43
     V22
     V27)
    (OR
     (NOT
      V13)
     V34
     V49)
    (OR
     (NOT
      V35)
     (NOT
      V46)
     V3)
    (OR
     V32
     V39
     (NOT
      V43))
    (OR
     V6
     (NOT
      V39)
     (NOT
      V9))
    (OR
     V27
     V39
     (NOT
      V16))
    (OR
     V25
     (NOT
      V17)
     (NOT
      V15))
    (OR
     (NOT
      V43)
     V27
     V34)
    (OR
     (NOT
      V6)
     V49
     V5)
    (OR
     (NOT
      V38)
     V11
     V14)
    (OR
     V40
     (NOT
      V38)
     V47)
    (OR
     V37
     (NOT
      V14)
     V17)
    (OR
     V39
     V29
     V36)
    (OR
     (NOT
      V39)
     (NOT
      V28)
     V1)
    (OR
     (NOT
      V18)
     V14
     (NOT
      V16))
    (OR
     (NOT
      V40)
     V50
     V15)
    (OR
     V37
     (NOT
      V42)
     V18)
    (OR
     (NOT
      V13)
     V31
     V33)
    (OR
     V2
     (NOT
      V42)
     V33)
    (OR
     V8
     (NOT
      V3)
     (NOT
      V22))
    (OR
     V1
     V23
     (NOT
      V31))
    (OR
     (NOT
       V20)
     (NOT
      V45)
     V26)
    (OR
     V42
     V11
     V49)
    (OR
     V29
     V11
     (NOT
      V43))
    (OR
     (NOT
      V20)
     (NOT
      V21)
     V30)
    (OR
     V23
     V45
     (NOT
      V35))
    (OR
     V38
     (NOT
      V30)
     (NOT
      V14))
    (OR
     (NOT
      V9)
     V48
     (NOT
      V29))
    (OR
     V11
     (NOT
      V18)
     (NOT
      V23))
    (OR
     (NOT
      V41)
     (NOT
      V1)
     (NOT
      V29))
    (OR
     V5
     V41
     V26)
    (OR
     V44
     (NOT
      V30)
     (NOT
      V7))
    (OR
     V38
     (NOT
      V6)
     (NOT
      V41))
    (OR
     V46
     V48
     (NOT
      V15))
    (OR
     (NOT
      V18)
     (NOT
      V10)
     (NOT
      V47))
    (OR
     V38
     V46
     (NOT
      V32))
    (OR
     (NOT
      V32)
     V46
     V12)
    (OR
     V31
     V40
     V14)
    (OR
     (NOT
      V18)
     V2
     V49)
    (OR
     V28
     (NOT
      V38)
     V27)
    (OR
     (NOT
      V16)
     (NOT
      V21)
     V14)
    (OR
     (NOT
      V29)
     V15
     V12)
    (OR
     V49
     V34
     V5)
    (OR
     V14
     V22
     (NOT
      V12))
    (OR
     V30
     V33
     V20)
    (OR
     (NOT
      V24)
     V22
     V25)
    (OR
     V4
     (NOT
      V48)
     (NOT
      V23))
    (OR
     (NOT
      V30)
     (NOT
      V36)
     V9)
    (OR
     V44
     V12
     (NOT
      V35))
    (OR
     V38
     V3
     (NOT
      V21))
    (OR
     (NOT
      V11)
     V33
     V49)))



