(defpackage :spaceship
  (:use :cl)
  (:import-from :alexandria :array-index :clamp)
  (:export :read-locations :solve))

(in-package :spaceship)

(defconstant +max-moves+ 10000000)

(deftype coord () '(signed-byte 32))

;; In a severe case of premature optimization, I decided to
;; put the locations into a flat vector #(x1 y1 x2 y2 ...).

(defun read-locations (file)
  (with-open-file (s file :direction :input)
    (let ((locations nil))
      (declare (type list locations))
      (do ((n (read s nil) (read s nil)))
	  ((not (integerp n))
	   (let ((lcs (coerce locations '(simple-array coord (*)))))
	     (unless (evenp (length lcs))
	       (error "corrupted spaceship problem"))
	     (nreverse lcs)))
	(push n locations)))))


(defun distance (x1 y1 x2 y2)
  "Minimum distance from (x1 y1) to (x2 y2)."
  (declare (optimize speed))
  (declare (type coord x1 y1 x2 y2))
  (let ((dx (- x2 x1))
	(dy (- y2 y1)))
  (max (abs dx)) (abs dy)))

(defun distance-estimate (x y vx vy dest-x dest-y)
  (declare (optimize speed))
  (declare (type coord x y vx vy dest-x dest-y))
  (let ((delta-x (- dest-x x))
	(delta-y (- dest-y y)))
    (when (and (not (= 0 vx)) (= (signum delta-x) (signum vx)))
      (setf delta-x (truncate delta-x vx)))
    (when (and (not (= 0 vy)) (= (signum delta-y) (signum vy)))
      (setf delta-y (truncate delta-y vy)))
    (max (abs delta-x) (abs delta-y))))


(defun galaxy-size (locations)
  (let ((xlo (aref locations 0))
	(xhi (aref locations 0))
	(ylo (aref locations 1))
	(yhi (aref locations 1))
	(n (length locations)))
    (do ((i 2 (+ i 2)))
	((>= i n)
	 (list xlo ylo xhi yhi))
      (let ((x (aref locations i))
	    (y (aref locations (+ i 1))))
	(when (< x xlo) (setf xlo x))
	(when (> x xhi) (setf xhi x))
	(when (< y ylo) (setf ylo y))
	(when (> y yhi) (setf yhi y))
	))))


(defun all-locations (x y)
  (declare (ignore x y))
  t)


(defun nearest-unvisited (locations visited x y vx vy &key (filter #'all-locations))
  (declare (type coord x y vx vy)
	   (type (simple-array coord (*)) locations)
	   (type (simple-array bit (*)) visited))
  (declare (optimize speed))
  (let ((pos nil)
	(dist 0)
	(n (length visited)))
    (dotimes (i n)
      (when (zerop (aref visited i))
	(let* ((dx (aref locations (* i 2)))
	       (dy (aref locations (+ (* i 2) 1)))
	       (d  (distance x y dx dy)))
	  (when (and (or (null pos) (< d dist)) (funcall filter dx dy))
	    (setf dist d)
	    (setf pos i)))))
    pos))

(defun dist-to-motionless (velocity)
  velocity)
  ;; (rem (* velocity (+ 1 velocity)) 2))


(defun reach (d v)
  (labels ((steps (d v)
	     (cond ((zerop v) 1000000000)
		   ((< d 0)    500000000)
		   ((< v 0)    250000000)
		   ((zerop d) 0)
		   (t
		 (let* ((d (- d v))
			(brake (steps d (- v 1)))
			(neutral (steps d v))
			(speedup (steps d (+ v 1))))
		   (1+ (min brake neutral speedup)))))))
    (let ((speedup (steps d ( + v 1)))
	  (neutral (steps d v))
	  (brake (steps d (- v 1))))
      (if (<= speedup neutral)
	  (if (<= speedup brake) 1
	      (if (<= neutral brake) 0 -1))
	  (if (<= neutral brake) 0 -1)))))

(declaim (ftype (function (list list) t) better))
(defun better (a b)
  (declare (optimize speed))
  (cond ((null a) t)
	((null b) nil)
	((symbolp (car a)) nil)
	((symbolp (car b)) t)
	(t (better (cdr a) (cdr b)))))

(declaim (ftype (function (coord coord) list) path))
(defun path (d v)
  (declare (optimize (speed 2)))
  (let ((d1 (- d v)))
    (cond ((= d 0) nil)
	  ((= v 0) '(ignore))
	  ((>= (abs d1) (abs d)) '(fail))
	  (t (let ((b (path d1 (- v 1)))
		   (n (path d1 v))
		   (s (path d1 (+ v 1))))
	       (if (better b n)
		   (if (better b s)
		       (cons -1 b)
		       (cons 1 s))
		   (if (better n s)
		       (cons 0 n)
		       (cons 1 s))))))))

(declaim (ftype (function (coord coord) (or null (integer -1 1))) best-accel))
(defun best-accel (d v)
  (let ((fail 1000000000))
    (labels ((step1 (d v)
	       (declare (type coord d v))
	       (let ((d1 (- d v)))
		 (cond ((= d 0) (values 0 1))
		       ((= v 0) (values fail 1))
		       ((>= (abs d1) (abs d)) (values fail 1))
		       (t (let ((cb (step1 d1 (- v 1)))
				(cn (step1 d1 v))
				(cs (step1 d1 (+ v 1))))
			    (if (<= cs cn)
				(if (<= cs cb)
				    (values (+ cs 1) 1)
				    (values (+ cb 1) -1))
				(if (<= cn cb)
				    (values (+ cn 1) 0)
				    (values (+ cb 1) -1)))))))))
      (multiple-value-bind (c a) (step1 d v)
	(if (>= c fail)
	    nil
	    a)))))


(declaim (ftype (function (coord coord coord)
			  (integer -1 1))
		accel))
(defun accel (target position velocity)
  (declare (optimize speed))
  ;; let's see what happens if we optimize some simple cases by hand
  (let ((delta (- target position)))
    (cond
      ((and (> delta 2) (= velocity (1+ (truncate delta 2)))) -1)
      ((and (< delta -2) (= velocity (floor delta 2))) 1)
      ((= delta velocity) 0)
      ((= delta (* 2 velocity)) 0)
      ((and (> delta 0) (<= delta (* 2 velocity))) -1)
      ((and (< delta 0) (>= delta (* 2 velocity))) 1)
      (t
       (let ((a (- target position velocity)))
	 (alexandria:clamp a -1 1))))))

(defun accel-exact (dx dy vx vy)
  (cond ((and (= dx 7) (= vx 4) (= dy 3) (= vy 2))
	 (values 0 0))
	(t
	 (values (clamp (- dx vx) -1 1)
		 (clamp (- dy vy) -1 1)))))

(declaim (ftype (function ((integer 0 8)) (values coord coord)) decode-move))
(defun decode-move (i)
  (declare (optimize speed))
  (ecase i
    (0 (values -1 -1))
    (1 (values  0 -1))
    (2 (values  1 -1))
    (3 (values -1  0))
    (4 (values  0  0))
    (5 (values  1  0))
    (6 (values -1  1))
    (7 (values  0  1))
    (8 (values  1  1))))

(defun moves-to (d1x d1y d2x d2y x y vx vy move-code)
  (declare (optimize speed))
  (declare (type fixnum d1x d1y d2x d2y x y vx vy move-code))
  (let ((1hit nil)
	(2hit nil))
    (do ((len 1 (+ len 1))
	 (code move-code (truncate code 9)))
	((= len 5) 5)
      (multiple-value-bind (ax ay)
	  (decode-move (rem code 9))
	(incf vx ax)
	(incf vy ay)
	(incf x vx)
	(incf y vy)
	(when (and (= x d1x) (= y d1y))
	  (setf 1hit t))
	(when (and (= x d2x) (= y d2y))
	  (setf 2hit t))
	(when (and 1hit 2hit)
	  (return-from moves-to len))))))


(defun accel-ahead (locs i x y vx vy)
  (let ((d1x (aref locs i))
	(d1y (aref locs (+ i 1))))
    (if (or (>= (+ i 2) (length locs)) (> (* 10 (1+ (distance x y d1x d1y))) (distance 0 0 vx vy)))
	(accel-exact (- d1x x) (- d1y y) vx vy)
	(let ((d2x (aref locs (+ i 2)))
	      (d2y (aref locs (+ i 3)))
	      (best-length 5)
	      (best-move 0))
	  (dotimes (move-code (* 9 9 9 9))
	    (let ((n (moves-to d1x d1y d2x d2y x y vx vy move-code)))
	      (when (< n best-length)
		;; (format t "Best improved to ~a~%" n)
		(setf best-length n)
		(setf best-move move-code))))
	  (if (> best-length 4)
	      (accel-exact (- d1x x) (- d1y y) vx vy)
	      (progn
		;; (format t "can go from ~a ~a via ~a ~a to ~a ~a in ~a moves:~%"
		;; 	x y d1x d1y d2x d2y
		;; 	best-length)
		;; (dotimes (i best-length)
		;;   (multiple-value-bind (ax ay)
		;;       (decode-move (rem (truncate best-move (expt 9 i)) 9))
		;;     (format t "  accel ~a ~a~%" ax ay)))
		
		(decode-move (rem best-move 9))))))))


(defparameter +keys+ #2a((#\1 #\4 #\7) (#\2 #\5 #\8) (#\3 #\6 #\9)))

(declaim (type (simple-array * (3 3)) +keys+))

(declaim (inline get-key))
(defun get-key (ax ay)
  (aref +keys+ (+ ax 1) (+ ay 1)))

(defun solve (problem-number locations)
  (case problem-number
    ;;(25 (solve-quadrants locations))
    ;;(5 (solve2 locations))
    (t (solve-change-dest locations))))

(defun solve1 (locations)
  (solve-set locations
	     #'all-locations
	     0 0
	     0 0))

(defun solve-quadrants (locations)
  (multiple-value-bind (m1 x y vx vy)
      (solve-set locations #'(lambda (x y) (and (>= x 0) (>= y 0))) 0 0 0 0)
    (multiple-value-bind (m2 x y vx vy)
      (solve-set locations #'(lambda (x y) (and (>= x 0) (< y 0))) x y vx vy)
      (multiple-value-bind (m3 x y vx vy)
	  (solve-set locations #'(lambda (x y) (and (< x 0) (< y 0))) x y vx vy)
	(multiple-value-bind (m4 x y vx vy)
	    (solve-set locations #'(lambda (x y) (and (< x 0) (>= y 0))) x y vx vy)
	  (declare (ignore x y vx vy))
	  (concatenate 'string m1 m2 m3 m4))))))
 
(defun solve-set (locations eligible-p x y vx vy)
  (declare (type (simple-array coord (*)) locations)
	   (type coord x y vx vy))
  (let ((visited (make-array (floor (length locations) 2)
			     :element-type 'bit
			     :initial-element 0))
	(num-moves 0)
	(moves nil))
    (declare 
     (type (integer 0 100000000) num-moves)
     (type list moves))

    (do ((dest-index (nearest-unvisited locations visited x y vx vy :filter eligible-p)
		     (nearest-unvisited locations visited x y vx vy :filter eligible-p)))
	((not dest-index)
	 (values (coerce (nreverse moves) 'string) x y vx vy))
      (do* ((dest-x (aref locations (* dest-index 2)))
	    (dest-y (aref locations (+ (* dest-index 2) 1))))
	   ((and (= x dest-x) (= y dest-y))
	    (setf (aref visited dest-index) 1))
	(let ((ax (accel dest-x x vx))
	      (ay (accel dest-y y vy)))
	  (push (get-key ax ay) moves)
	  (when (> num-moves +max-moves+)
	    (format t "~&Maximum number of moves reached!~%")
	    (return-from solve-set
	      (values (coerce (nreverse moves) 'string) x y vx vy)))
	  (incf num-moves)
	  (incf vx ax)
	  (incf vy ay)
	  (incf x vx)
	  (incf y vy)
	  )))))




(defun solve2 (locations)
  (declare (type (simple-array coord (*)) locations))
  (let ((x 0)
	(y 0)
	(vx 0)
	(vy 0)
	(visited (make-array (floor (length locations) 2)
			     :element-type 'bit
			     :initial-element 0))
	(num-moves 0)
	(moves nil))
    (declare (type coord x y vx vy)
	     (type (integer 0 100000000) num-moves)
	     (type list moves))
    (do ((dest-index 0 (+ dest-index 1)))
	((= dest-index (length visited))
	 (coerce (nreverse moves) 'string))
      (do* ((dest-x (aref locations (* dest-index 2)))
	    (dest-y (aref locations (+ (* dest-index 2) 1))))
	   ((and (= x dest-x) (= y dest-y))
	    (format t "Waypoint ~A (~A ~A) reached: ~A moves (Velocity ~a ~a)~%"
		    (+ dest-index 1)
		    dest-x dest-y
		    num-moves
		    vx vy)
	    (setf (aref visited dest-index) 1))
	(multiple-value-bind (ax ay)
	    (accel-ahead locations (* 2 dest-index) x y vx vy)
	  (push (get-key ax ay) moves)
	  ;; (format t "Move: ~C~%" (car moves))
	  (when (> num-moves +max-moves+)
	    (format t "~&Maximum number of moves reached!~%")
	    (return-from solve2 (coerce (nreverse moves) 'string)))
	  (incf num-moves)
	  (incf vx ax)
	  (incf vy ay)
	  (incf x vx)
	  (incf y vy)
	  )))))


(defun solve-change-dest (locations)
  (declare (type (simple-array coord (*)) locations))
  (declare (optimize speed))
  (let ((x 0)
	(y 0)
	(vx 0)
	(vy 0)
	(visited (make-array (floor (length locations) 2)
			     :element-type 'bit
			     :initial-element 0))
	(num-moves 0)
	(moves nil))
    (declare (type coord x y vx vy)
	     (type (integer 0 100000000) num-moves)
	     (type list moves))
    (do ((dest-index (nearest-unvisited locations visited x y 0 0)
		     (nearest-unvisited locations visited x y 0 0)))
	((not dest-index)
	 (coerce (nreverse moves) 'string))
      (let ((dest-x (aref locations (* dest-index 2)))
	    (dest-y (aref locations (+ (* dest-index 2) 1))))
	(cond 
	  ((and (= x dest-x) (= y dest-y))
	   (setf (aref visited dest-index) 1))
	  (t
	   (let ((ax (accel dest-x x vx))
		 (ay (accel dest-y y vy)))
	     (push (get-key ax ay) moves)
	     (when (> num-moves +max-moves+)
	       (format t "~&Maximum number of moves reached!~%")
	       (return-from solve-change-dest (coerce (nreverse moves) 'string)))
	     (incf num-moves)
	     (incf vx ax)
	     (incf vy ay)
	     (incf x vx)
	     (incf y vy)
	     )))))))
