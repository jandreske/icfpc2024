(defpackage :spaceship
  (:use :cl)
  (:import-from :alexandria :array-index)
  (:export :read-locations :solve))

(in-package :spaceship)

(deftype coord () '(signed-byte 16))

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


(declaim (ftype (function (coord coord coord coord) (unsigned-byte 24))
		manhattan-distance))
(declaim (inline manhattan-distance))
(defun manhattan-distance (x1 y1 x2 y2)
  (+ (abs (- x2 x1))
     (abs (- y2 y1))))


(declaim (ftype (function ((simple-array coord (*))
			   (simple-array bit (*))
			   coord coord)
			  (or null array-index))
		nearest-to))

(defun nearest-unvisited (locations visited x y)
  (declare (type coord x y))
  (let ((pos nil)
	(dist 0)
	(n (length visited)))
    (declare (optimize speed))
    (declare (type (unsigned-byte 24) dist))
    (dotimes (i n)
      (when (zerop (aref visited i))
	(let ((d (manhattan-distance x y (aref locations (* i 2)) (aref locations (+ (* i 2) 1)))))
	  (when (or (null pos) (< d dist))
	    (setf dist d)
	    (setf pos i)))))
    pos))

(defun dist-to-motionless (velocity)
  (floor (* velocity (+ 1 velocity)) 2))

(defun accel (target position velocity)
  (let ((a (- target position velocity)))
    (alexandria:clamp a -1 1)))

(defconstant +keys+ #2a((#\1 #\4 #\7) (#\2 #\5 #\8) (#\3 #\6 #\9)))

(defun get-key (ax ay)
  (aref +keys+ (+ ay 1) (+ ax 1)))

(defun solve (locations)
  (let ((x 0)
	(y 0)
	(vx 0)
	(vy 0)
	(visited (make-array (floor (length locations) 2)
			     :element-type 'bit
			     :initial-element 0))
	(moves nil))
    (do ((dest-index (nearest-unvisited locations visited
					(+ x (dist-to-motionless vx))
					(+ y (dist-to-motionless vy)))))
	((not dest-index) (coerce (nreverse moves) 'string))
      (do* ((dest-x (aref locations (* dest-index 2)))
	    (dest-y (aref locations (+ (* dest-index 2) 1))))
	   ((and (= x dest-x) (= y dest-y))
	    (setf (aref visited dest-index) 1))
	(let ((ox (accel dest-x x vx))
	      (oy (accel dest-y y vy)))
	  (push (get-key ox oy) moves)
	  (incf vx ox)
	  (incf vy oy)
	  (incf x vx)
	  (incf y vy))))))

