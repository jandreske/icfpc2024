(defpackage :binary-heap
  (:use :cl)
  (:import-from :alexandria :array-length)
  (:export :make-binary-heap :binary-heap-size :binary-heap-take-min :binary-heap-insert))

(in-package :binary-heap)


; (declaim (optimize (speed 3) (debug 0) (safety 0)))
(declaim (optimize (speed 3)))


(defstruct (binary-heap (:constructor %make-binary-heap))
  (less-p (error "Missing argument less-p") :type (function (t t) t))
  (size 0 :type array-length)
  (heap (make-array 4 :initial-element nil) :type simple-vector))

#+sbcl (declaim (sb-ext:freeze-type binary-heap))

(defun make-binary-heap (less-p)
  (%make-binary-heap :less-p less-p))


(declaim (ftype (function (binary-heap) t) empty-p))
(declaim (inline empty-p))
(defun empty-p (h)
  (zerop (binary-heap-size h)))


(declaim (ftype (function (binary-heap) t) binary-heap-find-min))
(defun binary-heap-find-min (h)
  (if (empty-p h)
      nil
      (svref (binary-heap-heap h) 1)))

(declaim (ftype (function (binary-heap array-length)) ensure-capacity))
(defun ensure-capacity (h n)
  (declare (type array-length n))
  (let* ((old-heap (binary-heap-heap h))
	 (curr-alloc (length old-heap)))
    (when (>= n curr-alloc)
      (let ((new-heap (make-array (* 2 curr-alloc) :initial-element nil)))
	(dotimes (i curr-alloc)
	  (setf (svref new-heap i) (svref old-heap i)))
	(setf (binary-heap-heap h) new-heap)))))

(declaim (ftype (function (binary-heap fixnum t)) fixup))
(defun fixup (h k key)
  (let ((a (binary-heap-heap h))
	(less-p (binary-heap-less-p h)))
    (labels ((up (k)
	       (declare (type (and fixnum (integer 1 *)) k))
	       (if (> k 1)
		   (let ((k2 (ash k -1)))
		     (cond ((funcall less-p key (svref a k2))
			    (setf (svref a k) (svref a k2))
			    (up k2))
			   (t
			    (setf (svref a k) key))))
		   (setf (svref a k) key))))
      (up k))))

(declaim (ftype (function (binary-heap fixnum)) fixdown))
(defun fixdown (h k)
  (let* ((a (binary-heap-heap h))
	 (size (binary-heap-size h))
	 (less-p (binary-heap-less-p h))
	 (key (svref a k)))
    (labels ((down (k)
	       (let ((j (* k 2)))
		 (declare (type (and fixnum (integer 1 *)) k j))
		 (if (<= j size)
		     (progn (when (and (< j size) (funcall less-p (svref a (1+ j)) (svref a j)))
			      (setf j (1+ j)))
			    (if (funcall less-p key (svref a j))
				(setf (svref a k) key)
				(progn (setf (svref a k) (svref a j))
				       (down j))))
		     (setf (svref a k) key)))))
      (down k))))


(declaim (ftype (function (binary-heap) t) binary-heap-take-min))
(defun binary-heap-take-min (h)
  (if (empty-p h)
      nil
      (let* ((a (binary-heap-heap h))
	     (size (binary-heap-size h))
	     (min (svref a 1)))
	(cond ((= size 1)
	       (setf (binary-heap-size h) 0)
	       (setf (svref a 1) nil))
	      (t
	       (setf (svref a 1) (svref a size))
	       (setf (svref a size) nil)
	       (setf (binary-heap-size h) (1- size))
	       (fixdown h 1)))
	min)))

(declaim (ftype (function (binary-heap t)) binary-heap-insert))
(defun binary-heap-insert (h key)
  (let ((new-size (1+ (binary-heap-size h))))
    (ensure-capacity h new-size)
    (setf (binary-heap-size h) new-size)
    (setf (svref (binary-heap-heap h) new-size) key)
    (fixup h new-size key)))
