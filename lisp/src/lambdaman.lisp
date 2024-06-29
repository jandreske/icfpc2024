(defpackage :lambdaman
  (:use :cl :binary-heap)
  (:export :path-to-pills :read-grid :grid :grid-width :grid-height))

(in-package :lambdaman)

; limit coordinates to sensible sizes
(deftype coordinate () '(mod 10000))

(declaim (inline %make-grid grid-width grid-height grid-cells))

(defstruct (grid (:constructor %make-grid))
  (width (error "missing argument width") :type coordinate)
  (height (error "missing argument height") :type coordinate)
  (cells (error "missing argument cells") :type (simple-array character (* *))))

(defun make-grid (width height init)
  (%make-grid :width width
	      :height height
	      :cells (make-array (list width height) :element-type 'character :initial-element init)))

(declaim (inline grid-ref grid-set))

(declaim (ftype (function (grid coordinate coordinate) character) grid-ref))
(defun grid-ref (grid x y)
  (aref (grid-cells grid) x y))

(declaim (ftype (function (grid coordinate coordinate character) t) grid-set))
(defun grid-set (grid x y value)
  (setf (aref (grid-cells grid) x y) value))

(declaim (inline make-pos pos-x pos-y))
(defstruct pos
  (x 0 :type coordinate)
  (y 0 :type coordinate))
#+sbcl (declaim (sb-ext:freeze-type pos))

(declaim (ftype (function (pos pos) t) pos=))
(defun pos= (a b)
  (declare (optimize speed))
  (and (= (pos-x a) (pos-x b))
       (= (pos-y a) (pos-y b))))

(declaim (ftype (function (pos pos) t) pos-less-p))
(defun pos-less-p (a b)
  (declare (optimize speed))
  (let ((ax (pos-x a))
	(bx (pos-x b)))
    (or (< ax bx)
	(and (= ax bx)
	     (< (pos-y a) (pos-y b))))))

(declaim (ftype (function (pos pos) coordinate) manhattan-distance)) 
(defun manhattan-distance (a b)
  (declare (optimize speed))
  (let ((dx (abs (- (pos-x a) (pos-x b))))
	(dy (abs (- (pos-y a) (pos-y b)))))
    (+ dx dy)))

(declaim (ftype (function (pos pos) character) get-move))
(defun get-move (from to)
  (cond ((< (pos-x to) (pos-x from)) #\L)
	((> (pos-x to) (pos-x from)) #\R)
	((< (pos-y to) (pos-y from)) #\U)
	((> (pos-y to) (pos-y from)) #\D)
	(t (error "no direction to move from ~S to ~S" from to))))

(declaim (inline make-cost+pos))
(defstruct cost+pos
  (cost 0 :type (mod 1000000))
  (pos (error "missing argument pos") :type pos))

(declaim (ftype (function (cost+pos cost+pos) t) cost-less-p))
(defun cost-less-p (a b)
  (declare (optimize speed))
  (let ((cost-a (cost+pos-cost a))
	(cost-b (cost+pos-cost b)))
    (or (< cost-a cost-b)
	(and (= cost-a cost-b)
	     (pos-less-p (cost+pos-pos a) (cost+pos-pos b))))))

(declaim (ftype (function (grid fixnum fixnum) t) can-move))
(defun can-move (grid x y)
  (declare (optimize speed))
  (and (>= x 0) (< x (grid-width grid))
       (>= y 0) (< y (grid-height grid))
       (not (char= #\# (grid-ref grid x y)))))

(declaim (ftype (function (grid pos) cons) neighbors))
(defun neighbors (grid p)
  (declare (optimize speed))
  (let ((r nil))
    (when (can-move grid (1- (pos-x p)) (pos-y p))
      (push (make-pos :x (1- (pos-x p)) :y (pos-y p)) r))
    (when (can-move grid (pos-x p) (1- (pos-y p)))
      (push (make-pos :x (pos-x p) :y (1- (pos-y p))) r))
    (when (can-move grid (1+ (pos-x p)) (pos-y p))
      (push (make-pos :x (1+ (pos-x p)) :y (pos-y p)) r))
    (when (can-move grid (pos-x p) (1+ (pos-y p)))
      (push (make-pos :x (pos-x p) :y (1+ (pos-y p))) r))
    r))


(declaim (inline pos-ref pos-set))

(declaim (ftype (function ((array cost+pos (* *)) pos) cost+pos) pos-ref))
(defun pos-ref (dist pos)
  (aref dist (pos-x pos) (pos-y pos)))

(declaim (ftype (function ((array cost+pos (* *)) pos cost+pos) t) pos-set))
(defun pos-set (dist pos value)
  (setf (aref dist (pos-x pos) (pos-y pos)) value))


(defun path (dist src dest)
  (labels ((from (path)
	     (let ((pos (car path)))
	       (if (pos= pos src)
		   path
		   (let ((prev (cost+pos-pos (pos-ref dist pos))))
		     (from (cons prev path)))))))
    (from (list dest))))

(defun shortest-path (grid src dest)
  (declare (type pos src dest))
  (let* ((todo (make-binary-heap #'cost-less-p))
	 (unvisited (make-cost+pos :cost 999999 :pos (make-pos)))
	 (dist (make-array (list (grid-width grid) (grid-height grid))
			   :element-type 'cost+pos
			   :initial-element unvisited))
	 (init (make-cost+pos :cost 0 :pos src)))
    (binary-heap-insert todo init)
    (pos-set dist src init)
    (labels ((loop1 (curr)
		(declare (optimize (speed 2)))
		(if (not curr)
		    nil ; no path
		    (let ((curr (cost+pos-pos curr)))
		      (declare (type pos curr))
		      (if (pos= curr dest)
			  (path dist src dest)
			  (let ((curr-cost (cost+pos-cost (pos-ref dist curr))))
			    (do ((nn (neighbors grid curr) (cdr nn)))
				((null nn) (loop1 (binary-heap-take-min todo)))
			      (let* ((next (car nn))
				     (cost-so-far (cost+pos-cost (pos-ref dist next)))
				     (new-cost (+ curr-cost 1)))
				(if (< new-cost cost-so-far)
				    (let* ((prio (+ new-cost (manhattan-distance dest next)))
					   (item (make-cost+pos :cost prio :pos next)))
				      (binary-heap-insert todo item)
				      (pos-set dist next (make-cost+pos :cost new-cost :pos curr))))))))))))
      (loop1 (binary-heap-take-min todo)))))


(defun read-map-from-stream (f)
  (let* ((lines (remove-if #'(lambda (s) (string= s "")) (asdf::slurp-stream-lines f)))
	 (width (length (car lines)))
	 (height (length lines))
	 (map (make-grid width height #\space))
	 (y 0))
    (dolist (ln lines)
      (dotimes (x width)
	(grid-set map x y (char ln x)))
      (incf y))
    map))
    

(defun read-grid (file)
  (with-open-file (ifile file :direction :input)
    (read-map-from-stream ifile)))

(declaim (ftype (function (grid pos character) (or null pos)) find-nearest))
(defun find-nearest (grid pos c)
  (declare (optimize speed))
  ;; uh..
  (let ((min-dist 1000000)
	(best-pos nil))
    (dotimes (x (grid-width grid))
      (dotimes (y (grid-height grid))
	(when (char= (grid-ref grid x y) c)
	  (let* ((p (make-pos :x x :y y))
		 (dist (manhattan-distance pos p)))
	    (when (< dist min-dist)
	      (setf min-dist dist)
	      (setf best-pos p))))))
    best-pos))

(declaim (ftype (function (grid) string) path-to-pills))
(defun path-to-pills (grid)
  (let ((start (find-nearest grid (make-pos) #\L)))
    (unless start
      (error "no initial pos found"))
    (do* ((pos start)
	  (path nil)
	  (pill (find-nearest grid pos #\.)
		(find-nearest grid pos #\.)))
	 ((not pill) (coerce (nreverse path) 'string))
      (let ((sp (shortest-path grid pos pill)))
	(unless (>= (length sp) 2)
	  (error "no path found: ~S -> ~S" pos pill))
	(let ((dest (cadr sp)))
	  (push (get-move pos dest) path)
	  (setf pos dest)
	  (grid-set grid (pos-x pos) (pos-y pos) #\space))))))
