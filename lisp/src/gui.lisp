(defpackage :gui
  (:use :cl :nodgui)
  (:export :show))

(in-package :gui)


;;; see: https://peterlane.codeberg.page/ltk-examples/

(defconstant +map-width+ 800)
(defconstant +map-height+ 800)



(defun show (&key spaceship)
  (with-nodgui ()
    (wm-title *tk* "ICFPC 2024")
    (let* ((content (make-instance 'frame))
	   (frame (make-instance 'canvas :master content
				 :borderwidth 5 :relief :ridge
				 :width +map-width+ :height +map-height+))
	   (name-label (make-instance 'label :master content
				      :text "Name"))
	   (name (make-instance 'entry :master content))
	   (cb-1 (make-instance 'check-button :master content :text "One"))
	   (cb-2 (make-instance 'check-button :master content :text "Two"))
	   (cb-3 (make-instance 'check-button :master content :text "Three"))
	   (ok (make-instance 'button :master content :text "OK"
			      :command (lambda () (setf (text name) "You clicked OK!"))
			      ))
	   (cancel (make-instance 'button :master content :text "Cancel")))
      ;; -- some adjustments to the widgets/frames
      (configure content :padding "3 3 12 12")
      (setf (value cb-1) t
	    (value cb-2) nil
	    (value cb-3) t)
      ;; -- layout the widgets in the grid
      (grid content 0 0 :sticky "nsew")                               ; fills entire window
      (grid frame 0 0 :columnspan 3 :rowspan 2 :sticky "nsew")        ; 3 columns, 2 row, top-left
      (grid name-label 0 3 :columnspan 2 :sticky "nw" :padx 5)        ; first row, last column, spans 2 columns
      (grid name 1 3 :columnspan 2 :sticky "new" :pady 5 :padx 5)
      (grid cb-1 3 0)
      (grid cb-2 3 1)
      (grid cb-3 3 2)
      (grid ok 3 3)
      (grid cancel 3 4)
      ;; -- tidy up the layout and resizing properties
      (grid-columnconfigure *tk* 0 :weight 1)                         ; 4
      (grid-rowconfigure *tk* 0 :weight 1)
      (grid-columnconfigure content 0 :weight 3)
      (grid-columnconfigure content 1 :weight 3)
      (grid-columnconfigure content 2 :weight 3)
      (grid-columnconfigure content 3 :weight 1)
      (grid-columnconfigure content 4 :weight 1)
      (grid-rowconfigure content 1 :weight 1)

      (when spaceship
	(draw-locations frame spaceship)))))

(defun draw-locations (canvas locations)
  (let* ((galaxy (spaceship::galaxy-size locations))
	 (x1 (car galaxy))
	 (y1 (cadr galaxy))
	 (x2 (caddr galaxy))
	 (y2 (cadddr galaxy))
	 (width (+ 1 (- x2 x1)))
	 (height (+ 1 (- y2 y1)))
	 (scale
	   (if (and (<= width +map-width+) (<= height +map-height+))
	       1
	       (max (/ width +map-width+)
		    (/ height +map-height+))))
	 (plotted 0))
    (format t "Galaxy: ~a~%" galaxy)
    (flet ((conv-x (x)
	     (/ (- x x1) scale))
	   (conv-y (y)
	     (- +map-height+ (+ 0.01 (/ (- y y1) scale)))))
      (do ((i 0 (+ i 2)))
	  ((>= i (length locations)))
	(when (draw-point canvas (conv-x (aref locations i)) (conv-y (aref locations (+ i 1))))
	  (incf plotted)))
      (format t "~a of ~a locations plotted~%" plotted (/ (length locations) 2))
      )))

(defun draw-point (canvas x y)
  (if (or (< x 0) (< y 0) (>= x +map-width+) (>= y +map-height+))
      (progn (format t "Outside: ~,2f ~,2f~%" x y) nil)
      (progn (create-rectangle canvas x y (+ x 1) (+ y 1))
	     t)))

