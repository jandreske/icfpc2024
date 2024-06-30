(defpackage :icfplang
  (:use :cl)
  (:import-from :alexandria :array-index :array-length)
  (:export :icfp->ascii :ascii->icfp :parse-program :encode :eval-icfp :simplify :optimize-moves))


(in-package :icfplang)

(defvar +to-ascii+
  (concatenate 'string
	       "abcdefghijklmnopqrstuvwxyz"
	       "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
	       "0123456789"
	       "!\"#$%&'()*+,-./:;<=>?@[\\]^_`|~ "
	       (string #\newline)))

(defconstant +max-reductions+ 10000000)

(declaim (inline atomp))
(defun atomp (e)
  (not (consp e)))

(defun icfp->ascii (s)
  (if (stringp s)
      (map 'string #'(lambda (c) (char +to-ascii+ (- (char-int c) 33))) s)
      s))

(defun ascii->icfp (s)
  ;; TODO: use table lookup
  (map 'string #'(lambda (c) (code-char (+ 33 (position c +to-ascii+)))) s))

(defun unop (c)
  (ecase c
    (#\- 'negate)
    (#\! 'not)
    (#\# 'string->int)
    (#\$ 'int->string)))

(defun binop (c)
  (ecase c
    (#\$ 'apply)
    (#\+ '+)
    (#\- '-)
    (#\* '*)
    (#\/ 'truncate)
    (#\% 'rem)
    (#\< '<)
    (#\> '>)
    (#\= '=)
    (#\| 'or)
    (#\& 'and)
    (#\. 'concat)
    (#\T 'take)
    (#\D 'drop)))

(defun encode-token (tag)
  (ecase tag
    (apply "B$")
    (if "?")
    (negate "U-")
    (not "U!")
    (string->int "U#")
    (int->string "U$")
    (+ "B+")
    (- "B-")
    (* "B*")
    (truncate "B/")
    (rem "B%")
    (< "B<")
    (> "B>")
    (= "B=")
    (or "B|")
    (and "B&")
    (concat "B.")
    (take "BT")
    (drop "BD")))
    

(defun encode-expr (e)
  (typecase e
    (string (concatenate 'string "S" e))
    (number (concatenate 'string "I" (int-to-string e)))
    (null "F")
    (cons
     (case (car e)
       (lambda (concatenate 'string "L" (int-to-string (cadr e)) " " (encode-expr (caddr e))))
       (var (concatenate 'string "v" (int-to-string (cadr e))))
       (t
	(reduce #'(lambda (a b) (concatenate 'string a " " b))
		(mapcar #'encode-expr (cdr e))
		:initial-value (encode-token (car e))))))
    (t (if (eq e t) "T" (error "unknown expression: ~S" e)))))
   

(defun encode (program)
  (reduce #'(lambda (a b) (concatenate 'string a " " b))
	  (mapcar #'encode-expr program)))

(defun self-evaluating-p (expr)
  (declare (optimize speed))
  (or (atomp expr)
      (eq (car expr) 'lambda)
      (eq (car expr) 'var)))

(declaim (ftype (function (integer) string) int-to-string))
(defun int-to-string (n)
  (labels ((build (s n)
	     (if (zerop n)
		 (coerce s 'string)
		 (multiple-value-bind (q r)
		     (truncate n 94)
		   (build (cons (code-char (+ r 33)) s)
			  q)))))
    (if (zerop n)
	"!"
	(build nil n))))
		 

(declaim (ftype (function (string array-index)
			  (values (integer 0 *) array-length))
		parse-int))
(defun parse-int (s i)
  (labels ((accum (len i n)
	     (declare (optimize speed))
	     (declare (type array-length len i)
		      (type (integer 0 *) n))
	     (if (>= i len)
		 (values n i)
		 (let ((c (char s i)))
		   (if (char<= c #\space)
		       (values n (+ i 1))
		       (accum len
			      (+ i 1)
			      (+ (* 94 n) (- (char-int c) 33))))))))
    (accum (length s) i 0)))

(declaim (ftype (function (string) t)
		parse-program))
(defun parse-program (s)
  (let ((end (length s)))
    (declare (type array-length end))
    (labels ((parse (i)
	       (declare (type array-length i))
	       (when (>= i end)
		 (error "attempt to parse beyond end of program ~S" i))
	       (ecase (char s i)
		 ((#\space #\newline) (parse (+ i 1)))
		 (#\F (values nil (+ i 1)))
		 (#\T (values t (+ i 1)))
		 (#\I (parse-int s (+ i 1)))
		 (#\S (let ((p (position #\space s :start (+ i 1))))
			(values (subseq s (+ i 1) p)
				(if p p end))))
		 (#\U (multiple-value-bind (e k)
			  (parse (+ i 3))
			(values (list (unop (char s (+ i 1))) e) k)))
		 (#\B (multiple-value-bind (e1 k1)
			  (parse (+ i 3))
			(multiple-value-bind (e2 k2)
			    (parse k1)
			  (values (list (binop (char s (+ i 1))) e1 e2) k2))))
		 (#\? (multiple-value-bind (e1 k1) (parse (+ i 1))
			(multiple-value-bind (e2 k2) (parse k1)
			  (multiple-value-bind (e3 k3) (parse k2)
			    (values (list 'if e1 e2 e3) k3)))))
		 (#\L (multiple-value-bind (v k)
			  (parse-int s (+ i 1))
			(multiple-value-bind (body k2)
			    (parse k)
			  (values (list 'lambda v body) k2))))
		 (#\v (multiple-value-bind (v k)
			  (parse-int s (+ i 1))
			(values (list 'var v) k))))))
      (multiple-value-bind (e k)
	  (parse 0)
	(if (= k end)
	    e
	    (error "incomplete parse: ~S of ~S" k end))))))


(defun eval-icfp (e)
  (let ((betas 0))
    (declare (type (integer 0 *) betas))
    (declare (optimize (speed 2)))
    (labels ((evaluate (e)
	       (do ((e e (eval1 e)))
		   ((self-evaluating-p e) e)))
	     (eval1 (e)
	       (declare (optimize (speed 2)))
	       (ecase (car e)
		 (if (if (evaluate (cadr e))
			 (caddr e)
			 (cadddr e)))
		 (negate (- (evaluate (cadr e))))
		 (not (not (evaluate (cadr e))))
		 (string->int
		  (let ((s (evaluate (cadr e))))
		    (multiple-value-bind (n e)
			(parse-int s 0)
		      (declare (ignore e))
		      n)))
		 (int->string
		  (int-to-string (evaluate (cadr e))))
		 (apply
		  (eval-apply (evaluate (cadr e)) (caddr e)))
		 (+ (+ (evaluate (cadr e)) (evaluate (caddr e))))
		 (- (- (evaluate (cadr e)) (evaluate (caddr e))))
		 (* (* (evaluate (cadr e)) (evaluate (caddr e))))
		 (truncate (truncate (evaluate (cadr e)) (evaluate (caddr e))))
		 (rem (rem (evaluate (cadr e)) (evaluate (caddr e))))
		 (< (< (evaluate (cadr e)) (evaluate (caddr e))))
		 (> (> (evaluate (cadr e)) (evaluate (caddr e))))
		 (= (equal (evaluate (cadr e)) (evaluate (caddr e))))
		 (or (or (evaluate (cadr e)) (evaluate (caddr e))))
		 (and (and (evaluate (cadr e)) (evaluate (caddr e))))
		 (concat (concatenate 'string
				      (evaluate (cadr e))
				      (evaluate (caddr e))))
		 (take (subseq (evaluate (caddr e))
			       0
			       (evaluate (cadr e))))
		 (drop (subseq (evaluate (caddr e))
			       (evaluate (cadr e))))))
	     (eval-apply (f a)
	       (declare (optimize (speed 2)))
	       (if (eq (car f) 'lambda)
		   (progn ; (incf betas)
			  (substit (cadr f) a (caddr f)))
		   (error "B$: lambda expected, but got ~S" f))))
      (let ((val (evaluate e)))
	(values val betas)))))

(defun substit (var val e)
  (declare (type (integer 0 *) var))
  (labels ((sub (e)
	     (if (atomp e)
		 e
		 (case (car e)
		   (var (if (= var (cadr e)) val e))
		   (if (cons 'if (mapcar #'sub (cdr e))))
		   (lambda (if (= var (cadr e))
			       e
			       (multiple-value-bind (v1 b1)
				   (rename-if-free (cadr e) (caddr e) val)
				 (list 'lambda v1 (sub b1)))))
		   (t
		    (cons (car e)
			  (mapcar #'sub (cdr e))))))))
    (sub e)))

(defun rename-if-free (v body expr)
  (if (free-p v expr)
      (let ((v1 (+ (max-var expr) 1)))
	(values v1 (rename v v 1 body)))
      (values v body)))

(defun free-p (v e)
  (declare (type (integer 0 *) v))
  (if (atomp e)
      nil
      (case (car e)
	(var (= v (cadr e)))
	(lambda (and
		 (not (= v (cadr e)))
		 (free-p v (caddr e))))
	(if (or (free-p v (cadr e))
		(free-p v (caddr e))
		(free-p v (cadddr e))))
	(t (or (free-p v (cadr e))
	       (free-p v (caddr e)))))))

(defun max-var (e)
  (if (atomp e)
      -1
      (case (car e)
	(var (cadr e))
	(lambda (max (cadr e) (max-var (caddr e))))
	(if (max (max-var (cadr e))
		 (max-var (caddr e))
		 (max-var (cadddr e))))
	(t (max (max-var (cadr e))
		(max-var (caddr e)))))))

(defun rename (v v1 e)
  (if (atomp e)
      e
      (case (car e)
	(var (if (= v (cadr e)) (list 'var v1) e))
	(lambda (list 'lambda
		      (if (= v (cadr e)) v1 (cadr e))
		      (rename v v1 (caddr e))))
	(if (list 'if
		  (rename v v1 (cadr e))
		  (rename v v1 (caddr e))
		  (rename v v1 (cadddr e))))
	(t (cons (car e) (mapcar #'(lambda (e) (rename v v1 e)) (cdr e)))))))

(defparameter +Y+ '(lambda 1 (apply
			      (lambda 2 (apply (var 1) (apply (var 2) (var 2))))
			      (lambda 2 (apply (var 1) (apply (var 2) (var 2)))))))
		   

(defvar +REPEAT+ `(apply ,+Y+
			 (lambda 3 ; recurse
			   (lambda 4 ; string
			     (lambda 5 ; n
			       (if (= (var 5) 1)
				   (var 4)
				   (concat (var 4)
					   (apply (apply (var 3) (var 4) (- (var 5) 1))))))))))

;; (defvar +LAM6+ `(apply (lambda 6 (apply (apply (var 6) "L") 18684)) ,+repeat+))
(defvar +LAM6+ `(apply (apply ,+REPEAT+ "L") 18684))

(defvar +LAM9+
  `(APPLY
    (APPLY
     ,+Y+
     (LAMBDA 1
       (LAMBDA 2
	 (IF (= (VAR 2) 2499)
	     ""
	     (CONCAT
	      (if (= (rem (truncate (var 2) 50) 2) 0) ; even row
		  (if (= (rem (var 2) 50) 49)
		      ,(ascii->icfp "D")
		      ,(ascii->icfp "R"))
		  (if (= (rem (var 2) 50) 49)
		      ,(ascii->icfp "D")
		      ,(ascii->icfp "L")))
	      (APPLY (VAR 1) (+ (VAR 2) 1)))))))
    0))


(defun optimize-moves (moves)
  (let ((groups (util:find-repetitions moves))
	(repeat '(var 10)))
    (labels ((run (str ms)
	       (cond ((null ms) (coerce (nreverse str) 'string))
		     ((< (car (car ms)) 8)
		      (run (cons-times (caar ms) (cdar ms) str) (cdr ms)))
		     (t
		      `(concat ,(coerce (nreverse str) 'string)
			       (concat (apply (apply ,repeat ,(string (cdar ms))) ,(caar ms))
				       ,(run nil (cdr ms))))))))
      `(apply (lambda 10 ,(run nil groups)), +repeat+))))


(defun cons-times (n x xs)
  (if (zerop n)
      xs
      (cons x (cons-times (- n 1) x xs))))

(defun compile-expr (e)
  (cond ((null e) 'nil)
	((numberp e) e)
	((stringp e) e)
	((atom e) ; must be t
	 't)
	((eq (car e) 'lambda) e)
	((eq (car e) 'if) `(if ,(compile-expr (cadr e))
			       ,(compile-expr (caddr e))
			       ,(compile-expr (cadddr e))))
	((eq (car e) 'apply)
	 `(eval-application ,(compile-expr (cadr e)) (delay ,(compile-expr (caddr e)))))
	((eq (car e) 'var) 'todo)
	((eq (car e) '+) (compile-bin "primPlus" e))
	((eq (car e) '-) (compile-bin "primMinus" e))
	((eq (car e) '*) (compile-bin "primMul" e))
	((eq (car e) '/) (compile-bin "primQuot" e))
	((eq (car e) '%) (compile-bin "primRem" e))
	((eq (car e) '<) (compile-bin "primLess" e))
	((eq (car e) '>) (compile-bin "primGreater" e))
	((eq (car e) '=) (compile-bin "primEq" e))
	((eq (car e) 'take) (compile-bin "primTake" e))
	((eq (car e) 'drop) (compile-bin "primDrop" e))
	((eq (car e) 'and) (compile-bin "primAnd" e))
	((eq (car e) 'or) (compile-bin "primOr" e))
	((eq (car e) 'concat) (compile-bin "primConcat" e))
	((eq (car e) 'not) (compile-unary "primNot" e))
	((eq (car e) 'negate) (compile-unary "primNegate" e))
	((eq (car e) 'int->string) (compile-unary "primIntToString" e))
	((eq (car e) 'string->int) (compile-unary "primStringToInt" e))
	(t (error "compile: unknown expression type: ~S" e))))

(defun compile-bin (op e)
  (format nil "(~A ~A ~A)~%" op (compile-to-hs (cadr e)) (compile-to-hs (caddr e))))

(defun compile-unary (op e)
  (format nil "(~A ~A)~%" op (compile-to-hs (cadr e))))


  
(defun simplify (e)
  "Simplify for display."
  (flatten-app
   (join-lambdas
    (var-to-symbol
     ;(static-eval
      (subst-if 'Y #'y-combinator-p e)))))

(defun flatten-app (e)
  (if (consp e)
      (let ((op (car e))
	    (rest (mapcar #'flatten-app (cdr e))))
	(if (and (member op '(and or + * concat))
		 (consp (car rest))
		 (eq op (car (car rest))))
	    `(,op ,@(cdar rest) ,@(cdr rest))
	    `(,op ,@rest)))
      e))

(defun join-lambdas (e)
  (if (consp e)
      (let ((op (car e))
	    (rest (mapcar #'join-lambdas (cdr e))))
	(if (and (eq op 'lambda)
		 (consp (cadr rest))
		 (eq (caadr rest) 'lambda))
	    `(lambda ,(join-vars (cadr e) (cadr (cadr rest))) ,(caddr (cadr rest)))
	    `(,op ,@rest)))
      e))

(defun join-vars (a b)
  (let ((a (if (atom a) (list a) a))
	(b (if (atom b) (list b) b)))
    (concatenate 'list a b)))

(defun var-to-symbol (e)
  (if (consp e)
      (case (car e)
	(var (read-from-string (format nil "v~a" (cadr e))))
	(t (cons (car e) (mapcar #'var-to-symbol (cdr e)))))
      e))

(defun y-combinator-p (e)
  (equal e +Y+))
		       
(defun static-eval (e)
  (if (not (consp e))
      e
      (case (car e)
	(apply `(apply ,(static-eval (cadr e)) ,(static-eval (caddr e))))
	(var e)
	(lambda `(lambda ,(cadr e) ,(static-eval (caddr e))))
	(if (static-eval-if (static-eval (cadr e))
			    (static-eval (caddr e))
			    (static-eval (cadddr e))))
	(or (static-eval-or (static-eval (cadr e))
			    (static-eval (caddr e))))
	(and (static-eval-and (static-eval (cadr e))
			      (static-eval (caddr e))))
	(not (static-eval-not (static-eval (cadr e))))
	(t (cons (car e) (mapcar #'static-eval (cdr e)))))))

(defun static-eval-if (test c a)
  (cond ((null test) a)
	((eq test 't) c)
	(t `(if ,test ,c ,a))))

(defun static-eval-not (e)
  (if (and (consp e) (eq (car e) 'not))
      (cadr e)
      e))

(defun static-eval-or (p q)
  (cond ((or (eq p 't) (eq q 't)) t)
	((and (consp p)
	      (eq (car p) 'not)
	      (equal (cadr p) q))
	 t)
	((and (consp q)
	      (eq (car q) 'not)
	      (equal (cadr q) p))
	 t)
	(t `(or ,p ,q))))

(defun static-eval-and (p q)
  (cond ((or (null p) (null q)) nil)
	((and (consp p)
	      (eq (car p) 'not)
	      (equal (cadr p) q))
	 nil)
	((and (consp q)
	      (eq (car q) 'not)
	      (equal (cadr q) p))
	 nil)
	(t `(and ,p ,q))))
  
