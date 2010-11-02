;;; -*- Coding: utf-8; Mode: Lisp; Syntax: Common Lisp; -*-

;; 状態: 連続値の二次元ベクトル
;; 行動: 上下左右の4行動(離散値)

(defclass continuous-pursuit-environment (rl-environment)
  ((hunter-location :accessor hunter-location-of :initarg :hunter-location
		     :initform (make-vector 2
					    :element-type 'double-float
					    :initial-contents '(1d0 1d0)))
   (target-location :accessor target-location-of :initarg :target-location
		    :initform (make-vector 2
					   :element-type 'double-float
					   :initial-contents '(9d0 9d0)))
   (n-of-step :accessor n-of-step-of :initform 1)
   (terminal? :accessor terminal?-of :initform nil)))

(defconstant UP 0)
(defconstant DOWN 1)
(defconstant RIGHT 2)
(defconstant LEFT 3)

(defun initialize-continuous-pursuit-environment (environment)
  (setf (aref (hunter-location-of environment) 0 0) 3d0
	(aref (hunter-location-of environment) 1 0) 3d0
	(aref (target-location-of environment) 0 0) 9d0
	(aref (target-location-of environment) 1 0) 9d0
	(n-of-step-of environment) 0
	(terminal?-of environment) nil))

;; direction: up=1, down=2, right=3, left=4
(defun continuous-pursuit-move (current-location direction)
  (let ((amount-of-movement (random-normal :mean 1d0 :sd 0.01))) ; 
    (cond ((= direction UP)
	   (let ((next-y (+ (aref current-location 1 0) amount-of-movement)))  
	     (make-vector 2
			  :element-type 'double-float
			  :initial-contents (list (aref current-location 0 0)
						  (if (> next-y 10) (- next-y 10) next-y)))))
	  ((= direction DOWN)
	   (let ((next-y (- (aref current-location 1 0) amount-of-movement)))
	     (make-vector 2 :element-type 'double-float
			  :initial-contents (list (aref current-location 0 0)
						  (if (< next-y 0) (+ next-y 10) next-y)))))
	  ((= direction RIGHT)
	   (let ((next-x (+ (aref current-location 0 0) amount-of-movement)))  
	     (make-vector 2
			  :element-type 'double-float
			  :initial-contents (list (if (> next-x 10) (- next-x 10) next-x)
						  (aref current-location 1 0)))))
	  ((= direction LEFT)
	   (let ((next-x (- (aref current-location 0 0) amount-of-movement)))
	     (make-vector 2 :element-type 'double-float
			  :initial-contents (list (if (< next-x 0) (+ next-x 10) next-x)
						  (aref current-location 1 0))))))))

(defmethod respond ((environment continuous-pursuit-environment) a)
  (let* ((next-hunter-location (continuous-pursuit-move (hunter-location-of environment) a)) ; 行動
	 (next-target-location (continuous-pursuit-move (target-location-of environment) (random 4))) ; (target-location-of environment)
	 (distance (euclidean-norm (m- next-hunter-location next-target-location)))
	 (reward (if (> distance 1d0) 0d0 (/ 100d0 (* (n-of-step-of environment)
						      (n-of-step-of environment))))))
    (if (terminal?-of environment)
	(values nil 0) ; -1で終了状態を表す
	(progn
	  (if (> reward 0d0) (setf (terminal?-of environment) t))
	  (setf (hunter-location-of environment) next-hunter-location
		(target-location-of environment) next-target-location)
	  (incf (n-of-step-of environment))
	  (values (vector-cat next-hunter-location next-target-location) reward)))))

(defun plot-state (s)
  (nplot:nplot-list (list (list (aref s 1 0) -1) (list -1 (aref s 3 0)))
		    :x-list (list (aref s 0 0) (aref s 2 0)) :style "points"
		    :xrange '(0 10) :yrange '(0 10)))

;; simulation
(defparameter pursuit-env (make-instance 'continuous-pursuit-environment))

(n-times 1000
  (plot-state (respond pursuit-env (random 4)))
  (sleep 0.01))

(defparameter pursuit-env (make-instance 'continuous-pursuit-environment))
(defparameter RBF-agent
  (make-RBF-sarsa-agent 4
			(n-times-collect 10
			  (make-vector 4 :element-type 'double-float
				       :initial-contents (list (random 10d0) (random 10d0)
							       (random 10d0) (random 10d0))))
			1d0
			:alpha 0.1d0 :gamma 0.9d0 :-lambda 0))

(nlet itr ()
  (initialize-continuous-pursuit-environment pursuit-env)
  (rl-process (vector-cat (hunter-location-of pursuit-env)
			  (target-location-of pursuit-env))
	      pursuit-env RBF-agent)
  @(car (return-list-of RBF-agent))
  (finish-output)
  (itr))