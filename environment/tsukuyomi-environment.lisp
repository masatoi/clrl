;;; -*- Coding: utf-8; Mode: Lisp; Syntax: Common Lisp; -*-

;;; 離散状態、離散行動で月讀の環境を定義する
;;  tsukuyomiのsessionオブジェクトとCL-HMMのhmmオブジェクトを持つ

(defclass tsukuyomi-environment (rl-environment)
  (
   (current-location :accessor current-location-of :initarg :current-location :initform '(3 0))
   (terminal? :accessor terminal?-of :initform nil)))

(defparameter *field* (make-array '(4 12)
				  :element-type 'fixnum
				  :initial-contents
				  '((-1    -1   -1   -1   -1   -1   -1   -1   -1   -1   -1  -1)
				    (-1    -1   -1   -1   -1   -1   -1   -1   -1   -1   -1  -1)
				    (-1    -1   -1   -1   -1   -1   -1   -1   -1   -1   -1  -1)
				    (-1  -100 -100 -100 -100 -100 -100 -100 -100 -100 -100 100))))

(defun initialize-cliff-warking-environment (environment)
  (setf (terminal?-of environment) nil))

(defun location->index (row col)
  (+ (* row 12) col))

(defconstant UP 0)
(defconstant DOWN 1)
(defconstant RIGHT 2)
(defconstant LEFT 3)

;; direction: up=1, down=2, right=3, left=4
(defun move (current-location direction)
  (cond ((= direction UP)
	 (if (zerop (car current-location))
	     current-location
	     (list (1- (car current-location)) (cadr current-location))))
	((= direction DOWN)
	 (if (= (car current-location) 3)
	     current-location
	     (list (1+ (car current-location)) (cadr current-location))))
	((= direction RIGHT)
	 (if (= (cadr current-location) 11)
	     current-location
	     (list (car current-location) (1+ (cadr current-location)))))
	((= direction LEFT)
	 (if (zerop (cadr current-location))
	     current-location
	     (list (car current-location) (1- (cadr current-location)))))))

(defmethod respond ((environment cliff-warking-environment) a)
  (let* ((next-location (move (current-location-of environment) a)) ; 行動
	 (reward (apply #'aref *field* next-location))) ; 報酬
    ;; 状態を更新
    (cond ((terminal?-of environment) (values nil 0))
	  ((and (= (car (current-location-of environment)) 3) ; ゴールに着いた
		(= (cadr (current-location-of environment)) 11))
	   (setf (current-location-of environment) '(3 0)
		 (terminal?-of environment) t)
	   (values (apply #'location->index (current-location-of environment)) reward))
	  ((and (= (car (current-location-of environment)) 3) ; 崖から落ちたときはスタート地点に戻す
		(> (cadr (current-location-of environment)) 0)
		(< (cadr (current-location-of environment)) 11))
	   (setf (current-location-of environment) '(3 0))
	   (values (apply #'location->index (current-location-of environment)) reward))
	  (t ; それ以外
	   (setf (current-location-of environment) next-location)
	   (values (apply #'location->index (current-location-of environment)) reward)))))

