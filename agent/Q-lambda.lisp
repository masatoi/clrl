;;; -*- Coding: utf-8; Mode: Lisp; Syntax: Common Lisp; -*-

;;; テーブル型Q(lambda)
;;  状態と行動はそれぞれ整数で表現され、行動価値テーブルのインデックスになっている

(defclass Q-lambda-agent (rl-agent)
  ((n-of-state :accessor n-of-state-of :initarg :n-of-state)
   (n-of-action :accessor n-of-action-of :initarg :n-of-action)
   (action-value-table :accessor action-value-table-of :initarg :action-value-table)
   (eligibility-table :accessor eligibility-table-of :initarg :eligibility-table)
   (alpha :accessor alpha-of :initarg :alpha :initform 0.1)
   (gamma :accessor gamma-of :initarg :gamma :initform 0.9)
   (-lambda :accessor -lambda-of :initarg :-lambda :initform 0.8)))

(defun make-Q-lambda-agent (n-of-state n-of-action &key (alpha 0.1) (gamma 0.9) (-lambda 0.8))
  (make-instance 'Q-lambda-agent
		 :n-of-state n-of-state :n-of-action n-of-action
		 :action-value-table (make-array (list n-of-state n-of-action)
						 :element-type 'double-float
						 :initial-element 1d0)
		 :eligibility-table (make-array (list n-of-state n-of-action)
						:element-type 'double-float
						:initial-element 0d0)
		 :alpha alpha :gamma gamma :-lambda -lambda))

;; 行動価値テーブルにおいて、状態を固定して価値が最大となる行動とその価値を多値で返す
(defun argmax-action-with-fixed-state (Q-table state)
  (let ((n-of-action (cadr (array-dimensions Q-table))))
    (nlet itr ((i 1)
	       (max-Q (aref Q-table state 0))
	       (max-Q-position 0))
      (if (= i n-of-action)
	  (values max-Q-position max-Q)
	  (if (< max-Q (aref Q-table state i))
	      (itr (1+ i) (aref Q-table state i) i)
	      (itr (1+ i) max-Q max-Q-position))))))

(defmethod action-selector ((agent Q-lambda-agent) s)
  (declare (ignore s))
  (random (n-of-action-of agent)))

;; epsilon-greedy selector
(defmethod action-selector ((agent Q-lambda-agent) s)
  (let ((epsilon 0.1)
	(greedy-action (argmax-action-with-fixed-state (action-value-table-of agent) s)))
    (if (> (random 1.0) epsilon)
	greedy-action
	(random (n-of-action-of agent)))))

(defmethod rl-process (init-state environment (agent Q-lambda-agent))
  (nlet itr ((s init-state) (a (action-selector agent init-state)))
    (multiple-value-bind (next-s r)
	(respond environment a)
      (incf (return-accumulator-of agent) r)
      (if (= next-s TERMINAL-STATE)
	  ;; 終了時に総報酬をreturn-listに記録し、このエピソードの総報酬を返す
	  (let ((return-acc (return-accumulator-of agent)))
	    (setf (return-list-of agent) (cons (return-accumulator-of agent) (return-list-of agent))
		  (return-accumulator-of agent) 0)
	    return-acc)
	  (let* ((next-a (action-selector agent next-s))
		 (optimal-a (argmax-action-with-fixed-state (action-value-table-of agent) next-s))
		 (td-error (+ r
			      (* (gamma-of agent)
				 (aref (action-value-table-of agent) next-s optimal-a))
			      (- (aref (action-value-table-of agent) s a)))))
	    (incf (aref (eligibility-table-of agent) s a))
	    (loop for s from 0 to (1- (n-of-state-of agent)) do
		 (loop for a from 0 to (1- (n-of-action-of agent)) do
		      (setf (aref (action-value-table-of agent) s a)
			    (+ (aref (action-value-table-of agent) s a)
			       (* (alpha-of agent) td-error (aref (eligibility-table-of agent) s a))))
		      (if (= next-a optimal-a)
			  (setf (aref (eligibility-table-of agent) s a)
				(* (gamma-of agent) (-lambda-of agent)
				   (aref (eligibility-table-of agent) s a)))
			  (setf (aref (eligibility-table-of agent) s a) 0d0))))
	    (itr next-s next-a))))))

;;; simulation
(load "/home/wiz/program/cl/wiz-util/frontend-nlisp.lisp")
(defparameter ql-agent (make-Q-lambda-agent 48 4 :alpha 0.01 :gamma 0.99 :-lambda 0.9))
(defparameter cliff-env (make-instance 'cliff-warking-environment))
(n-times 1000 (rl-process (location->index 3 0) cliff-env ql-agent))
(plot-lst (reverse (return-list-of ql-agent)))

(rl-process (location->index 3 0) cliff-env ql-agent)