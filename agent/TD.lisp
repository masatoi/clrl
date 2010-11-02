;;; -*- Coding: utf-8; Mode: Lisp; Syntax: Common Lisp; -*-

(defclass td-agent (rl-agent)
  ((n-of-action :accessor n-of-action-of :initarg :n-of-action)
   (value-table :accessor value-table-of :initarg :value-table :initform nil)
   (alpha :accessor alpha-of :initarg :alpha :initform 0.1)
   (gamma :accessor gamma-of :initarg :gamma :initform 0.9)))

(defun make-td-agent (n-of-state n-of-action &key (alpha 0.1) (gamma 0.9))
  (make-instance 'td-agent
		 :n-of-action n-of-action
		 :value-table (make-array n-of-state
					  :element-type 'double-float
					  :initial-element 0d0)
		 :alpha alpha :gamma gamma))

(defmethod action-selector ((agent td-agent) s)
  (declare (ignore s))
  (random (n-of-action-of agent)))

;; params: current-state next-state reward
(defmethod learning ((agent td-agent) params)
  (let ((current-state (car params))
	(next-state (cadr params))
	(reward (caddr params)))
    (let* ((TD-error (+ reward
			(* (gamma-of agent) (aref (value-table-of agent) next-state))
			(- (aref (value-table-of agent) current-state))))
	   (new-value (+ (aref (value-table-of agent) current-state) (* (alpha-of agent) TD-error))))
      (setf (aref (value-table-of agent) current-state) new-value))))

(defmethod rl-process (init-state environment (agent td-agent))
  (nlet itr ((s init-state))
    (multiple-value-bind (next-s reward)
	(respond environment (action-selector agent s))
      (incf (return-accumulator-of agent) reward)
      (if (= next-s TERMINAL-STATE) ; 状態が-1のときに終了
	  ;; 終了時に総報酬をreturn-listに記録し、このエピソードの総報酬を返す
	  (let ((return-acc (return-accumulator-of agent)))
	    (setf (return-list-of agent) (cons (return-accumulator-of agent) (return-list-of agent))
		  (return-accumulator-of agent) 0)
	    return-acc)
	  (progn
	    (learning agent (list s next-s reward))
	    (itr next-s))))))

;;; simulation
(defun format-value-table (td-agent)
  (loop for j from 0 to 3
       do
       (loop for i from 0 to 11
	  do (format t "~4f " (aref (value-table-of td-agent) (+ (* 12 j) i))))
       (format t "~%")))

;;; simulation
(defparameter td-agent (make-td-agent 48 4 :alpha 0.1 :gamma 0.99))
(defparameter cliff-env (make-instance 'cliff-warking-environment))
(format-value-table td-agent)
(rl-process (location->index 3 0) cliff-env td-agent)
(format-value-table td-agent)
(n-times 100 (rl-process (location->index 3 0) cliff-env td-agent))
