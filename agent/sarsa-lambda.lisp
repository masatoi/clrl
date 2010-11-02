;;; -*- Coding: utf-8; Mode: Lisp; Syntax: Common Lisp; -*-

;;; テーブル型Q(lambda)
;;  状態と行動はそれぞれ整数で表現され、行動価値テーブルのインデックスになっている

(defclass sarsa-lambda-agent (rl-agent)
  ((n-of-state :accessor n-of-state-of :initarg :n-of-state)
   (n-of-action :accessor n-of-action-of :initarg :n-of-action)
   (action-value-table :accessor action-value-table-of :initarg :action-value-table)
   (eligibility-table :accessor eligibility-table-of :initarg :eligibility-table)
   (alpha :accessor alpha-of :initarg :alpha :initform 0.1)
   (gamma :accessor gamma-of :initarg :gamma :initform 0.9)
   (-lambda :accessor -lambda-of :initarg :-lambda :initform 0.8)))

(defun make-sarsa-lambda-agent (n-of-state n-of-action &key (alpha 0.1) (gamma 0.9) (-lambda 0.8))
  (make-instance 'sarsa-lambda-agent
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

;; ;; random selector
;; (defmethod action-selector ((agent sarsa-lambda-agent) s)
;;   (declare (ignore s))
;;   (random (n-of-action-of agent)))

;; epsilon-greedy selector
(defmethod action-selector ((agent sarsa-lambda-agent) s)
  (let ((epsilon 0.01)
	(greedy-action (argmax-action-with-fixed-state (action-value-table-of agent) s)))
    (if (> (random 1.0) epsilon)
	greedy-action
	(random (n-of-action-of agent)))))

;; ;; Boltzmann selector
;; (defun sarsa-Boltzmann-selection-probabilities (s agent)
;;   (let* ((tau 0.25)
;; 	 (numerator-list (loop for ak from 0 to (1- (n-of-action-of agent))
;; 			    collect (exp (/ (Q-value-from-RBF s ak agent) tau))))
;; 	 (denominator (loop for numerator in numerator-list sum numerator)))
;;     (if (zerop denominator)
;; 	(make-list (n-of-action-of agent) :initial-element (/ 1.0d0 (n-of-action-of agent)))
;; 	(loop for numerator in numerator-list collect (/ numerator denominator)))))

;; (defun accumulated-probabilities-list (probabilities-list)
;;   (nlet itr ((acc 0)
;; 	     (prob-lst probabilities-list)
;; 	     (product '()))
;;     (if (null prob-lst)
;; 	(nreverse product)
;; 	(let ((current-acc (+ acc (car prob-lst))))
;; 	  (itr current-acc (cdr prob-lst) (cons current-acc product))))))

;; (defmethod action-selector ((agent RBF-sarsa-agent) s)
;;   (let* ((random-num (random 1.0))
;; 	 (selected-action (position-if
;; 			   (lambda (x) (< random-num x))
;; 			   (accumulated-probabilities-list (sarsa-Boltzmann-selection-probabilities s agent)))))
;;     (if selected-action selected-action (1- (n-of-action-of agent)))))

(defun sarsa-lambda-learning! (s a next-s next-a r agent)
  (let ((td-error (+ r
		     (* (gamma-of agent)
			(aref (action-value-table-of agent) next-s next-a))
		     (- (aref (action-value-table-of agent) s a)))))
    (incf (aref (eligibility-table-of agent) s a))
    (loop for s from 0 to (1- (n-of-state-of agent)) do
	 (loop for a from 0 to (1- (n-of-action-of agent)) do
	      (setf (aref (action-value-table-of agent) s a)
		    (+ (aref (action-value-table-of agent) s a)
		       (* (alpha-of agent) td-error (aref (eligibility-table-of agent) s a))))
	      (setf (aref (eligibility-table-of agent) s a)
		    (* (gamma-of agent) (-lambda-of agent)
		       (aref (eligibility-table-of agent) s a)))))))

(defmethod rl-process (init-state environment (agent sarsa-lambda-agent))
  (nlet itr ((s init-state) (a (action-selector agent init-state)))
    (multiple-value-bind (next-s r)
	(respond environment a)
      (incf (return-accumulator-of agent) r)
      (if (null next-s)
	  ;; 終了時に総報酬をreturn-listに記録し、このエピソードの総報酬を返す
	  (let ((return-acc (return-accumulator-of agent)))
	    (setf (return-list-of agent) (cons (return-accumulator-of agent) (return-list-of agent))
		  (return-accumulator-of agent) 0)
	    return-acc)
	  (let ((next-a (action-selector agent next-s)))
	    (sarsa-lambda-learning! s a next-s next-a r agent)
	    (itr next-s next-a))))))