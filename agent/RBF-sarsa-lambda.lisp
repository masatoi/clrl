;;; -*- Coding: utf-8; Mode: Lisp; Syntax: Common Lisp; -*-

;;; load wiz-util
(asdf:oos 'asdf:load-op :wiz-util)
(use-package :wiz-util)

;;; load NLISP and define plot function
(asdf:oos 'asdf:load-op :nlisp-wrapper)
(use-package :nlisp-wrapper)

;; ng-netのプログラムファイルをロード
(asdf:oos 'asdf:load-op :gnoem)
(use-package :gnoem)

(use-package :wiz-numerical)

(defclass bayesian-linear-regression-model ()
  ((basis-function       :accessor basis-function-of       :initarg :basis-function)
   (weight-vector        :accessor weight-vector-of        :initarg :weight-vector)
   (datum-dimension      :accessor datum-dimension-of      :initarg :datum-dimension)
   (n-of-basis-functions :accessor n-of-basis-functions-of :initarg :n-of-basis-functions)))

(defclass RBF-model (bayesian-linear-regression-model)
  ((mean-list :accessor mean-list-of :initarg :mean-list)
   (variance  :accessor variance-of  :initarg :variance)))

(defclass RBF-sarsa-agent (rl-agent)
  ((n-of-action :accessor n-of-action-of :initarg :n-of-action)
   (RBF-model-list :accessor RBF-model-list-of :initarg :RBF-model-list)
   (eligibility-vector :accessor eligibility-vector-of :initarg :eligibility-vector)
   (alpha :accessor alpha-of :initarg :alpha :initform 0.1)
   (gamma :accessor gamma-of :initarg :gamma :initform 0.9)
   (-lambda :accessor -lambda-of :initarg :-lambda :initform 0.8)))

(defun make-RBF-model (mean-list variance)
  (make-instance 'RBF-model
		 :weight-vector (make-vector (1+ (length mean-list))
					     :element-type 'double-float :initial-element 1.0d0)
		 :datum-dimension (array-dimension (car mean-list) 0)
		 :mean-list mean-list :variance variance
		 :basis-function
		 (list->vector
		  (cons (lambda (x) x 1) ; bias
			(mapcar
			 (lambda (mu)
			   (if (arrayp (car mean-list))
			       (lambda (xvec)
				 (let ((centri (m- xvec mu)))
				   (exp (- (/ (aref (m* (m-t centri) centri) 0 0)
					      (* 2 variance))))))
			       (lambda (x)
				 (let ((centri (- x mu)))
				   (exp (- (/ (square centri) (* 2 variance))))))))
			 mean-list)))
		 :n-of-basis-functions (length mean-list)))

(defun make-RBF-sarsa-agent (n-of-action mean-list variance &key (alpha 0.1) (gamma 0.9) (-lambda 0.8))
  (make-instance 'RBF-sarsa-agent
		 :n-of-action n-of-action
		 :RBF-model-list (loop for i from 0 to (1- n-of-action)
				      collect (make-RBF-model mean-list variance))
		 :eligibility-vector (make-vector (1+ (length mean-list))
						  :element-type 'double-float :initial-element 0d0)
		 :alpha alpha :gamma gamma :-lambda -lambda))

(defun Q-value-from-RBF (s a agent)
  (let ((model (nth a (RBF-model-list-of agent))))
    (let ((phi-s (mapvec (lambda (phi) (funcall phi s))
			 (basis-function-of model))))
      (aref (m* (m-t (weight-vector-of model)) phi-s) 0 0))))

(defun RBF-argmax-action-with-fixed-state (agent state)
  (nlet itr ((i 1)
	     (max-Q (Q-value-from-RBF state 0 agent))
	     (max-Q-position 0))
    (if (= i (n-of-action-of agent))
	(values max-Q-position max-Q)
	(let ((current-Q (Q-value-from-RBF state i agent)))
	  (if (< max-Q current-Q)
	      (itr (1+ i) current-Q i)
	      (itr (1+ i) max-Q max-Q-position))))))

;; epsilon-greedy selector
(defmethod action-selector ((agent RBF-sarsa-agent) s)
  (let ((epsilon 0.1)
	(greedy-action (RBF-argmax-action-with-fixed-state agent s)))
    (if (> (random 1.0) epsilon)
	greedy-action
	(random (n-of-action-of agent)))))

;; Boltzmann selector
(defun RBF-Boltzmann-selection-probabilities (s agent)
  (let* ((tau 0.25)
	 (numerator-list (loop for ak from 0 to (1- (n-of-action-of agent))
			    collect (exp (/ (Q-value-from-RBF s ak agent) tau))))
	 (denominator (loop for numerator in numerator-list sum numerator)))
    (if (zerop denominator)
	(make-list (n-of-action-of agent) :initial-element (/ 1.0d0 (n-of-action-of agent)))
	(loop for numerator in numerator-list collect (/ numerator denominator)))))

(defun accumulated-probabilities-list (probabilities-list)
  (nlet itr ((acc 0)
	     (prob-lst probabilities-list)
	     (product '()))
    (if (null prob-lst)
	(nreverse product)
	(let ((current-acc (+ acc (car prob-lst))))
	  (itr current-acc (cdr prob-lst) (cons current-acc product))))))

(defmethod action-selector ((agent RBF-sarsa-agent) s)
  ;; (sfor (i 0 3)
  ;;   (format t "Q_~A=~A~%" i (Q-value-from-RBF s i agent))) ; DEBUG
  (let* ((random-num (random 1.0))
	 (selected-action (position-if
			   (lambda (x) (< random-num x))
			   (accumulated-probabilities-list (RBF-Boltzmann-selection-probabilities s agent)))))
    (if selected-action selected-action (1- (n-of-action-of agent)))))
  
(defmethod rl-process (init-state environment (agent RBF-sarsa-agent))
  (nlet itr ((s init-state) (a (action-selector agent init-state)))
    (multiple-value-bind (next-s r)
	(respond environment a) ; 環境からの応答を観測
      (incf (return-accumulator-of agent) r)
      ;(plot-state next-s)
      (if (null next-s)
	  ;; 終了時: 総報酬をreturn-listに記録し、このエピソードの総報酬を返す
	  (let ((return-acc (return-accumulator-of agent)))
	    (setf (return-list-of agent) (cons (return-accumulator-of agent) (return-list-of agent))
		  (return-accumulator-of agent) 0)
	    return-acc)
	  (let* ((next-a (action-selector agent next-s)) ; 次状態における行動
		 (td-error (+ r ; Sarsa型TD誤差を計算
			      (* (gamma-of agent)
				 (Q-value-from-RBF next-s next-a agent))
			      (- (Q-value-from-RBF s a agent)))))
	    (let ((current-model (nth a (RBF-model-list-of agent))))
	      ;; 適格度トレースを更新
	      (setf (eligibility-vector-of agent)
		    (m+ (m* (gamma-of agent) (-lambda-of agent) (eligibility-vector-of agent))
			(mapvec (lambda (basis-func) (funcall basis-func s))
				(basis-function-of current-model))))
	      ;; 重みベクトルを更新
	      (setf (weight-vector-of current-model)
		    (m+ (weight-vector-of current-model)
			(m* (alpha-of agent) td-error (eligibility-vector-of agent)))))
	    (itr next-s next-a))))))