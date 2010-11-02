;;; -*- Coding: utf-8; Mode: Lisp; Syntax: Common Lisp; -*-

(load "/home/wiz/program/cl/multivariate-gaussian.lisp")
(load "/home/wiz/program/cl/util/frontend-nlisp.lisp")
(load "/home/wiz/program/cl/cl-statistics.lisp")

(defclass bayesian-linear-regression-model ()
  ((basis-function       :accessor basis-function-of       :initarg :basis-function)
   (weight-vector        :accessor weight-vector-of        :initarg :weight-vector)
   (datum-dimension      :accessor datum-dimension-of      :initarg :datum-dimension-of)
   (n-of-basis-functions :accessor n-of-basis-functions-of :initarg :n-of-basis-functions)))

(defclass RBF-model (bayesian-linear-regression-model)
  ((mean-list :accessor mean-list-of :initarg :mean-list)
   (variance  :accessor variance-of  :initarg :variance)
   ))

(defclass action-model ()
  ((action-table :accessor action-table-of :initarg :action-table)))

(defun RBF-model-initializer (mean-list variance)
  (make-instance 'RBF-model
		 :weight-vector (make-vector (1+ (length mean-list))
					     :initial-element 1.0d0)
		 :mean-list mean-list :variance variance
		 :basis-function
		 (list->vector
		  (cons (lambda (x) x 1) ; バイアス
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

(defun regression-function (x w model)
  (let ((phi-x (mapvec (lambda (phi) (funcall phi x))
		       (basis-function-of model))))
    (aref (m* (m-t w) phi-x) 0 0)))

;;; 正則化確率的最急降下法 (least-mean-squares algorithm)

;; 訓練データ点 (x_n, t_n) と現在のパラメータを含むモデルを受け取りパラメータ更新後のモデルを返す
;; 学習率: eta, L2正則化係数: regularization-lambda
;; どちらのメタパラメータも正で微小. 正則化係数は0に近付くほど精度が高くなる(同時に過学習の危険が増す)

(defun LMS-algorithm (x_n t_n w model &key (eta 1) (regularization-lambda 0))
  (let ((phi-x (mapvec (lambda (phi) (funcall phi x_n))
		       (basis-function-of model))))
    (m+ w (m* eta (m+ (m* (- t_n (aref (m* (m-t w) phi-x) 0 0)) phi-x)
		      (m* (- regularization-lambda) w))))))

;;; TODO: 正則化パラメータやステップサイズパラメータの取り方に対してロバストにするには,
;;;       IP-SG: interior-point-stochastic-gradient

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; experiment;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; モデルを初期化
(set 'model (rbf-model-initializer
	     (mapcar #'list->vector (direct-product (seq -2d0 2d0 :by 0.2d0)
						    (seq -2d0 2d0 :by 0.2d0)))
	     0.3d0))

;;; 初期状態の予測を表示
(plot-lst-2dim (lambda (x1 x2)
		 (regression-function (vec x1 x2) (weight-vector-of model) model))
	       (seq -1.0d0 1.0d0 :by 0.1d0)
	       (seq -1.0d0 1.0d0 :by 0.1d0))

;;; load cl-store
(asdf:oos 'asdf:load-op :cl-store)

(defvar x_in.dat1.schaal)
(defvar x_out.dat1.schaal)
(setf x_in.dat1.schaal (cl-store:restore "/home/wiz/research/NGnet/oibem/data/x_in.2dim.data1"))
(setf x_out.dat1.schaal (cl-store:restore "/home/wiz/research/NGnet/oibem/data/x_out.2dim.data1"))

;;; Schaal関数のデータでLMSアルゴリズムをやる
(nlet itr ((x.dat x_in.dat1.schaal)
	   (t.dat x_out.dat1.schaal)
	   (w (weight-vector-of model))
	   (eta 0.05d0))
  (if (null x.dat)
      (progn 
	(set 'w-ml w)
	(itr x_in.dat1.schaal x_out.dat1.schaal w eta))
      (progn
	(let ((w-new (lms-algorithm (car x.dat) (car t.dat) w model
				    :eta eta :regularization-lambda -0.001d0)))
	  (print (m* (m-t w-new) w))
	  (itr (cdr x.dat) (cdr t.dat) w-new eta)))))

(plot-lst-2dim (lambda (x1 x2)
		 (regression-function (vec x1 x2) w-ml model))
	       (seq -1.0d0 1.0d0 :by 0.1d0)
	       (seq -1.0d0 1.0d0 :by 0.1d0))
;	       :output-file "/home/wiz/document/rbf-optimized.png")
  
(defvar environment-state)
(setf environment-state (vec 1.0d0 2.0d0))

(defun environment (action)
  (let* ((next-state (m+ environment-state action
			 (vec (random-normal :sd 0.01) (random-normal :sd 0.01))))
	 (reward (+ (- (euclidean-norm next-state)) 10.0d0)))	  
    (setf environment-state next-state)
    (values next-state reward)))

;;; state dimension and action dimension
(defvar state-dimension)
(defvar action-dimension)


(defvar *collect-info*)
(defvar environment-state)
(defvar critic-session-backup)
(defvar actor-session-backup)

(defvar *action-list*)
(setf *action-list* '(-1 -0.5 -0.3 -0.2 -0.1 0.1 0.2 0.3 0.5 1))
(defvar *action-probability-list*)


;;; モデルを初期化
(set 'model (rbf-model-initializer
	     (mapcar #'list->vector (direct-product (seq -1d0 1d0 :by 0.3d0)
						    (seq -1d0 1d0 :by 0.3d0)
						    (seq -1d0 1d0 :by 0.8d0)
						    (seq -1d0 1d0 :by 0.8d0)
						    ))
	     0.3d0))

(defun softmax-policy (state natural-num-action-index w model action-model &key (tau 0.3d0))
  (let ((total-action-number (apply #'* (array-dimensions (action-table-of action-model)))))
    (/ (exp (/ (regression-function
		(vector-cat2 state
			     (apply #'aref (action-table-of action-model) 
				    (scalar-index->index-list
				     (array-dimensions (action-table-of action-model))
				     natural-num-action-index)))
		w model)
	       tau))
       (summation (ak-index 0 (1- total-action-number)) ; summation by natural number index
	 (exp (/ (regression-function
		  (vector-cat2 state ; natural number -> index list -> refer action-table
			       (apply #'aref (action-table-of action-model) 
				      (scalar-index->index-list
				       (array-dimensions (action-table-of action-model))
				       ak-index)))
		  w model)
		 tau))))))

(defun action-selector (state w model action-model &key (tau 0.3d0))
  (let* ((action-dimensions (array-dimensions (action-table-of action-model)))
	 (total-action-number (apply #'* action-dimensions))
	 (policy
	  (iter (for a-index from 0 to (1- total-action-number))
		(collect (softmax-policy state a-index w model action-model :tau tau))))
	 (selected-action-index (random-from-probability-list policy)))
    (apply #'aref (action-table-of action-model)
	   (scalar-index->index-list action-dimensions selected-action-index))))

(defun action-selector (state w model action-model &key (tau 0.3d0))
  (let* ((action-dimensions (array-dimensions (action-table-of action-model)))
	 (total-action-number (apply #'* action-dimensions))	 
	 (selected-action-index (random 25)))
    (apply #'aref (action-table-of action-model)
	   (scalar-index->index-list action-dimensions selected-action-index))))
  
;;; Reinforcement Learning Algorithm by RBF-networks
;; (1) observe current state x_c(t).
(defun reinforcement-Q-learning (state w model action-model
				 &key (tau 0.3d0) (decay-rate 0.99d0) (eta 0.01d0) (regularization-lambda 0.01d0))
  ;; (2) action sellection by actor network => TODO: stocastic
  (let ((action (action-selector state w model action-model :tau tau)))
    ;; (3) observe reaction of environment
    (multiple-value-bind (next-state reward)
	(environment action)
      ;; (4) training RBF network
      (let* ((next-w
	      (lms-algorithm
	       (vector-cat2 state action)
	       (+ (* decay-rate
		     (regression-function (vector-cat2 state action) w model))
		  reward)
	       w model :eta eta :regularization-lambda regularization-lambda)))
	(format t "~A ~A ~A ~A~%" state action reward (m* (m-t w) next-w))
	(force-output)
	;; (5) recursive call
	(reinforcement-Q-learning next-state next-w model action-model
				  :tau tau :decay-rate decay-rate
				  :eta eta :regularization-lambda regularization-lambda)))))


;;;;;;;;;;;; MODEL SETTING ;;;;;;;;;;;;;;;;

(set 'model (rbf-model-initializer
	     (mapcar #'list->vector (direct-product (seq -2d0 2d0 :by 0.5d0)
						    (seq -2d0 2d0 :by 0.5d0)
						    (seq -2d0 2d0 :by 0.5d0)))
	     0.1d0))


(set 'a-model (make-instance 'action-model
			     :action-table (make-array '(5 5)
						       :initial-contents (mapcar (lambda (x1)
										   (mapcar (lambda (x2) (vec x1 x2)) '(-0.3 -0.1 0 0.1 0.3)))
										 '(-0.3 -0.1 0 0.1 0.3)))))

(setf environment-state (vec 0.0d0 0.0d0))
(action-selector environment-state (weight-vector-of model) model a-model)
(reinforcement-q-learning environment-state (weight-vector-of model) model a-model :eta 0.001d0 :regularization-lambda 0.01d0 :tau 0.1d0)