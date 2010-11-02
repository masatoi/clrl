;;; -*- Coding: utf-8; Mode: Lisp; Syntax: Common Lisp; -*-

;; 状態: 離散値のベクトル
;; 行動: 上下左右の4行動(離散値)

(defclass discrete-pursuit-environment (rl-environment)
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
	 (reward (if (> distance 1d0) 0d0 (/ 1d0 (* (n-of-step-of environment)
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
			:alpha 0.1d0 :gamma 0.9d0 :-lambda 0.8d0))

(nlet itr ()
  (initialize-continuous-pursuit-environment pursuit-env)
  (rl-process (vector-cat (hunter-location-of pursuit-env)
			  (target-location-of pursuit-env))
	      pursuit-env RBF-agent)
  @(car (return-list-of RBF-agent))
  (finish-output)
  (itr))


(defun update-state (state agent-id behavior)
  (labels 
      ((transition (s a)
	 ;; 状態遷移先を返す
	 (if (= a stay)
	     s
	     (let ((next-loc
		    (cond ((= a up)
			   (if (zerop (car s)) 
			       (list (- *size* 1) (cadr s))
			       (list (- (car s) 1) (cadr s))))
			  ((= a down)
			   (if (= (car s) (- *size* 1))
			       (list 0 (cadr s))
			       (list (+ (car s) 1) (cadr s))))
			  ((= a left)
			   (if (zerop (cadr s))
			       (list (car s) (- *size* 1))
			       (list (car s) (- (cadr s) 1))))
			  ((= a right)
			   (if (= (cadr s) (- *size* 1))
			       (list (car s) 0)
			       (list (car s) (+ (cadr s) 1)))))))
	       (if (and			; 遷移先との衝突判定
		    (null (delete-if-not ; (I)遷移先に他のエージェントがいない
			   #'(lambda (x)
			       (equal next-loc x))
			   (mapcar #'(lambda (x) (car x)) state)))
		    (if *obstacle*
			(= 0		; (II)遷移先に障害物がない
			   (aref *obstacle* (car next-loc) (cadr next-loc)))
			t))
		   next-loc		;次状態
		   s)))))		;その場にとどまる
    ;; body of update-state
    (if (zerop behavior)
	state
	(nlet iter ((i 0)
		    (state-list state)
		    (product-list '()))
	      (if (null state-list)
		  (reverse product-list)
		  (if (= i agent-id)
		      (iter (+ i 1) (cdr state-list)
			    (cons (list (transition (caar state-list) behavior)
					(cadar state-list))
				  product-list))
		      (iter (+ i 1) (cdr state-list)
			    (cons (car state-list) product-list))))))))

(defun adjacent? (state)
  ;; すべてのハンターが獲物に隣接しているかを判定する述語
  (let ((prey-loc (car (nth *prey-id* state))))
    (let ((prey-next-loc-list
	   (list  (if (zerop (car prey-loc))
		      (list (- *size* 1) (cadr prey-loc))
		      (list (- (car prey-loc) 1) (cadr prey-loc)))
		  (if (= (car prey-loc) (- *size* 1))
		      (list 0 (cadr prey-loc))
		      (list (+ (car prey-loc) 1) (cadr prey-loc)))
		  (if (zerop (cadr prey-loc))
		      (list (car prey-loc) (- *size* 1))
		      (list (car prey-loc) (- (cadr prey-loc) 1)))
		  (if (= (cadr prey-loc) (- *size* 1))
		      (list (car prey-loc) 0)
		      (list (car prey-loc) (+ (cadr prey-loc) 1)))))
	  (hunter-loc-list
	   (nlet iter ((product '())
		       (state-list state))
		 (if (null state-list)
		     product
		     (if (= (cadar state-list) 0)
			 (iter (cons (caar state-list) product) (cdr state-list))
			 (iter product (cdr state-list)))))))
      (or
       ;;障害物があるなら,全てのハンターに囲まれなくても,獲物が移動不可能になれば終了する.
       (and *obstacle* 
	    (null (remove-if
		   #'(lambda (loc)
		       (or (= 1 (aref *obstacle* (car loc) (cadr loc)))
			   (member loc hunter-loc-list :test 'equal)))
		   prey-next-loc-list)))
       ;; 全てのハンターに取り囲まれる
       (= *n-of-hunter*
	  (length (nintersection prey-next-loc-list
				 hunter-loc-list :test 'equal)))
       ))))

(defmethod reward ((agent <agent>) &optional L)
  0)

;; 方策勾配法のエージェントに与える報酬
(defmethod reward ((agent <PG-agent>) &optional L)
  (if L
      (/ 1 (expt L 2)) ;エピソード終了時
      0)) ;エピソード中
;; Profit Sharingのエージェントに与える報酬
(defmethod reward ((agent <PS-agent>) &optional L)
  (if L
      (/ 1 (expt L 2)) ;エピソード終了時 (/ 1 (expt L 2))
      0))
;; (defmethod reward ((agent <QL-agent>) &optional L)
;;   (if L
;;       1.0 ;エピソード終了時
;;       -0.05)) ;エピソード中

(defmethod reward ((agent <QL-agent>) &optional L)
  (if L
      (/ 1 (expt L 2)) ;エピソード終了時
      0)) ;エピソード中

;; 通常の終了条件
(defmethod environment (state action L (agent <agent>))
  (let ((next-state (update-state state (id-of agent) action)))
    (if (adjacent? next-state) ; 隣接状態(終了状態)なら
	(values next-state (reward agent L)) ;エピソード長を与える
	(values next-state (reward agent)))))
