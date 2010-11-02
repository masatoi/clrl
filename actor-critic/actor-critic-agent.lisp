;;; -*- Coding: utf-8; Mode: Lisp; Syntax: Common Lisp; -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;           ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;; TSUKUYOMI ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;           ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; load NGnet
(load "/home/wiz/research/NGnet/oibem/online-ib-em.lisp")

;;; load numerical calculation
(load "/home/wiz/program/cl/numerical-calculation.lisp")

;;; state dimension and action dimension
(defvar state-dimension)
(defvar action-dimension)
;;; Actor
(defvar actor-session)
;;; Critic
(defvar critic-session)
(defvar *collect-info*)
(defvar environment-state)
(defvar critic-session-backup)
(defvar actor-session-backup)

;;; Reinforcement Learning Algorithm by NGnet
;; (1) observe current state x_c(t).
(defun reinforcement-learning-ngnet
    (state actor-session critic-session &key (decay-rate 0.99d0) (learning-rate 0.1d0))
  ;; (DEBUG) backup session
  (setf actor-session-backup actor-session
	critic-session-backup critic-session)
  ;; (2) action sellection by actor network => TODO: stocastic
  (let ((action (action-selector state actor-session)))
    ;; (3) observe reaction of environment
    (multiple-value-bind (next-state reward)
	(environment action)
      ;; (4) training Critic network
      (let* ((next-critic-session
;;	      'hoge)
	      (ibem-iterator
	       (vector-cat2 state action) ; input of critic
	       (+ (* decay-rate           ; output of critic
		     (predict-sampling (vector-cat2 next-state
					      (action-selector next-state actor-session))
				 critic-session))
		  reward)
	       critic-session))
	     ;; (5) calculate numerical differentiation of critic-network
	     (critic-differentiation
	      (numerical-differentiation (lambda (u) (pseud-critic state u)) action)
	      ;; (if (arrayp action)
;; 		  (numerical-differentiation-vec
;; 		   (lambda (u) (prediction (vector-cat2 state u) next-critic-session))
;; 		   action)
;; 		  (numerical-differentiation
;; 		   (lambda (u) (prediction (vector-cat2 state u) next-critic-session))
;; 		   action))
	       )
	     ;; (6) traning Actor network
	     (next-actor-session
	      (ibem-iterator state
			     (+ action @(* learning-rate critic-differentiation))
			     actor-session)))
	@state
	;@action
	;@critic-differentiation
	;; (format t "Q(x(t),u(t)): ~A, Q(x(t),u_target(t)): ~A~%"
;; 		(prediction (vector-cat2 state action) next-critic-session)
;; 		(prediction (vector-cat2 state (+ action (* learning-rate critic-differentiation))) next-critic-session))
	;; (DEBUG) recording
	(push (list state action reward critic-differentiation) *collect-info*)
;; 	;; (DEBUG) plot Actor Networks joint-distribution
;; 	(plot-lst-2dim
;; 	 (lambda (state action)
;; 	   (ssum (apply #'joint-probability state action (session-params actor-session))))
;; 	 (seq (- 10.0) 10.0 :by 1)  (seq (- 10.0) 10.0 :by 1) :surface nil)

;; 	(plot-lst (mapcar (lambda (x) (prediction x actor-session)) (seq -10 10 :by 0.1)))

	;; (7) recursive call
	(reinforcement-learning-ngnet next-state
				      next-actor-session
				      next-critic-session)))))

;; ;;; epsilon-greedy-selection
;; (defun action-selector (state actor-session &key (random-select-probability 0.1))
;;   (if (> random-select-probability (random 1.0))
;;       ;; exploration (use random selection)
;;       (- (random 2.0d0) 1.0d0)
;;       ;; exploitation (use outputs of Actor-Network)
;;       (prediction state actor-session)))

;; (defun action-selector (state actor-session)
;;   (random-normal :mean (prediction state actor-session) :sd 1)
;;   )

;; (defun action-selector (state actor-session)
;;   (+ (if (> state 0) -0.1d0 0.1d0)
;;      (- (random 1.0d0) 0.5d0)))

;;; use Actor-Network action-sellection
(defun action-selector (state actor-session)
  (predict-sampling state actor-session))
      
;;; Example1: an agent moves 1-dimension line, and if agent attain origin then receive reward. both of input and output dimension is 1.
;; 1-dimension state space, 1-dimension action space

;; (defun environment (action)
;;   (let* ((next-state (+ environment-state action))
;; 	 (reward (+ (- (/ (square next-state) 100d0)) 1.0d0)))
;;     (setf environment-state next-state)
;;     (values next-state reward)))

(defun environment (action)
  (let* ((tmp-state (+ environment-state action))
	 (next-state
	  ;(if (> (abs tmp-state) 10d0) environment-state tmp-state))
	  tmp-state)
	 (reward (+ (- (/ (square next-state) 100d0)) 1.0d0)))
    (setf environment-state next-state)
    (values next-state reward)))


;; setting of dimensions
(setf state-dimension 1
      action-dimension 1)
;; make Actor & Critic Networks
(setf actor-session (session-initializer 10 state-dimension action-dimension :gamma 1.0d0 :lambda-factor 0.99d0)
      critic-session (session-initializer 10 (+ state-dimension action-dimension) 1 :gamma 1.0d0 :lambda-factor 0.99d0))

(setf actor-session actor-session-backup
      critic-session (session-initializer 10 (+ state-dimension action-dimension) 1 :gamma 1.0d0 :lambda-factor 0.99d0))

;; initialize
(setf environment-state -10d0)
(setf *collect-info* nil)

;; RUN
(reinforcement-learning-ngnet environment-state actor-session critic-session
			      :decay-rate 0.9d0 :learning-rate 0.1d0)

;;; Actor-Networkの入出力の同時分布
(plot-lst-2dim
 (lambda (state action)
   (ssum (apply #'joint-probability state action (session-params actor-session-backup))))
 (seq -2.0 2.0 :by 0.1)  (seq -2.0 2.0 :by 0.1) :surface nil)

;;; Actor-Networkの出力の予測
(plot-lst (mapcar (lambda (x) (prediction x actor-session-backup)) (seq -10 10 :by 0.1)))

;;; Critic-Networkの出力の予測
(plot-lst-2dim
 (lambda (state action)
   (prediction (list->vector (list state action)) critic-session-backup))
 (seq -10.0 10.0 :by 0.2)  (seq -10.0 10.0 :by 0.2))

;; ;;; Example2: an agent moves 2-dimension plane, and if agent attain origin then receive reward.
;; ;; 2-dimension state space, 2-dimension action space
;; (defun environment (action)
;;   (let* ((next-state (m+ environment-state action))
;; 	 (reward (+ (- (euclidean-norm next-state)) 1.0d0)))
;;     (setf environment-state next-state)
;;     (values next-state reward)))

;; ;; setting of dimensions
;; (setf state-dimension 2
;;       action-dimension 2)
;; ;; make Actor & Critic Networks
;; (setf actor-session (session-initializer 10 state-dimension action-dimension)
;;       critic-session (session-initializer 10 (+ state-dimension action-dimension) 1))

;; ;; initialize
;; (setf environment-state (list->vector '(10.0d0 10.0d0)))
;; (setf *collect-info* nil)

;; ;; RUN
;; (reinforcement-learning-ngnet environment-state actor-session critic-session)

(nlet iter ((session session))
  (let* ((x_in (- (random (* 4 pi)) (* 2 pi)))
	 (s (ibem-iterator x_in (cos (* 2 x_in)) session)))
    (plot-lst (mapcar (lambda (x) (prediction x session)) (seq (* -5 pi) (* 5 pi) :by 0.1)))
    (iter s)))

(plot-lst (mapcar (lambda (x) (cos (* 2 x))) (seq (* -2 pi) (* 2 pi) :by 0.1)))


(defun pseud-critic (state action)
  (universal-gaussian (list->vector (list state action))
		      (list->vector '(0d0 0d0))
		      (make-array '(2 2) :initial-contents
				  '((0.5d0 0.8d0) (0d0 0.5d0)))))
