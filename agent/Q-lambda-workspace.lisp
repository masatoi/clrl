;;; -*- Coding: utf-8; Mode: Lisp; Syntax: Common Lisp; -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; experiment;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; モデルを初期化
(set 'model (rbf-model-initializer
	     ;; mean-list
	     (mapcar #'list->vector (let ((dip (direct-product (seq -2d0 2d0 :by 0.5d0)
							       (seq -2d0 2d0 :by 0.5d0))))
				      (mapcar (lambda (dip-elem)
						(list (+ (car dip-elem) 0) ; (- (random 1d0) 0.5d0)
						      (+ (cadr dip-elem) 0))) ; (- (random 1d0) 0.5d0)
					      dip)))
	     ;; variance
	     0.5d0))

;; (direct-product (seq -2d0 2d0 :by 0.5d0)
;; 		(seq -2d0 2d0 :by 0.5d0))

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
	   (eta 0.01d0))
  (if (null x.dat)
      (progn
	(plot-lst-2dim
	 (lambda (x1 x2)
	   (regression-function (vec x1 x2) w model))
	 (seq -1.0d0 1.0d0 :by 0.1d0)
	 (seq -1.0d0 1.0d0 :by 0.1d0) :surface nil)
	(set 'w-ml w)
	(itr x_in.dat1.schaal x_out.dat1.schaal w eta))
      (progn
	;; (plot-lst-2dim
	;;  (lambda (x1 x2)
	;;    (regression-function (vec x1 x2) w model))
	;;  (seq -1.0d0 1.0d0 :by 0.1d0)
	;;  (seq -1.0d0 1.0d0 :by 0.1d0))
	(let ((w-new (LMS-algorithm (car x.dat) (car t.dat) w model
				    :eta eta :regularization-lambda 0.01d0)))
	  ;(print (m* (m-t w-new) w))
	  (itr (cdr x.dat) (cdr t.dat) w-new eta)))))

(plot-lst-2dim (lambda (x1 x2)
		 (regression-function (vec x1 x2) w-ml model))
	       (seq -1.0d0 1.0d0 :by 0.05d0)
	       (seq -1.0d0 1.0d0 :by 0.05d0))
;	       :output-file "/home/wiz/document/rbf-optimized.png")