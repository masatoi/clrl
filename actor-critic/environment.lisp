;;; -*- coding: utf-8 mode: lisp -*-
(require :mcclim)

(in-package :clim-user)

(defun draw (frame stream)
  (declare (ignore frame))
  (loop for i from 1 to 100 by (random 10)
        do   (draw-circle* stream (+ 100 i) (+ 100 i i) 30
                  :filled nil
                  :ink (make-rgb-color
                        (random 1.0) (random 1.0) (random 1.0)))))

(define-application-frame nn-frame ()
  ()
  (:menu-bar t)
  (:panes
   (canvas :application
           :min-width 500
           :min-height 500
           :scroll-bars nil
           :display-time :command-loop  ; command の度に描画する
           :display-function 'draw))
  (:layouts
   (defalut (horizontally () canvas))))

(define-nn-frame-command (com-quit :menu t) ()
  (frame-exit *application-frame*))

(define-nn-frame-command (com-redraw :menu t) ()
  ;; :display-time :command-loop なので何もする必要なし
  )


(defun run ()
  (run-frame-top-level
   (make-application-frame 'nn-frame)))

(run)
