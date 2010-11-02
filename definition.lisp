;;; -*- Coding: utf-8; Mode: Lisp; Syntax: Common Lisp; -*-

(defclass rl-agent ()
  ((return-accumulator :accessor return-accumulator-of :initform 0)
   (return-list :accessor return-list-of :initform ())))

(defclass rl-environment () ())

;;; for agent object
(defgeneric rl-process (init-state environment agent))
(defgeneric action-selector (agent s))
(defgeneric learning (agent params))

;;; for environment object
;;  return: s_{t+1}, r_t
(defgeneric respond (environment a))

(defconstant TERMINAL-STATE -1)