;;;; -*- Mode:CommonLisp; tab-width:2; indent-tabs-mode:nil; 

;;;; Keep multiple init files in $HOME/.ccl-init/*-init.lisp

(in-package :cl-user)

#-ccl
(warn "Initialization script for CCL application not running in CCL")

;; For personal conditionalization
(eval-when (:compile-toplevel :load-toplevel :execute)
  (pushnew ':kmcorbett *features*))

;; Globals
(setf *compile-verbose* t *load-verbose* t)

(defun ccl-init (&rest scripts)
  "loads one or more init scripts"
  (flet ((ccl-init-load (filename)
           (load (merge-pathnames
                  (make-pathname :name (format nil "~(~A~)-init" filename)
                                 :directory '(:relative ".ccl-init")
                                 :type "lisp")
                  (user-homedir-pathname)))))
    (loop for script in scripts
       do (ccl-init-load script))))

(ccl-init "quicklisp")
