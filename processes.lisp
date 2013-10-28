;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          processes.lisp
;;;; Purpose:       Multiprocessing functions
;;;; Programmer:    Kevin M. Rosenberg
;;;; Date Started:  June 2003
;;;; *************************************************************************

(in-package #:kmrcl)


(defun make-process (name func)
  #+allegro (mp:process-run-function name func)
  #+cmu (mp:make-process func :name name)
  #+lispworks (mp:process-run-function name nil func)
  #+sb-thread (sb-thread:make-thread func :name name)
  #+ccl (ccl:process-run-function name func)
  #-(or allegro cmu lispworks sb-thread ccl) (funcall func)
  )

(defun destroy-process (process)
  #+cmu (mp:destroy-process process)
  #+allegro (mp:process-kill process)
  #+sb-thread (sb-thread:destroy-thread process)
  #+lispworks (mp:process-kill process)
  #+ccl (ccl:process-kill process)
  )

(defun make-lock (name)
  "Make a named process lock."
  #+abcl (ext:make-thread-lock)
  #+allegro (mp:make-process-lock :name name)
  #+ccl (ccl:make-lock name)
  #+cmu (mp:make-lock name)
  #+lispworks (mp:make-lock :name name)
  #+sb-thread (sb-thread:make-mutex :name name)
  #-(or lispworks abcl openmcl allegro sb-thread)
  (declare (ignore name))
  #-(or abcl allegro ccl cmu lispworks sb-thread)
  nil)


(defmacro with-lock-held ((lock) &body body)
  #+abcl
  `(ext:with-thread-lock (,lock) ,@body)
  #+allegro
  `(mp:with-process-lock (,lock) ,@body)
  #+ccl
  `(ccl:with-lock-grabbed (,lock) ,@body)
  #+cmu
  `(mp:with-lock-held (,lock) ,@body)
  #+lispworks
  `(mp:with-lock (,lock) ,@body)
  #+sb-thread
  `(sb-thread:with-recursive-lock (,lock) ,@body)
  #-(or abcl allegro ccl cmu lispworks sb-thread)
  `(progn ,@body)
  )


(defmacro with-timeout ((seconds) &body body)
  #+allegro
  `(mp:with-timeout (,seconds) ,@body)
  #+cmu
  `(mp:with-timeout (,seconds) ,@body)
  #+sb-thread
  `(sb-ext:with-timeout ,seconds ,@body)
  #+ccl
  `(ccl:process-wait-with-timeout "waiting"
                                 (* ,seconds ccl:*ticks-per-second*)
                                 #'(lambda ()
                                     ,@body) nil)
  #-(or allegro cmu sb-thread ccl)
  `(progn ,@body)
  )

(defun process-sleep (n)
  "Put thread to sleep for n seconds."
  #+allegro (mp:process-sleep n)
  #-allegro (sleep n))



