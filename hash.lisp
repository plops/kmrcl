;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          hash.lisp
;;;; Purpose:       Hash functions for KMRCL package
;;;; Programmer:    Kevin M. Rosenberg
;;;; Date Started:  Apr 2000
;;;;
;;;; This file, part of KMRCL, is Copyright (c) 2002-2011 by Kevin M. Rosenberg
;;;;
;;;; KMRCL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************


(in-package #:kmrcl)

;;; hashs

(defun print-hash (h &key (stream *standard-output*)
                   key-transform-fn value-transform-fn
                   (prefix "") (divider " -> ") (terminator "~%"))
  (maphash #'(lambda (k v)
               (format stream "~A~S~A~S"
                       prefix
                       (if key-transform-fn
                           (funcall key-transform-fn k)
                           k)
                       divider
                       (if value-transform-fn
                           (funcall value-transform-fn v)
                           v))
               (when terminator (format stream terminator)))
           h)
  h)

