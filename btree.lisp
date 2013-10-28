;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          btree.lisp
;;;; Purpose:       Binary tree search function
;;;; Programmer:    Kevin M. Rosenberg
;;;; Date Started:  Mar 2010
;;;;
;;;; This file, part of KMRCL, is Copyright (c) 2010 by Kevin M. Rosenberg
;;;;
;;;; KMRCL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************

(in-package #:kmrcl)

(defmacro def-string-tricmp (fn simple)
  "Defines a string tri-valued compare function.
Can choose optimized version for simple-string."
  `(defun ,fn (a b)
     ,(format nil "Compares two ~Astrings. Returns (VALUES CMP MAX-MATCHED). ~
CMP is -1 if a<b, 0 if a=b, +1 if b>a. ~
MAX-MATCHED is maximum numbers of letters of A ~
successfully compared."
              (if simple "simple " ""))
     (declare ,(if simple '(simple-string a b) '(string a b))
              (optimize (speed 3) (safety 0) (debug 0)
                        (compilation-speed 0) (space 0)))
     (let ((alen (length a))
           (blen (length b)))
       (declare (fixnum alen blen))
       (dotimes (i alen)
         (declare (fixnum i))
         (when (>= i blen)
           ;; At this point, A and B have matched, but A has more letters and B does not
           (return-from ,fn (values 1 i)))
         (let ((ac (,(if simple 'schar 'char) a i))
               (bc (,(if simple 'schar 'char) b i)))
           (cond
             ((char-lessp ac bc)
              (return-from ,fn (values -1 i)))
             ((char-greaterp ac bc)
              (return-from ,fn (values 1 i))))))
       ;; At this point, A and B are equal up to the length of A
       (when (= alen blen)
         (return-from ,fn (values 0 alen)))
       ;; B is greater than A length, so A is less
       (values -1 alen))))

(def-string-tricmp string-tricmp nil)
(def-string-tricmp simple-string-tricmp t)

(defun number-tricmp (a b)
  "Compares two numbers. Returns -1 if a<b, 0 if a=b, +1 if b>a."
  (declare (real a b)
           (optimize (speed 3) (space 0) (debug 0) (compilation-speed 0)))
  (cond
    ((< a b) -1)
    ((> a b) 1)
    (t 0)))

(defun complex-number-tricmp (a b)
  "Compares the magnitude of two complex numbers.
Returns -1 if a<b, 0 if a=b, +1 if b>a."
  (declare (number a b)
           (optimize (speed 3) (space 0) (debug 0) (compilation-speed 0)))
  (let ((a-mag2 (+ (* (realpart a) (realpart a)) (* (imagpart a) (imagpart a))))
        (b-mag2 (+ (* (realpart b) (realpart b)) (* (imagpart b) (imagpart b)))))
    (declare (real a-mag2 b-mag2))
    (cond
      ((< a-mag2 b-mag2) -1)
      ((> a-mag2 b-mag2) 1)
      (t 0))))

(defun sorted-vector-find (key-val sorted-vector &key test key trace)
  "Finds index of element in sorted vector using a binary tree search. ~
Order log2(N). Returns (VALUES POS LAST-VALUE LAST-POS COUNT).
POS is NIL if not found."
  (declare (optimize (speed 3) (safety 0) (space 0) (debug 0)
                     (compilation-speed 0)))
  (unless test
    (setq test
          (etypecase key-val
            (simple-string #'simple-string-tricmp)
            (string #'string-tricmp)
            (complex #'complex-number-tricmp)
            (number #'number-tricmp))))
  (when (zerop (length sorted-vector))
    (return-from sorted-vector-find (values nil nil nil 0)))
  (do* ((len (length sorted-vector))
        (last (1- len))
        (pos (floor len 2))
        (last-width 0 width)
        (last2-width last-width last-width)
        (width (1+ (ceiling pos 2)) (ceiling width 2))
        (count 1 (1+ count))
        (cur-raw (aref sorted-vector pos)
                 (aref sorted-vector pos))
        (cur (if key (funcall key cur-raw) cur-raw)
             (if key (funcall key cur-raw) cur-raw))
        (cmp (funcall test key-val cur) (funcall test key-val cur)))
       ((or (zerop cmp) (= 1 last2-width))
        (when trace
          (format trace "~A ~A ~A ~A ~A~%" cur pos width last-width cmp))
        (values (if (zerop cmp) pos nil) cur-raw pos count))
    (declare (fixnum len last pos last-width width count cmp))
    (when trace
      (format trace "~A ~A ~A ~A ~A~%" cur pos width last-width cmp))
    (case cmp
      (-1
       ;; str < cur
       (decf pos width)
       (when (minusp pos) (setq pos 0)))
      (1
       ;; str > cur
       (incf pos width)
       (when (> pos last) (setq pos last))))))
