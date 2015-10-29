;;;; ***********************************************************************
;;;;
;;;; Name:          namer.lisp
;;;; Project:       taps
;;;; Purpose:       a name generator using taps
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:taps)

(defparameter *name-starts* nil)
(defparameter *name-parts* nil)
(defparameter *name-ends* nil)

(defun long-enough? (seq)
  (> (length seq) 1))

(defun read-names (filename)
  (filter #'long-enough? (tap :lines (pathname filename))))

;;; (time (read-names "dickens.names"))

(defun triples (str)
  (take-until (lambda (it)(< (length it) 3))
                       (take-m-by-n 3 1 str)))

;;; (triples "Barnaby")

(defun parse-names (filename)
  (let* ((names (tap-fn #'triples (read-names filename)))
         (starts (tap-fn (lambda (name)(take 1 name))
                         names))
         (parts (tap-fn (lambda (name)(drop 1 name))
                        names)))
    (values starts parts)))

;;; (parse-names "dickens.names")

(defun init-rules (rule-file)
  (setf (values *name-starts*
                *name-parts*)
        (parse-names rule-file)))

;;; (init-rules "dickens.names")

(defun generate-name ()
  (extend-name (any-name-start)))
