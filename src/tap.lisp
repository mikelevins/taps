;;;; ***********************************************************************
;;;;
;;;; Name:          tap.lisp
;;;; Project:       taps
;;;; Purpose:       creating series
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:taps)

(defgeneric tap (element-type source &key &allow-other-keys))

(defun tap-integers (&key (from 0)(by 1) upto below downto above)
  (scan-range :type 'integer
              :from from
              :by by
              :upto upto
              :below below
              :downto downto
              :above above))

(defun tap-random-integers (below &optional (random-state *random-state*))
  (let ((iota (scan-range :from 0 :by 1)))
    (map-fn t (lambda (i)(random below random-state))
            iota)))

(defun tap-fn (fn series)
  (map-fn t fn series))

