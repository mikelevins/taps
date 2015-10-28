;;;; ***********************************************************************
;;;;
;;;; Name:          take.lisp
;;;; Project:       taps
;;;; Purpose:       collecting subsets of taps
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:taps)

(defun filter (fn series)
  (let ((flags (tap-fn fn series)))
    (choose flags series)))

(defmethod take ((n integer)(series series::foundation-series))
  (subseries series 0 n))

(defun take-until (fn series)
  (let ((flags (tap-fn fn series)))
    (until flags series)))

(defmethod take-by ((n integer)(series series::foundation-series))
  (let* ((starts (scan-range :by n)))
    (map-fn t 
            (lambda (x)(subseries series x (+ x n)))
            starts)))

(defmethod take-m-by-n ((m integer)(n integer)(series series::foundation-series))
  (let* ((starts (scan-range :by n)))
    (map-fn t 
            (lambda (x)(subseries series x (+ x m)))
            starts)))

