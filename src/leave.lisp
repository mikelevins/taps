;;;; ***********************************************************************
;;;;
;;;; Name:          drop.lisp
;;;; Project:       taps
;;;; Purpose:       removing finite prefixes from series
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:taps)

;;; WARNING: leave works only with finite seres!
(defmethod leave ((n integer)(series series::foundation-series))
  (let ((len (collect-length series)))
    (subseries series (- len n))))

(defmethod leave ((n integer)(sequence sequence))
  (let ((len (length sequence)))
    (subseq sequence (- len n))))

