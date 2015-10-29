;;;; ***********************************************************************
;;;;
;;;; Name:          tails.lisp
;;;; Project:       taps
;;;; Purpose:       generating the tails of series and sequences
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:taps)

(defmethod tails ((sequence sequence))
  (let ((len (length sequence)))
    (loop for i from 0 below len
         collect (subseq sequence i))))

(defmethod tails ((series series::foundation-series))
  (let ((indexes (take-until #'null (positions series))))
    (map-fn t (lambda (i)(subseries series i))
            indexes)))

(defmethod tails-by ((n integer)(sequence sequence))
  (let ((len (length sequence)))
    (loop for i from 0 below len by n
         collect (subseq sequence i))))

(defmethod tails-by ((n integer)(series series::foundation-series))
  (let ((indexes (tap-integers :by n)))
    (map-fn t (lambda (i)(subseries series i))
            indexes)))
