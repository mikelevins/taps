;;;; ***********************************************************************
;;;;
;;;; Name:          contains.lisp
;;;; Project:       taps
;;;; Purpose:       searching for elements
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:taps)

;;; WARNING: contains? works only on finite series
(defmethod contains? ((series series::foundation-series) val &key (test #'eql))
  (block searching
    (iterate ((it series))
             (when (funcall test val it)
               (return-from searching t)))
    nil))

(defmethod contains? ((sequence sequence) val &key (test #'eql))
  (find val sequence :test test))

