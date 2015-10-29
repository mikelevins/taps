;;;; ***********************************************************************
;;;;
;;;; Name:          filter.lisp
;;;; Project:       taps
;;;; Purpose:       collecting elements by predicate
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:taps)

(defmethod filter (fn (series series::foundation-series))
  (let ((flags (tap-fn fn series)))
    (choose flags series)))

(defmethod filter (fn (sequence sequence))
  (remove-if-not fn sequence))
