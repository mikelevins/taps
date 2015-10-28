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

(defmethod drop ((n integer)(series series::foundation-series))
  (subseries series n))

