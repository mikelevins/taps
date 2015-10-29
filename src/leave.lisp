;;;; ***********************************************************************
;;;;
;;;; Name:          leave.lisp
;;;; Project:       taps
;;;; Purpose:       taking finite suffixes from sequences
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:taps)

(defmethod leave ((n integer)(sequence sequence))
  (let ((len (length sequence)))
    (subseq sequence (- len n))))


