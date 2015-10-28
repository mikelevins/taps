;;;; ***********************************************************************
;;;;
;;;; Name:          hash.lisp
;;;; Project:       taps
;;;; Purpose:       tapping strings
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:taps)

(defmethod tap ((type (eql :hash-entries))(source hash-table) &key &allow-other-keys)
  (scan-hash source))

(defmethod tap ((type (eql :keys))(source hash-table) &key &allow-other-keys)
  (multiple-value-bind (keys vals)(scan-hash source)
    keys))

(defmethod tap ((type (eql :values))(source hash-table) &key &allow-other-keys)
  (multiple-value-bind (keys vals)(scan-hash source)
    vals))

;;; (defparameter $tbl (make-hash-table))
;;; (setf (gethash :fred $tbl) '(:name "Fred" :color :orange :age 35))
;;; (setf (gethash :wilma $tbl) '(:name "Wilma" :color :white :age 35))
;;; (setf (gethash :barney $tbl) '(:name "Barney" :color :brown :age 34))
;;; (setf (gethash :betty $tbl) '(:name "Betty" :color :blue :age 34))
;;; (tap :hash-entries $tbl)
;;; (tap :keys $tbl)
;;; (tap :values $tbl)
