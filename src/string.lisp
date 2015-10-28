;;;; ***********************************************************************
;;;;
;;;; Name:          string.lisp
;;;; Project:       taps
;;;; Purpose:       tapping strings
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:taps)

(defmethod tap ((type (eql :characters))(source string) &key &allow-other-keys)
  (scan source))

(defmethod tap ((type (eql :words))(source string)
                &key (word-break-characters +whitespace-characters+)
                  &allow-other-keys)
  (with-input-from-string (in source)
    (tap :words in)))

(defmethod tap ((type (eql :lines))(source string)
                &key (line-break-characters +line-break-characters+)
                  &allow-other-keys)
  (with-input-from-string (in source)
    (tap :lines in)))


(defmethod tap ((type (eql :objects))(source string)
                &key &allow-other-keys)
  (with-input-from-string (in source)
    (tap :objects in)))

;;; (tap :characters "This is a string to use for testing")
;;; (tap :words "This is a string to use for testing")
;;; (tap :lines (format nil "~%This is a string ~%to use for testing"))
;;; (tap :objects "0 1.0 \"Two\" :three (4 5)")
