;;;; ***********************************************************************
;;;;
;;;; Name:          file.lisp
;;;; Project:       taps
;;;; Purpose:       tapping files
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:taps)

(defmethod tap ((type (eql :bytes))(source pathname) &key &allow-other-keys)
  (with-open-file (in source :direction :input
                      :element-type '(unsigned-byte 8))
    (tap :bytes in)))

(defmethod tap ((type (eql :characters))(source pathname) &key &allow-other-keys)
  (with-open-file (in source :direction :input
                      :element-type 'character)
    (tap :characters in)))

(defmethod tap ((type (eql :words))(source pathname)
                &key (word-break-characters +whitespace-characters+)
                  &allow-other-keys)
  (with-open-file (in source :direction :input
                      :element-type 'character)
    (tap :words in)))

(defmethod tap ((type (eql :lines))(source pathname)
                &key (line-break-characters +line-break-characters+)
                  &allow-other-keys)
  (with-open-file (in source :direction :input
                      :element-type 'character)
    (tap :lines in)))

(defmethod tap ((type (eql :objects))(source pathname)
                &key &allow-other-keys)
  (with-open-file (in source :direction :input
                      :element-type 'character)
    (tap :objects in)))

;;; (tap :bytes (pathname "/Users/mikel/.emacs"))
;;; (tap :characters (pathname "/Users/mikel/.emacs"))
;;; (tap :words (pathname "/Users/mikel/.emacs"))
;;; (tap :lines (pathname "/Users/mikel/.emacs"))
;;; (tap :objects (pathname "/Users/mikel/.emacs"))
