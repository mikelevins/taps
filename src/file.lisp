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
  )

(defmethod tap ((type (eql :characters))(source pathname) &key &allow-other-keys)
  )

(defmethod tap ((type (eql :words))(source pathname)
                &key (word-break-characters +whitespace-characters+)
                  &allow-other-keys)
  )

(defmethod tap ((type (eql :lines))(source pathname)
                &key (line-break-characters +line-break-characters+)
                  &allow-other-keys)
  )

(defmethod tap ((type (eql :objects))(source pathname)
                &key &allow-other-keys)
  )

