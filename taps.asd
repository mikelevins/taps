;;;; ***********************************************************************
;;;;
;;;; Name:          taps.asd
;;;; Project:       taps
;;;; Purpose:       the system definitions for the taps system
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:cl-user)

(asdf:defsystem #:taps
  :description "A library of conveniences for using series"
  :author "mikel evins <mevins@me.com>"
  :license "Apache 2.0"
  :serial t
  :depends-on (:series :split-sequence)
  :components ((:module "src"
                :serial t
                :components ((:file "package")
                             (:file "drop")
                             (:file "take")
                             (:file "tap")
                             (:file "character")
                             (:file "stream")
                             (:file "string")
                             (:file "hash")))))

;;; (asdf:load-system :taps)
