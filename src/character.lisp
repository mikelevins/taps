;;;; ***********************************************************************
;;;;
;;;; Name:          character.lisp
;;;; Project:       taps
;;;; Purpose:       commonly-used character sets
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:taps)

(defparameter +whitespace-characters+
  '(#\space #\tab #\return #\newline #\vt #\formfeed))

(defparameter +line-break-characters+
  '(#\return #\newline #\formfeed))
