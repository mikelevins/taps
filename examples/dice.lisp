;;;; ***********************************************************************
;;;;
;;;; Name:          dice.lisp
;;;; Project:       taps
;;;; Purpose:       how to use taps to roll dice
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:taps)

(defparameter *rolled-dice*
  (tap-random-integers 6))

(defun roll-the-dice ()
  (let ((dice (map-fn t '1+ (take 2 *rolled-dice*))))
    (setf *rolled-dice*
          (drop 2 *rolled-dice*))
    dice))

;;; (roll-the-dice)

