;;;; ***********************************************************************
;;;;
;;;; Name:          any.lisp
;;;; Project:       taps
;;;; Purpose:       collecting arbitrary elements
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:taps)

;;; WARNING: any works only on finite sequences!
(defmethod any ((series series::foundation-series))
  (let ((len (collect-length series)))
    (if (< len 1)
        nil
        (collect-nth (random len)
                     series))))

(defmethod any ((sequence sequence))
  (let ((len (length sequence)))
    (if (< len 1)
        nil
        (elt sequence
             (random len)))))
