;;;; ***********************************************************************
;;;;
;;;; Name:          take.lisp
;;;; Project:       taps
;;;; Purpose:       collecting subsets of taps
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:taps)

(defmethod take ((n integer)(series series::foundation-series))
  (subseries series 0 n))

(defmethod take ((n integer)(sequence sequence))
  (subseq sequence 0 n))

(defmethod take-until (fn (series series::foundation-series))
  (let ((flags (tap-fn fn series)))
    (until flags series)))

(defmethod take-until (fn (sequence sequence))
  (let ((end (position-if fn sequence)))
    (if end
        (subseq sequence 0 end)
        sequence)))

(defmethod take-by ((n integer)(series series::foundation-series))
  (let* ((starts (scan-range :by n)))
    (map-fn t 
            (lambda (x)(subseries series x (+ x n)))
            starts)))

(defmethod take-by ((n integer)(sequence sequence))
  (let ((len (length sequence)))
    (loop
       for start from 0 by n
       for end = (+ start n) then (+ start n)
       until (>= start len)
       collect (if (>= end len)
                   (subseq sequence start)
                   (subseq sequence start end)))))

(defmethod take-m-by-n ((m integer)(n integer)(series series::foundation-series))
  (let* ((starts (scan-range :by n)))
    (map-fn t 
            (lambda (x)(subseries series x (+ x m)))
            starts)))

(defmethod take-m-by-n ((m integer)(n integer)(sequence sequence))
  (let ((len (length sequence)))
    (loop
       for start from 0 by n
       for end = (+ start m) then (+ start m)
       until (>= start len)
       collect (if (>= end len)
                   (subseq sequence start)
                   (subseq sequence start end)))))

