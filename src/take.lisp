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

(defun filter (fn series)
  (let ((flags (tap-fn fn series)))
    (choose flags series)))

(defmethod take ((n integer)(series series::foundation-series))
  (subseries series 0 n))

(defmethod take ((n integer)(seq sequence))
  (subseq seq 0 n))

(defmethod take-until (fn (series series::foundation-series))
  (let ((flags (tap-fn fn series)))
    (until flags series)))

(defmethod take-until (fn (seq vector))
  (let* ((series (scan seq))
         (flags (tap-fn fn series)))
    (collect 'vector (until flags series))))

(defmethod take-until (fn (seq list))
  (let* ((series (scan seq))
         (flags (tap-fn fn series)))
    (collect 'list (until flags series))))

(defmethod take-by ((n integer)(series series::foundation-series))
  (let* ((starts (scan-range :by n)))
    (map-fn t 
            (lambda (x)(subseries series x (+ x n)))
            starts)))

(defmethod take-by ((n integer)(sequence cl:sequence))
  (let* ((len (length sequence))
         (start-series (tap-integers :by n)))
    (take-until (lambda (it)(not it))
                (map-fn t (lambda (start)
                            (let ((end (+ start n)))
                              (cond ((>= start len) nil)
                                    ((>= end len)(subseq sequence start))
                                    (t (subseq sequence start end)))))
                        start-series))))

(defmethod take-m-by-n ((m integer)(n integer)(series series::foundation-series))
  (let* ((starts (scan-range :by n)))
    (map-fn t 
            (lambda (x)(subseries series x (+ x m)))
            starts)))

(defmethod take-m-by-n ((m integer)(n integer)(sequence sequence))
  (let* ((len (length sequence))
         (start-series (tap-integers :by n)))
    (take-until (lambda (it)(not it))
                (map-fn t (lambda (start)
                            (let ((end (+ start m)))
                              (cond ((>= start len) nil)
                                    ((>= end len)(subseq sequence start))
                                    (t (subseq sequence start end)))))
                        start-series))))


