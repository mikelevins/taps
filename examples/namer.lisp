;;;; ***********************************************************************
;;;;
;;;; Name:          namer.lisp
;;;; Project:       taps
;;;; Purpose:       a name generator using taps
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:taps)

(defparameter *name-starts* nil)
(defparameter *name-parts* nil)
(defparameter *name-ends* nil)

(defun long-enough? (seq)
  (> (length seq) 2))

(defun triples (str)
  (take-until (lambda (it)(< (length it) 3))
              (take-m-by-n 3 1 str)))

(defun read-names (filename)
  (map-fn t #'triples
          (filter #'long-enough?
                  (tap :lines (pathname filename)))))

;;; (time (read-names "dickens.names"))

(defun name-starts (name-lists)
  (map-fn t #'first name-lists))

;;; (name-starts (read-names "dickens.names"))

(defun remove-nil (series)
  (choose series series))

(defun name-parts (name-lists)
  (scan
   (collect-append
    (remove-nil
     (map-fn t #'rest
             name-lists)))))

;;; (name-parts (read-names "dickens.names"))

(defun name-ends (name-lists)
  (map-fn t #'(lambda (nm)(first (last nm))) name-lists))

;;; (name-ends (read-names "dickens.names"))

(defun init-rules (sample-file)
  (let ((samples (read-names sample-file)))
    (setf *name-starts* (name-starts samples))
    (setf *name-parts* (name-parts samples))
    (setf *name-ends* (name-ends samples))
    nil))

;;; (init-rules "dickens.names")

(defun mergeable? (left right)
  (equalp (leave 2 left)
          (take 2 right)))

(defmethod merge-parts ((left string)(right string))
  (concatenate 'string
               left
               (drop 2 right)))

(defun any-start ()
  (any *name-starts*))

;;; (time (any-start))

(defun find-extension (start)
  (any (filter (lambda (part)(mergeable? start part))
               *name-parts*)))

;;; (init-rules "dickens.names")
;;; (defparameter $start (any-start))
;;; (find-extension $start)

(defun extend-name (start)
  (let ((next (find-extension start)))
    (if next
        (if (contains? *name-ends* next :test #'equalp)
            (merge-parts start next)
            (extend-name (merge-parts start next)))
        start)))

(defun generate-name (samples-file)
  (init-rules samples-file)
  (extend-name (any-start)))

;;; (generate-name "dickens.names")

(defun generate-names (n samples-file)
  (init-rules samples-file)
  (loop for i from 0 below n
     collect (extend-name (any-start))))

;;; (generate-names 10 "dickens.names")

