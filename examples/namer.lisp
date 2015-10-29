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

(defmethod read-lines ((path pathname))
  (tap :lines path))

;;; (time (read-lines (pathname "dickens.names")))

(defmethod long-enough? ((thing sequence))
  (> (length thing) 1))

(defmethod read-names ((path pathname))
  (filter #'long-enough? (read-lines path)))

;;; (time (read-names (pathname "dickens.names")))

(defmethod triples ((txt string))
  (let* ((chunks (take-m-by-n 3 1 (scan txt)))
         (strings (map-fn t (lambda (chunk)(collect 'string chunk)) chunks)))
    (take-until (lambda (s)(equal "" s))
                strings)))

;;; (time (triples "Barney"))

(defmethod parse-names ((path pathname))
  (let* ((triples (tap-fn #'triples (read-names path)))
         (starts (tap-fn #'(lambda (it)(take 1 it)) triples))
         (ends (tap-fn #'(lambda (it)(drop 1 it)) triples)))
    (values triples starts ends)))

;;; (time (parse-names (pathname "dickens.names")))

