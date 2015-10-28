;;;; ***********************************************************************
;;;;
;;;; Name:          stream.lisp
;;;; Project:       taps
;;;; Purpose:       tapping streams
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:taps)

(defmethod tap ((type (eql :bytes))(source stream) &key &allow-other-keys)
  (scan-stream source #'read-byte))

(defmethod tap ((type (eql :characters))(source stream) &key &allow-other-keys)
  (scan-stream source #'read-char))

(defmethod tap ((type (eql :words))(source stream)
                &key (word-break-characters +whitespace-characters+)
                  &allow-other-keys)
  (let* ((chars (scan-stream source #'read-char))
         (break-flags (map-fn t (lambda (ch)
                                  (member ch word-break-characters))
                              chars))
         (break-positions (positions break-flags))
         (break-positions0 (catenate (scan '(-1))
                                     break-positions))
         (break-positions1 (catenate break-positions
                                     (scan '(nil))))
         (chunks (map-fn t (lambda (x y)
                             (if y
                                 (subseries chars (1+ x) y)
                                 (subseries chars (1+ x))))
                         break-positions0
                         break-positions1)))
    (map-fn t (lambda (s)(collect 'string s))
            chunks)))

(defmethod tap ((type (eql :lines))(source stream)
                &key (line-break-characters +line-break-characters+)
                  &allow-other-keys)
  (scan-stream source #'read-line))

(defmethod tap ((type (eql :objects))(source stream)
                &key &allow-other-keys)
  (scan-stream source #'read))


#|
(with-input-from-string (in "Hello there, world")
  (tap :characters in))
(with-input-from-string (in "Hello there, world")
  (tap :words in))
(with-input-from-string (in "0 1.0 \"Two\" :three (4 5)")
  (tap :objects in))

(with-open-file (in "/Users/mikel/.emacs" :element-type '(unsigned-byte 8))
  (tap :bytes in))
(with-open-file (in "/Users/mikel/.emacs")
  (tap :characters in))
(with-open-file (in "/Users/mikel/.emacs")
  (tap :words in))
(with-open-file (in "/Users/mikel/.emacs")
  (tap :lines in))
|#
