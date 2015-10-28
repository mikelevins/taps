;;;; package.lisp

(defpackage #:taps
  (:use #:cl #:series)
  (:export
   #:+line-break-characters+
   #:+whitespace-characters+
   #:drop
   #:filter
   #:take
   #:take-by
   #:take-m-by-n
   #:take-until
   #:tap
   #:tap-fn
   #:tap-integers
   #:tap-random-integers
   ))

